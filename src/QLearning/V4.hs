{-|
Module      : QLearning.V4
Description : Final path-aware tabular Q-learning design for CerviQ.

Version 4 extends the compact action-quality representation introduced in V3
with path-aware food navigation and explicit loop detection. It also performs
reusable graph searches over the current map so state encoding, safety
analysis, fallback behaviour, and debugging can share the same underlying
spatial information.

The generic Q-learning algorithm remains in "QLearning.Core". This module
defines the V4 state representation, environment analysis, reward shaping,
learned policy, and the additional safety/topology fallback used by the final
agent.
-}

module QLearning.V4
    ( ActionQuality(..)
    , FoodPathDirection(..)
    , LoopStatus(..)
    , RLState(..)

    , encodeState
    , rewardForStep

    , foodPathInfo
    , loopStatusForWorm

    , v4Spec
    , QTable

    , saveQTable
    , loadQTable
    , qLearningAgent
    , qLearningAgentWithFallback

    , describeState
    , v4DebugProvider
    ) where

import Agent
import Collision
import Maps
import Movement
import Types

import Data.List ()
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified QLearning.Core as Core
import qualified QLearning.Debug as Debug


-- -----------------------------------------------------------------------------
-- RL state representation
-- -----------------------------------------------------------------------------

-- | Overall quality of one candidate action.
--
-- The categories retain the V3 interpretation: they combine immediate safety,
-- opponent threats, future manoeuvrability, and reachable space into one
-- compact value for each of the three possible actions.
data ActionQuality
    = Fatal
    | Contested
    | DeadEnd
    | Threatened
    | ForcedCritical
    | ForcedRestricted
    | ForcedLimited
    | ForcedOpen
    | Critical
    | Restricted
    | Limited
    | Open
    deriving (Show, Read, Eq, Ord)


-- | Direction of the first action on the shortest currently reachable path to
-- food.
--
-- Unlike V2 and V3, this is based on actual navigable paths rather than only
-- the geometric position of the nearest food.
data FoodPathDirection
    = FoodPathLeft
    | FoodPathStraight
    | FoodPathRight
    | FoodPathUnavailable
    deriving (Show, Read, Eq, Ord)


-- | Whether recent head history suggests that the worm is repeating a route.
data LoopStatus
    = NotRepeating
    | RepeatingLoop
    deriving (Show, Read, Eq, Ord)


-- | Version-4 state used as the key of the tabular Q-table.
--
-- V4 retains the three V3 action qualities, replaces geometric relative food
-- direction with the first action of a reachable shortest path, and adds one
-- bit of recent movement-history information for loop detection.
data RLState = RLState
    { qualityLeft :: ActionQuality
    , qualityStraight :: ActionQuality
    , qualityRight :: ActionQuality
    , foodPathDirection :: FoodPathDirection
    , loopStatus :: LoopStatus
    }
    deriving (Show, Read, Eq, Ord)


-- -----------------------------------------------------------------------------
-- Geometry helpers
-- -----------------------------------------------------------------------------

-- | Returns the four orthogonal neighbours of a map position.
neighbours :: Position -> [Position]
neighbours (x, y) =
    [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]


-- | Replaces one worm in a complete worm list with a hypothetical version.
replaceWorm :: Worm -> [Worm] -> [Worm]
replaceWorm updatedWorm worms =
    [ if wormId worm == wormId updatedWorm then updatedWorm else worm
    | worm <- worms
    ]


-- | Simulates the controlled worm after one candidate action.
--
-- Growth is included if the candidate destination currently contains food.
-- Other worms remain at their current positions.
wormAfterAction :: GameState -> Worm -> Action -> Worm
wormAfterAction state worm action =
    moveWormAfterAction grows action worm
  where
    nextHead = headAfterAction worm action
    grows = isFood (gameMap state) nextHead


-- -----------------------------------------------------------------------------
-- Reusable map-search context
-- -----------------------------------------------------------------------------

-- | Static map information shared by graph searches over one game map.
--
-- Walls and poison are collected once into 'searchStaticBlocked', while food is
-- stored separately for path queries.
data MapSearchBase = MapSearchBase
    { searchMapWidth :: Int
    , searchMapHeight :: Int
    , searchStaticBlocked :: Set.Set Position
    , searchFoodPositions :: Set.Set Position
    }


-- | Complete graph-search context for one worm in one game state.
--
-- Static map information is reused through 'searchBase', while occupied worm
-- cells form the dynamic part of the context.
data SearchContext = SearchContext
    { searchBase :: MapSearchBase
    , searchOccupied :: Set.Set Position
    }


-- | Builds reusable static search information in one traversal of the map's
-- explicitly stored tiles.
makeMapSearchBase :: GameMap -> MapSearchBase
makeMapSearchBase currentMap =
    MapSearchBase
        { searchMapWidth = mapWidth currentMap
        , searchMapHeight = mapHeight currentMap
        , searchStaticBlocked = blockedPositions
        , searchFoodPositions = foods
        }
  where
    (blockedPositions, foods) =
        Map.foldrWithKey collectTile (Set.empty, Set.empty) (mapTiles currentMap)

    collectTile position tile (blocked, foodSet) =
        case tile of
            Wall -> (Set.insert position blocked, foodSet)
            Poison -> (Set.insert position blocked, foodSet)
            Food -> (blocked, Set.insert position foodSet)
            Empty -> (blocked, foodSet)


-- | Builds the dynamic part of a graph-search context.
--
-- The controlled worm's current head is removed from occupied positions so it
-- can serve as the starting position of a search. Its remaining body and all
-- other living worms remain obstacles.
makeSearchContext :: MapSearchBase -> GameState -> Worm -> SearchContext
makeSearchContext base state worm =
    SearchContext
        { searchBase = base
        , searchOccupied = Set.delete (wormHead worm) occupied
        }
  where
    aliveWorms = filter wormAlive (gameWorms state)
    occupied = Set.fromList (occupiedPositions aliveWorms)


-- | Returns whether a position lies inside the map represented by a search base.
isInsideSearchMap :: MapSearchBase -> Position -> Bool
isInsideSearchMap base (x, y) =
    x >= 0
        && y >= 0
        && x < searchMapWidth base
        && y < searchMapHeight base


-- | Returns whether a position blocks an ordinary graph search.
isSearchBlocked :: SearchContext -> Position -> Bool
isSearchBlocked context position =
    not (isInsideSearchMap base position)
        || position `Set.member` searchStaticBlocked base
        || position `Set.member` searchOccupied context
  where
    base = searchBase context


-- | Returns whether a position blocks a search when additional temporary
-- blockers are considered.
--
-- V4 uses the additional set primarily for potential opponent head positions.
isSearchBlockedWith :: SearchContext -> Set.Set Position -> Position -> Bool
isSearchBlockedWith context extraBlocked position =
    isSearchBlocked context position
        || position `Set.member` extraBlocked


-- -----------------------------------------------------------------------------
-- Reusable breadth-first search
-- -----------------------------------------------------------------------------

-- | Returns every position reachable from the given start while respecting
-- ordinary and temporary blockers.
--
-- Breadth-first search marks positions visited when they are enqueued rather
-- than when they are removed from the queue. This prevents the same position
-- from being inserted into the queue repeatedly.
reachablePositions :: SearchContext -> Set.Set Position -> Position -> Set.Set Position
reachablePositions context extraBlocked start
    | isSearchBlockedWith context extraBlocked start = Set.empty
    | otherwise = search (Seq.singleton start) (Set.singleton start)
  where
    search queue visited =
        case Seq.viewl queue of
            Seq.EmptyL ->
                visited

            position Seq.:< remainingQueue ->
                search updatedQueue updatedVisited
              where
                nextPositions =
                    [ nextPosition
                    | nextPosition <- neighbours position
                    , nextPosition `Set.notMember` visited
                    , not (isSearchBlockedWith context extraBlocked nextPosition)
                    ]

                updatedVisited =
                    foldl' (flip Set.insert) visited nextPositions

                updatedQueue =
                    foldl' (Seq.|>) remainingQueue nextPositions


-- -----------------------------------------------------------------------------
-- Path-aware food navigation
-- -----------------------------------------------------------------------------

-- | Returns the relative action whose next head position is the supplied
-- neighbouring cell.
actionForFirstStep :: Worm -> Position -> Maybe Action
actionForFirstStep worm position =
    case
        [ action
        | action <- allActions
        , headAfterAction worm action == position
        ]
    of
        action : _ -> Just action
        [] -> Nothing


-- | Finds the first action and length of the shortest currently reachable path
-- to any food item using a prepared graph-search context.
--
-- Each BFS queue item stores the current position, the first position taken
-- from the worm's head, and the path length. Once food is reached, that first
-- position is converted back into the corresponding relative 'Action'. The
-- initial queue contains only the three legal relative-action destinations, so
-- a path can never begin with an impossible 180-degree reversal.
foodPathInfoWithContext :: SearchContext -> Worm -> Maybe (Action, Int)
foodPathInfoWithContext context worm =
    search initialQueue initialVisited
  where
    startPosition = wormHead worm

    initialSteps =
        [ position
        | action <- allActions
        , let position = headAfterAction worm action
        , not (isSearchBlocked context position)
        ]

    initialQueue =
        Seq.fromList
            [ (position, position, 1)
            | position <- initialSteps
            ]

    initialVisited =
        foldl' (flip Set.insert) (Set.singleton startPosition) initialSteps

    foods =
        searchFoodPositions (searchBase context)

    search queue visited =
        case Seq.viewl queue of
            Seq.EmptyL ->
                Nothing

            (position, firstStep, pathLength) Seq.:< remainingQueue
                | position `Set.member` foods ->
                    case actionForFirstStep worm firstStep of
                        Just action ->
                            Just (action, pathLength)

                        Nothing ->
                            continueSearch position firstStep pathLength remainingQueue visited

                | otherwise ->
                    continueSearch position firstStep pathLength remainingQueue visited

    continueSearch position firstStep pathLength remainingQueue visited =
        search updatedQueue updatedVisited
      where
        nextPositions =
            [ nextPosition
            | nextPosition <- neighbours position
            , nextPosition `Set.notMember` visited
            , not (isSearchBlocked context nextPosition)
            ]

        updatedVisited =
            foldl' (flip Set.insert) visited nextPositions

        updatedQueue =
            foldl'
                (\queue nextPosition -> queue Seq.|> (nextPosition, firstStep, pathLength + 1))
                remainingQueue
                nextPositions


-- | Finds the first action and distance of the shortest currently reachable path
-- to any food item.
--
-- Walls, poison, and currently occupied worm cells are treated as obstacles.
-- The search considers the current board only; it does not predict future worm
-- movement.
foodPathInfo :: GameState -> Worm -> Maybe (Action, Int)
foodPathInfo state worm =
    foodPathInfoWithContext context worm
  where
    base = makeMapSearchBase (gameMap state)
    context = makeSearchContext base state worm


-- | Converts calculated shortest-path information into the V4 food-path state.
foodPathDirectionFromInfo :: Maybe (Action, Int) -> FoodPathDirection
foodPathDirectionFromInfo (Just (TurnLeft, _)) = FoodPathLeft
foodPathDirectionFromInfo (Just (GoStraight, _)) = FoodPathStraight
foodPathDirectionFromInfo (Just (TurnRight, _)) = FoodPathRight
foodPathDirectionFromInfo Nothing = FoodPathUnavailable


-- | Returns the shortest currently reachable path distance to food.
foodPathDistance :: GameState -> Worm -> Maybe Int
foodPathDistance state worm =
    fmap snd (foodPathInfo state worm)


-- -----------------------------------------------------------------------------
-- Opponent threat prediction
-- -----------------------------------------------------------------------------

-- | Returns all cells that living opponents could safely move their heads into
-- during the next simultaneous game tick.
--
-- As in V3, this is a conservative one-step threat approximation rather than
-- full adversarial search.
possibleEnemyNextHeads :: GameState -> Worm -> Set.Set Position
possibleEnemyNextHeads state controlled =
    Set.fromList
        [ headAfterAction enemy action
        | enemy <- gameWorms state
        , wormAlive enemy
        , wormId enemy /= wormId controlled
        , action <- safeActions state enemy
        ]


-- -----------------------------------------------------------------------------
-- Head history and loop detection
-- -----------------------------------------------------------------------------

-- | Maximum number of recent head positions considered by V4 loop analysis.
loopHistoryWindow :: Int
loopHistoryWindow = 256


-- | Returns recent head positions of one worm from newest to oldest.
recentHeadPositions :: GameState -> Worm -> [Position]
recentHeadPositions state worm =
    take loopHistoryWindow $
        Map.findWithDefault [] (wormId worm) (gameHeadHistory state)


-- | Converts newest-first head history into chronological directed movement
-- edges.
--
-- For @[current, previous, older]@ the result is
-- @[(previous,current), (older,previous)]@, so the first pair is the most recent
-- movement edge.
recentMovementEdges :: [Position] -> [(Position, Position)]
recentMovementEdges history =
    zip (drop 1 history) history


-- | Counts how often a position appears in already extracted recent history.
recentVisitCountIn :: [Position] -> Position -> Int
recentVisitCountIn history position =
    length (filter (== position) history)


-- | Counts how often one directed movement edge appears in recent history.
recentEdgeVisitCountIn :: [Position] -> Position -> Position -> Int
recentEdgeVisitCountIn history fromPosition toPosition =
    length $
        filter (== (fromPosition, toPosition)) (recentMovementEdges history)


-- | Determines whether the most recent movement edge occurred earlier in the
-- stored history.
--
-- Repeating a directed edge is a stronger indication of following the same
-- route than merely revisiting a position and can detect loops substantially
-- larger than a short local cycle.
loopStatusFromHistory :: [Position] -> LoopStatus
loopStatusFromHistory history =
    case recentMovementEdges history of
        [] ->
            NotRepeating

        currentEdge : olderEdges
            | currentEdge `elem` olderEdges -> RepeatingLoop
            | otherwise -> NotRepeating


-- | Determines whether one worm currently appears to be repeating a route.
loopStatusForWorm :: GameState -> Worm -> LoopStatus
loopStatusForWorm state worm =
    loopStatusFromHistory (recentHeadPositions state worm)


-- -----------------------------------------------------------------------------
-- Tail connectivity
-- -----------------------------------------------------------------------------

-- | Determines whether the worm's tail remains connected to the threat-aware
-- reachable component.
--
-- The tail cell itself is occupied by the worm, so the search cannot enter it
-- directly. Reaching at least one safe neighbour of the tail is therefore used
-- as a practical indication that the worm has not separated its head from its
-- own trailing route.
--
-- A length-one worm is trivially tail-connected.
tailReachableFrom :: Worm -> Set.Set Position -> Set.Set Position -> Bool
tailReachableFrom worm threats safeReachable =
    case reverse (wormBody worm) of
        [] ->
            False

        [_] ->
            True

        tailPosition : _ ->
            tailPosition `Set.notMember` threats
                && any (`Set.member` safeReachable) (neighbours tailPosition)


-- -----------------------------------------------------------------------------
-- Action analysis
-- -----------------------------------------------------------------------------

-- | Detailed internal analysis of one candidate action.
--
-- Only 'analysisQuality' is stored directly in the V4 reinforcement-learning
-- state. The remaining values are reused by the fallback policy and debugger,
-- allowing those components to inspect the richer spatial analysis without
-- increasing the tabular state space.
data ActionAnalysis = ActionAnalysis
    { analysisQuality :: ActionQuality
    , analysisRawArea :: Int
    , analysisThreatAwareArea :: Int
    , analysisRawNextMoves :: Int
    , analysisRobustNextMoves :: Int
    , analysisCanReachTail :: Bool
    , analysisFoodPathDistance :: Maybe Int
    , analysisRecentEdgeVisits :: Int
    , analysisRecentVisits :: Int
    }


-- | Information that can be computed once and reused while analysing all three
-- candidate actions from the same game state.
data ActionAnalysisContext = ActionAnalysisContext
    { analysisCurrentSafeActions :: [Action]
    , analysisCurrentEnemyThreats :: Set.Set Position
    , analysisSearchBase :: MapSearchBase
    , analysisRecentHistory :: [Position]
    }


-- | Complete reusable V4 analysis of one game state.
--
-- Keeping the three 'ActionAnalysis' values together with the final encoded
-- state avoids repeating the expensive searches when the same decision is
-- subsequently used by state encoding, fallback selection, or debugging.
data StateAnalysis = StateAnalysis
    { stateAnalysisRlState :: RLState
    , stateAnalysisLeft :: ActionAnalysis
    , stateAnalysisStraight :: ActionAnalysis
    , stateAnalysisRight :: ActionAnalysis
    }


-- | Creates information shared by analysis of all three candidate actions.
makeActionAnalysisContext :: GameState -> Worm -> ActionAnalysisContext
makeActionAnalysisContext state worm =
    ActionAnalysisContext
        { analysisCurrentSafeActions = safeActions state worm
        , analysisCurrentEnemyThreats = possibleEnemyNextHeads state worm
        , analysisSearchBase = makeMapSearchBase (gameMap state)
        , analysisRecentHistory = recentHeadPositions state worm
        }


-- | Converts threat-aware reachable area into a V4 action-quality category.
--
-- Thresholds scale with worm length. The @Forced@ variants indicate that only
-- one robust continuation remains after the candidate move.
qualityFromArea :: Bool -> Worm -> Int -> ActionQuality
qualityFromArea forced worm area
    | 2 * area <= wormLength =
        if forced then ForcedCritical else Critical

    | area <= wormLength =
        if forced then ForcedRestricted else Restricted

    | area <= 2 * wormLength =
        if forced then ForcedLimited else Limited

    | otherwise =
        if forced then ForcedOpen else Open
  where
    wormLength = length (wormBody worm)


-- | Analyses one candidate action using information shared across all three
-- possible actions.
--
-- V4 retains V3's high-level classification order:
--
-- 1. immediately unsafe -> 'Fatal';
-- 2. destination contested by an opponent this turn -> 'Contested';
-- 3. no ordinary continuation -> 'DeadEnd';
-- 4. no robust continuation after recalculating opponent threats -> 'Threatened';
-- 5. otherwise classify threat-aware reachable space.
--
-- In addition, the analysis records tail connectivity, path distance to food,
-- and recent movement repetition for use by the V4 fallback policy.
analyzeActionWithContext
    :: ActionAnalysisContext
    -> GameState
    -> Worm
    -> Action
    -> ActionAnalysis
analyzeActionWithContext context state worm action
    | action `notElem` currentSafeActions =
        analysisWithoutFuture Fatal

    | nextHead `Set.member` currentEnemyThreats =
        analysisWithoutFuture Contested

    | rawMoveCount == 0 =
        analysisWithQuality DeadEnd

    | robustMoveCount == 0 =
        analysisWithQuality Threatened

    | otherwise =
        analysisWithQuality $
            qualityFromArea
                (robustMoveCount == 1)
                movedWorm
                safeArea

  where
    currentSafeActions =
        analysisCurrentSafeActions context

    currentEnemyThreats =
        analysisCurrentEnemyThreats context

    nextHead =
        headAfterAction worm action

    movedWorm =
        wormAfterAction state worm action

    hypotheticalState =
        state {gameWorms = replaceWorm movedWorm (gameWorms state)}

    -- Unlike V3, V4 recalculates opponent threats in the hypothetical state
    -- after the controlled worm has performed the candidate action.
    futureEnemyThreats =
        possibleEnemyNextHeads hypotheticalState movedWorm

    futureSafeActions =
        safeActions hypotheticalState movedWorm

    robustFutureActions =
        [ futureAction
        | futureAction <- futureSafeActions
        , headAfterAction movedWorm futureAction `Set.notMember` futureEnemyThreats
        ]

    rawMoveCount =
        length futureSafeActions

    robustMoveCount =
        length robustFutureActions

    searchContext =
        makeSearchContext
            (analysisSearchBase context)
            hypotheticalState
            movedWorm

    rawReachable =
        reachablePositions
            searchContext
            Set.empty
            (wormHead movedWorm)

    safeReachable =
        reachablePositions
            searchContext
            futureEnemyThreats
            (wormHead movedWorm)

    rawArea =
        Set.size rawReachable

    safeArea =
        Set.size safeReachable

    tailReachable =
        tailReachableFrom
            movedWorm
            futureEnemyThreats
            safeReachable

    pathDistance =
        fmap snd (foodPathInfoWithContext searchContext movedWorm)

    history =
        analysisRecentHistory context

    edgeVisits =
        recentEdgeVisitCountIn
            history
            (wormHead worm)
            nextHead

    visits =
        recentVisitCountIn
            history
            nextHead

    analysisWithoutFuture quality =
        ActionAnalysis
            { analysisQuality = quality
            , analysisRawArea = 0
            , analysisThreatAwareArea = 0
            , analysisRawNextMoves = 0
            , analysisRobustNextMoves = 0
            , analysisCanReachTail = False
            , analysisFoodPathDistance = Nothing
            , analysisRecentEdgeVisits = edgeVisits
            , analysisRecentVisits = visits
            }

    analysisWithQuality quality =
        ActionAnalysis
            { analysisQuality = quality
            , analysisRawArea = rawArea
            , analysisThreatAwareArea = safeArea
            , analysisRawNextMoves = rawMoveCount
            , analysisRobustNextMoves = robustMoveCount
            , analysisCanReachTail = tailReachable
            , analysisFoodPathDistance = pathDistance
            , analysisRecentEdgeVisits = edgeVisits
            , analysisRecentVisits = visits
            }


-- -----------------------------------------------------------------------------
-- Complete state analysis
-- -----------------------------------------------------------------------------

-- | Performs the complete reusable V4 analysis of one game state.
--
-- Shared information such as the static map search base, current opponent
-- threats, and recent head history is constructed once. The three candidate
-- actions are then analysed from that common context, after which the compact
-- reinforcement-learning state is assembled from their qualities, the current
-- shortest path to food, and loop status.
analyzeState :: GameState -> Worm -> StateAnalysis
analyzeState state worm =
    StateAnalysis
        { stateAnalysisRlState = rlState
        , stateAnalysisLeft = leftAnalysis
        , stateAnalysisStraight = straightAnalysis
        , stateAnalysisRight = rightAnalysis
        }
  where
    context =
        makeActionAnalysisContext state worm

    leftAnalysis =
        analyzeActionWithContext context state worm TurnLeft

    straightAnalysis =
        analyzeActionWithContext context state worm GoStraight

    rightAnalysis =
        analyzeActionWithContext context state worm TurnRight

    currentSearchContext =
        makeSearchContext
            (analysisSearchBase context)
            state
            worm

    currentFoodPath =
        foodPathInfoWithContext currentSearchContext worm

    currentLoopStatus =
        loopStatusFromHistory (analysisRecentHistory context)

    rlState =
        RLState
            { qualityLeft = analysisQuality leftAnalysis
            , qualityStraight = analysisQuality straightAnalysis
            , qualityRight = analysisQuality rightAnalysis
            , foodPathDirection = foodPathDirectionFromInfo currentFoodPath
            , loopStatus = currentLoopStatus
            }


-- | Returns the three candidate-action analyses in relative-action order.
stateActionAnalyses :: StateAnalysis -> [(Action, ActionAnalysis)]
stateActionAnalyses analysis =
    [ (TurnLeft, stateAnalysisLeft analysis)
    , (GoStraight, stateAnalysisStraight analysis)
    , (TurnRight, stateAnalysisRight analysis)
    ]


-- -----------------------------------------------------------------------------
-- State encoding
-- -----------------------------------------------------------------------------

-- | Encodes the current game situation into the compact V4 tabular state.
encodeState :: GameState -> Worm -> RLState
encodeState state worm =
    stateAnalysisRlState (analyzeState state worm)


-- -----------------------------------------------------------------------------
-- Reward
-- -----------------------------------------------------------------------------

-- | Computes shaping reward from changes in the actual reachable path distance
-- to food.
--
-- Unlike V1-V3, V4 does not use Manhattan distance. Moving to a state with a
-- shorter navigable path is rewarded, while increasing that path length is
-- penalized. Gaining or losing food reachability is treated analogously.
--
-- Distance shaping is suppressed on transitions where food was eaten because
-- food consumption already has its own larger reward.
foodDistanceReward :: GameState -> Worm -> GameState -> Worm -> Double
foodDistanceReward beforeState beforeWorm afterState afterWorm
    | Core.wormFoodDelta beforeWorm afterWorm > 0 = 0
    | otherwise =
        case (foodPathDistance beforeState beforeWorm, foodPathDistance afterState afterWorm) of
            (Just beforeDistance, Just afterDistance)
                | afterDistance < beforeDistance -> 2
                | afterDistance > beforeDistance -> -2
                | otherwise -> 0

            (Just _, Nothing) -> -2
            (Nothing, Just _) -> 2
            _ -> 0


-- | Computes the V4 reward for one transition.
--
-- The numerical reward weights remain equal to V3:
--
-- * +50 for each food item eaten,
-- * +10 for each kill,
-- * -100 for dying,
-- * +2/-2 for improving/worsening food reachability,
-- * -1 for every simulated tick.
--
-- The important V4 change is that food progress uses shortest navigable path
-- distance rather than Manhattan distance.
rewardForStep :: GameState -> Worm -> GameState -> Worm -> Double
rewardForStep beforeState beforeWorm afterState afterWorm =
    foodReward + killReward + deathPenalty + distanceReward + timePenalty
  where
    foodReward =
        50 * fromIntegral (Core.wormFoodDelta beforeWorm afterWorm)

    killReward =
        10 * fromIntegral (Core.wormKillDelta beforeWorm afterWorm)

    deathPenalty =
        if Core.wormDied beforeWorm afterWorm then -100 else 0

    distanceReward =
        foodDistanceReward beforeState beforeWorm afterState afterWorm

    timePenalty =
        -1


-- -----------------------------------------------------------------------------
-- V4 Q-learning specification
-- -----------------------------------------------------------------------------

-- | Complete version-specific specification passed to the shared Q-learning core.
v4Spec :: Core.QLearningSpec RLState
v4Spec =
    Core.QLearningSpec
        { Core.qlEncodeState = encodeState
        , Core.qlRewardForStep = rewardForStep
        }


-- | Q-table whose keys use the V4 state representation.
type QTable = Core.QTable RLState


-- | Saves a V4 Q-table.
saveQTable :: FilePath -> QTable -> IO ()
saveQTable =
    Core.saveQTable


-- | Loads a V4 Q-table.
loadQTable :: FilePath -> IO QTable
loadQTable =
    Core.loadQTable


-- | Creates the pure greedy V4 learned agent.
--
-- This variant follows only learned Q-values. The final gameplay agent
-- 'qLearningAgentWithFallback' additionally applies V4's deterministic safety
-- and topology policy.
qLearningAgent :: QTable -> Agent
qLearningAgent =
    Core.qLearningAgent v4Spec


-- -----------------------------------------------------------------------------
-- V4 safety and topology policy
-- -----------------------------------------------------------------------------

-- | Returns whether an action quality represents immediate or effectively
-- unavoidable danger.
--
-- These actions are removed whenever at least one alternative exists.
isHardUnsafeQuality :: ActionQuality -> Bool
isHardUnsafeQuality Fatal = True
isHardUnsafeQuality Contested = True
isHardUnsafeQuality DeadEnd = True
isHardUnsafeQuality _ = False


-- | Returns whether an action leaves critically little reachable space.
isCriticalQuality :: ActionQuality -> Bool
isCriticalQuality ForcedCritical = True
isCriticalQuality Critical = True
isCriticalQuality _ = False


-- | Removes hard-unsafe candidates whenever at least one safer candidate exists.
preferNonHardUnsafe :: [(Action, ActionAnalysis)] -> [(Action, ActionAnalysis)]
preferNonHardUnsafe analyses
    | null saferActions = analyses
    | otherwise = saferActions
  where
    saferActions =
        filter
            (not . isHardUnsafeQuality . analysisQuality . snd)
            analyses


-- | Removes critically space-restricted candidates whenever at least one
-- non-critical alternative exists.
preferNonCritical :: [(Action, ActionAnalysis)] -> [(Action, ActionAnalysis)]
preferNonCritical analyses
    | null nonCriticalActions = analyses
    | otherwise = nonCriticalActions
  where
    nonCriticalActions =
        filter
            (not . isCriticalQuality . analysisQuality . snd)
            analyses


-- | Keeps tail-connected candidates whenever at least one such action exists.
--
-- Tail connectivity is treated as a topology preference rather than an
-- absolute requirement: if every action disconnects from the tail region, no
-- candidate is removed at this stage.
preferTailConnected :: [(Action, ActionAnalysis)] -> [(Action, ActionAnalysis)]
preferTailConnected analyses
    | null tailConnected = analyses
    | otherwise = tailConnected
  where
    tailConnected =
        filter
            (analysisCanReachTail . snd)
            analyses


-- | Keeps only candidates with the minimum value of the selected diagnostic.
--
-- This helper is used by loop breaking to progressively narrow candidates
-- while preserving ties.
preferMinimumBy
    :: (ActionAnalysis -> Int)
    -> [(Action, ActionAnalysis)]
    -> [(Action, ActionAnalysis)]
preferMinimumBy _ [] =
    []

preferMinimumBy selector analyses =
    filter
        ((== minimumValue) . selector . snd)
        analyses
  where
    minimumValue =
        minimum (map (selector . snd) analyses)


-- | Prefers actions that leave a detected repeated route.
--
-- Exact directed-edge repetition is considered first. If multiple actions are
-- tied, recent destination visits are used as a secondary criterion.
--
-- When no loop is detected, candidates are returned unchanged.
preferLoopBreaking
    :: LoopStatus
    -> [(Action, ActionAnalysis)]
    -> [(Action, ActionAnalysis)]
preferLoopBreaking NotRepeating analyses =
    analyses

preferLoopBreaking RepeatingLoop analyses =
    preferMinimumBy
        analysisRecentVisits
        (preferMinimumBy analysisRecentEdgeVisits analyses)


-- -----------------------------------------------------------------------------
-- V4 fallback scoring
-- -----------------------------------------------------------------------------

-- | Assigns an ordering score to action-quality categories.
--
-- Higher values represent more desirable outcomes. Non-forced categories are
-- preferred to their forced counterparts because they retain more than one
-- robust continuation.
qualityScore :: ActionQuality -> Int
qualityScore Fatal = 0
qualityScore Contested = 1
qualityScore DeadEnd = 2
qualityScore Threatened = 3
qualityScore ForcedCritical = 4
qualityScore Critical = 5
qualityScore ForcedRestricted = 6
qualityScore Restricted = 7
qualityScore ForcedLimited = 8
qualityScore Limited = 9
qualityScore ForcedOpen = 10
qualityScore Open = 11


-- | Converts optional food-path distance into a score where shorter reachable
-- paths are preferred and unavailable food paths rank below reachable ones.
foodDistanceScore :: Maybe Int -> Int
foodDistanceScore Nothing =
    -1000000

foodDistanceScore (Just pathLength) =
    negate pathLength


-- | Computes the lexicographic fallback score of one analysed action.
--
-- Earlier tuple components have greater priority:
--
-- 1. action quality,
-- 2. number of robust next moves,
-- 3. threat-aware reachable area,
-- 4. shortest path distance to food,
-- 5. recent use of the same directed edge,
-- 6. recent visits to the destination.
fallbackScore :: ActionAnalysis -> (Int, Int, Int, Int, Int, Int)
fallbackScore analysis =
    ( qualityScore (analysisQuality analysis)
    , analysisRobustNextMoves analysis
    , analysisThreatAwareArea analysis
    , foodDistanceScore (analysisFoodPathDistance analysis)
    , negate (analysisRecentEdgeVisits analysis)
    , negate (analysisRecentVisits analysis)
    )


-- | Chooses the best remaining action according to detailed V4 diagnostics.
--
-- This is used only when none of the policy candidates has an explicitly
-- learned Q-value.
fallbackAction :: [(Action, ActionAnalysis)] -> IO Action
fallbackAction analyses =
    randomChoice bestActions'
  where
    bestScore =
        maximum (map (fallbackScore . snd) analyses)

    bestActions' =
        [ action
        | (action, analysis) <- analyses
        , fallbackScore analysis == bestScore
        ]


-- -----------------------------------------------------------------------------
-- Final V4 agent
-- -----------------------------------------------------------------------------

-- | Creates the final V4 agent combining learned Q-values with deterministic
-- safety, topology, and loop-breaking preferences.
--
-- Candidate actions are progressively filtered:
--
-- 1. avoid hard-unsafe moves when possible;
-- 2. avoid critically small regions when possible;
-- 3. prefer actions that retain tail connectivity;
-- 4. when looping, prefer less repeated edges and destinations.
--
-- Among the surviving candidates, explicitly learned Q-values remain the
-- primary decision mechanism. If none of those candidates has a learned value,
-- 'fallbackAction' ranks them using the richer deterministic V4 diagnostics.
--
-- 'Core.hasQValue' is required to distinguish genuinely learned values from
-- unseen state-action pairs, which numerically default to Q-value zero.
qLearningAgentWithFallback :: QTable -> Agent
qLearningAgentWithFallback table gameState worm =
    case learnedCandidates of
        [] ->
            fallbackAction policyCandidates

        _ ->
            randomChoice bestLearnedActions

  where
    completeAnalysis =
        analyzeState gameState worm

    state =
        stateAnalysisRlState completeAnalysis

    analyses =
        stateActionAnalyses completeAnalysis

    nonHardUnsafeCandidates =
        preferNonHardUnsafe analyses

    nonCriticalCandidates =
        preferNonCritical nonHardUnsafeCandidates

    tailCandidates =
        preferTailConnected nonCriticalCandidates

    policyCandidates =
        preferLoopBreaking
            (loopStatus state)
            tailCandidates

    learnedCandidates =
        [ (action, analysis)
        | (action, analysis) <- policyCandidates
        , Core.hasQValue table state action
        ]

    bestLearnedValue =
        maximum
            [ Core.qValue table state action
            | (action, _) <- learnedCandidates
            ]

    bestLearnedActions =
        [ action
        | (action, _) <- learnedCandidates
        , Core.qValue table state action == bestLearnedValue
        ]


-- -----------------------------------------------------------------------------
-- V4 debugging
-- -----------------------------------------------------------------------------

-- | Converts the compact V4 state into human-readable diagnostic values.
describeState :: RLState -> [(String, String)]
describeState state =
    [ ("Quality L/S/R", show (qualityLeft state) ++ " / " ++ show (qualityStraight state) ++ " / " ++ show (qualityRight state))
    , ("Food path", show (foodPathDirection state))
    , ("Loop status", show (loopStatus state))
    ]


-- | Formats an optional food-path distance for the GUI debugger.
showPathDistance :: Maybe Int -> String
showPathDistance Nothing =
    "-"

showPathDistance (Just pathLength) =
    show pathLength


-- | Creates detailed version-independent GUI diagnostics for a V4 learned agent.
--
-- The debugger exposes both the compact state used by the Q-table and the
-- richer deterministic measurements used by V4 analysis and fallback. The
-- latter do not enlarge the learned tabular state.
v4DebugProvider :: QTable -> Debug.AgentDebugProvider
v4DebugProvider table gameState worm =
    Debug.AgentDebugInfo
        { Debug.debugStateLines =
            describeState state
                ++ [ ("Raw area L/S/R", showTriple analysisRawArea)
                   , ("Safe area L/S/R", showTriple analysisThreatAwareArea)
                   , ("Raw moves L/S/R", showTriple analysisRawNextMoves)
                   , ("Robust moves L/S/R", showTriple analysisRobustNextMoves)
                   , ("Tail reachable L/S/R", showTriple analysisCanReachTail)
                   , ("Food dist L/S/R", showDistanceTriple)
                   , ("Edge visits L/S/R", showTriple analysisRecentEdgeVisits)
                   , ("Recent visits L/S/R", showTriple analysisRecentVisits)
                   , ("Known Q actions", show knownActionCount ++ " / 3")
                   , ("Length", show (length (wormBody worm)))
                   ]
        , Debug.debugQValues =
            Core.qValuesForState table state
        , Debug.debugBestActions =
            Core.bestQActions table state
        }
  where
    completeAnalysis =
        analyzeState gameState worm

    state =
        stateAnalysisRlState completeAnalysis

    leftAnalysis =
        stateAnalysisLeft completeAnalysis

    straightAnalysis =
        stateAnalysisStraight completeAnalysis

    rightAnalysis =
        stateAnalysisRight completeAnalysis

    showTriple selector =
        show (selector leftAnalysis)
            ++ " / " ++ show (selector straightAnalysis)
            ++ " / " ++ show (selector rightAnalysis)

    showDistanceTriple =
        showPathDistance (analysisFoodPathDistance leftAnalysis)
            ++ " / " ++ showPathDistance (analysisFoodPathDistance straightAnalysis)
            ++ " / " ++ showPathDistance (analysisFoodPathDistance rightAnalysis)

    knownActionCount =
        length
            [ action
            | action <- allActions
            , Core.hasQValue table state action
            ]
