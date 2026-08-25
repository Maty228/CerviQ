module QLearning.V4
    ( ActionQuality(..)
    , FoodPathDirection(..)
    , LoopStatus(..)
    , RLState(..)

    , encodeState
    , rewardForStep

    , foodPathInfo
    , loopStatusForWorm
    , canReachTailAfterAction

    , v4Spec
    , QTable

    , saveQTable
    , loadQTable
    , qLearningAgent
    , qLearningAgentWithFallback

    , theoreticalStateCount
    , theoreticalEntryCount

    , describeState
    , v4DebugProvider
    ) where

import Agent
import Collision
import Maps
import Movement
import Types

import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.List (foldl')
import qualified QLearning.Core as Core
import qualified QLearning.Debug as Debug


-- -----------------------------------------------------------------------------
-- RL state representation
-- -----------------------------------------------------------------------------

-- | Overall quality of one candidate action.
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


-- | Direction of the first step on the shortest currently reachable path to food.
data FoodPathDirection
    = FoodPathLeft
    | FoodPathStraight
    | FoodPathRight
    | FoodPathUnavailable
    deriving (Show, Read, Eq, Ord)


-- | Whether the worm appears to be repeatedly visiting the same route.
data LoopStatus
    = NotRepeating
    | RepeatingLoop
    deriving (Show, Read, Eq, Ord)


-- | Version-4 RL state.
--
-- Version 4 keeps the compact action-quality representation of V3, replaces
-- geometric food direction with path-aware food navigation and explicitly
-- distinguishes repeating movement patterns.
data RLState = RLState
    {
        qualityLeft :: ActionQuality,
        qualityStraight :: ActionQuality,
        qualityRight :: ActionQuality,
        foodPathDirection :: FoodPathDirection,
        loopStatus :: LoopStatus
    }
    deriving (Show, Read, Eq, Ord)


-- | Number of theoretically possible V4 RL states.
theoreticalStateCount :: Int
theoreticalStateCount =
    12 * 12 * 12 * 4 * 2


-- | Number of theoretically possible V4 state-action entries.
theoreticalEntryCount :: Int
theoreticalEntryCount =
    theoreticalStateCount * 3


-- -----------------------------------------------------------------------------
-- Geometry helpers
-- -----------------------------------------------------------------------------

-- | Returns all four neighbouring cells of a map position.
neighbours :: Position -> [Position]
neighbours (x, y) =
    [
        (x + 1, y),
        (x - 1, y),
        (x, y + 1),
        (x, y - 1)
    ]


-- | Replaces one worm in a list with its updated version.
replaceWorm :: Worm -> [Worm] -> [Worm]
replaceWorm updatedWorm worms =
    [
        if wormId worm == wormId updatedWorm
            then updatedWorm
            else worm
        | worm <- worms
    ]


-- | Computes the hypothetical controlled worm after one action.
wormAfterAction :: GameState -> Worm -> Action -> Worm
wormAfterAction state worm action =
    moveWormAfterAction grows action worm
  where
    nextHead = headAfterAction worm action
    grows = isFood (gameMap state) nextHead


-- -----------------------------------------------------------------------------
-- Reusable map-search context
-- -----------------------------------------------------------------------------

-- | Static map information shared by all graph searches performed for one game
-- state.
data MapSearchBase = MapSearchBase
    {
        searchMapWidth :: Int,
        searchMapHeight :: Int,
        searchStaticBlocked :: Set.Set Position,
        searchFoodPositions :: Set.Set Position
    }


-- | Complete graph-search context for one worm position.
data SearchContext = SearchContext
    {
        searchBase :: MapSearchBase,
        searchOccupied :: Set.Set Position
    }


-- | Builds reusable static search information from a game map in one traversal
-- of its explicitly stored tiles.
makeMapSearchBase :: GameMap -> MapSearchBase
makeMapSearchBase currentMap =
    MapSearchBase
        {
            searchMapWidth = mapWidth currentMap,
            searchMapHeight = mapHeight currentMap,
            searchStaticBlocked = blockedPositions,
            searchFoodPositions = foods
        }
  where
    (blockedPositions, foods) =
        Map.foldrWithKey collectTile (Set.empty, Set.empty) (mapTiles currentMap)

    collectTile position tile (blocked, foodSet) =
        case tile of
            Wall ->
                (Set.insert position blocked, foodSet)

            Poison ->
                (Set.insert position blocked, foodSet)

            Food ->
                (blocked, Set.insert position foodSet)

            Empty ->
                (blocked, foodSet)


-- | Builds the dynamic part of a search context. The controlled worm's head is
-- removed from occupied cells so graph searches may start there.
makeSearchContext :: MapSearchBase -> GameState -> Worm -> SearchContext
makeSearchContext base state worm =
    SearchContext
        {
            searchBase = base,
            searchOccupied = occupiedWithoutOwnHead
        }
  where
    aliveWorms =
        filter wormAlive (gameWorms state)

    occupied =
        Set.fromList (occupiedPositions aliveWorms)

    occupiedWithoutOwnHead =
        Set.delete (wormHead worm) occupied


-- | Returns True if a position lies inside the map represented by a search
-- context.
isInsideSearchMap :: MapSearchBase -> Position -> Bool
isInsideSearchMap base (x, y) =
    x >= 0
        && y >= 0
        && x < searchMapWidth base
        && y < searchMapHeight base


-- | Returns True if a position blocks an ordinary graph search.
isSearchBlocked :: SearchContext -> Position -> Bool
isSearchBlocked context position =
    not (isInsideSearchMap base position)
        || position `Set.member` searchStaticBlocked base
        || position `Set.member` searchOccupied context
  where
    base =
        searchBase context


-- | Returns True if a position blocks a graph search with extra temporary
-- blockers such as predicted enemy head positions.
isSearchBlockedWith :: SearchContext -> Set.Set Position -> Position -> Bool
isSearchBlockedWith context extraBlocked position =
    isSearchBlocked context position
        || position `Set.member` extraBlocked


-- -----------------------------------------------------------------------------
-- Reusable graph search
-- -----------------------------------------------------------------------------

-- | Returns every position reachable from the given start while respecting the
-- ordinary and extra blockers.
--
-- Positions are marked visited when enqueued so the same position is never
-- added to the BFS queue repeatedly.
reachablePositions :: SearchContext -> Set.Set Position -> Position -> Set.Set Position
reachablePositions context extraBlocked start
    | isSearchBlockedWith context extraBlocked start =
        Set.empty

    | otherwise =
        search
            (Seq.singleton start)
            (Set.singleton start)
  where
    search queue visited =
        case Seq.viewl queue of
            Seq.EmptyL ->
                visited

            position Seq.:< remainingQueue ->
                let nextPositions =
                        [
                            nextPosition
                            | nextPosition <- neighbours position,
                              nextPosition `Set.notMember` visited,
                              not (isSearchBlockedWith context extraBlocked nextPosition)
                        ]

                    updatedVisited =
                        foldl' (flip Set.insert) visited nextPositions

                    updatedQueue =
                        foldl' (Seq.|>) remainingQueue nextPositions

                in
                    search updatedQueue updatedVisited


-- -----------------------------------------------------------------------------
-- Path-aware food navigation
-- -----------------------------------------------------------------------------

-- | Returns the action corresponding to one neighbouring position.
actionForFirstStep :: Worm -> Position -> Maybe Action
actionForFirstStep worm position =
    case
        [
            action
            | action <- allActions,
              headAfterAction worm action == position
        ]
    of
        action : _ -> Just action
        [] -> Nothing


-- | Finds the first action and distance of the shortest currently reachable path
-- to any food item using an already prepared graph-search context.
foodPathInfoWithContext :: SearchContext -> Worm -> Maybe (Action, Int)
foodPathInfoWithContext context worm =
    search initialQueue initialVisited
  where
    startPosition =
        wormHead worm

    initialSteps =
        [
            position
            | position <- neighbours startPosition,
              not (isSearchBlocked context position)
        ]

    initialQueue =
        Seq.fromList
            [
                (position, position, 1)
                | position <- initialSteps
            ]

    initialVisited =
        foldl'
            (flip Set.insert)
            (Set.singleton startPosition)
            initialSteps

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
                            continueSearch
                                position
                                firstStep
                                pathLength
                                remainingQueue
                                visited

                | otherwise ->
                    continueSearch
                        position
                        firstStep
                        pathLength
                        remainingQueue
                        visited

    continueSearch position firstStep pathLength remainingQueue visited =
        let nextPositions =
                [
                    nextPosition
                    | nextPosition <- neighbours position,
                      nextPosition `Set.notMember` visited,
                      not (isSearchBlocked context nextPosition)
                ]

            updatedVisited =
                foldl'
                    (flip Set.insert)
                    visited
                    nextPositions

            updatedQueue =
                foldl'
                    (\currentQueue nextPosition ->
                        currentQueue
                            Seq.|>
                                (
                                    nextPosition,
                                    firstStep,
                                    pathLength + 1
                                )
                    )
                    remainingQueue
                    nextPositions

        in
            search updatedQueue updatedVisited


-- | Finds the first action and distance of the shortest currently reachable path
-- to any food item.
--
-- Walls, poison and currently occupied worm cells are treated as obstacles.
foodPathInfo :: GameState -> Worm -> Maybe (Action, Int)
foodPathInfo state worm =
    foodPathInfoWithContext context worm
  where
    base =
        makeMapSearchBase (gameMap state)

    context =
        makeSearchContext base state worm


-- | Converts an already calculated food path into its relative direction.
foodPathDirectionFromInfo :: Maybe (Action, Int) -> FoodPathDirection
foodPathDirectionFromInfo pathInfo =
    case pathInfo of
        Just (TurnLeft, _) -> FoodPathLeft
        Just (GoStraight, _) -> FoodPathStraight
        Just (TurnRight, _) -> FoodPathRight
        Nothing -> FoodPathUnavailable


-- | Returns the shortest currently reachable path distance to food.
foodPathDistance :: GameState -> Worm -> Maybe Int
foodPathDistance state worm =
    fmap snd (foodPathInfo state worm)


-- -----------------------------------------------------------------------------
-- Opponent threat prediction
-- -----------------------------------------------------------------------------

-- | Returns all cells that living opponents could safely move their heads into.
possibleEnemyNextHeads :: GameState -> Worm -> Set.Set Position
possibleEnemyNextHeads state controlled =
    Set.fromList
        [
            headAfterAction enemy action
            | enemy <- gameWorms state,
              wormAlive enemy,
              wormId enemy /= wormId controlled,
              action <- safeActions state enemy
        ]


-- -----------------------------------------------------------------------------
-- Head-history and loop detection
-- -----------------------------------------------------------------------------

-- | Number of recent head positions considered for loop detection.
loopHistoryWindow :: Int
loopHistoryWindow = 256


-- | Returns recent head positions of one worm from newest to oldest.
recentHeadPositions :: GameState -> Worm -> [Position]
recentHeadPositions state worm =
    take
        loopHistoryWindow
        (Map.findWithDefault [] (wormId worm) (gameHeadHistory state))


-- | Converts newest-first head-position history into directed movement edges.
--
-- For history [current, previous, older], the resulting edges are
-- [(previous, current), (older, previous)].
recentMovementEdges :: [Position] -> [(Position, Position)]
recentMovementEdges history =
    zip
        (drop 1 history)
        history


-- | Counts visits to a position in an already extracted recent history.
recentVisitCountIn :: [Position] -> Position -> Int
recentVisitCountIn history position =
    length
        (filter (== position) history)


-- | Counts how often one directed movement edge occurred in recent history.
recentEdgeVisitCountIn :: [Position] -> Position -> Position -> Int
recentEdgeVisitCountIn history fromPosition toPosition =
    length
        ( filter
            (== (fromPosition, toPosition))
            (recentMovementEdges history)
        )


-- | Determines whether the most recent movement edge has already occurred
-- earlier in the stored history.
--
-- Repeating an edge is a stronger indication of following the same route than
-- merely revisiting a position and allows substantially larger loops to be
-- detected.
loopStatusFromHistory :: [Position] -> LoopStatus
loopStatusFromHistory history =
    case recentMovementEdges history of
        [] ->
            NotRepeating

        currentEdge : olderEdges
            | currentEdge `elem` olderEdges ->
                RepeatingLoop

            | otherwise ->
                NotRepeating


-- | Determines whether the worm appears to be repeating a movement loop.
loopStatusForWorm :: GameState -> Worm -> LoopStatus
loopStatusForWorm state worm =
    loopStatusFromHistory
        (recentHeadPositions state worm)


-- -----------------------------------------------------------------------------
-- Tail connectivity
-- -----------------------------------------------------------------------------

-- | Determines whether the worm's tail is reachable from the threat-aware
-- reachable component.
--
-- The tail itself is currently occupied, so reaching a safe neighbour of the
-- tail is sufficient.
tailReachableFrom :: Worm -> Set.Set Position -> Set.Set Position -> Bool
tailReachableFrom worm threats safeReachable =
    case reverse (wormBody worm) of
        [] ->
            False

        [_] ->
            True

        tailPosition : _ ->
            tailPosition `Set.notMember` threats
                && any
                    (`Set.member` safeReachable)
                    (neighbours tailPosition)


-- | Returns True if the worm can still reach its own tail after performing an
-- immediately safe action.
canReachTailAfterAction :: GameState -> Worm -> Action -> Bool
canReachTailAfterAction state worm action
    | action `notElem` currentSafeActions =
        False

    | otherwise =
        tailReachableFrom
            movedWorm
            futureEnemyThreats
            safeReachable
  where
    currentSafeActions =
        safeActions state worm

    movedWorm =
        wormAfterAction state worm action

    hypotheticalState =
        state
            {
                gameWorms = replaceWorm movedWorm (gameWorms state)
            }

    futureEnemyThreats =
        possibleEnemyNextHeads hypotheticalState movedWorm

    base =
        makeMapSearchBase (gameMap state)

    searchContext =
        makeSearchContext
            base
            hypotheticalState
            movedWorm

    safeReachable =
        reachablePositions
            searchContext
            futureEnemyThreats
            (wormHead movedWorm)


-- -----------------------------------------------------------------------------
-- Action analysis
-- -----------------------------------------------------------------------------

-- | Internal diagnostics used to classify one candidate action.
data ActionAnalysis = ActionAnalysis
    {
        analysisQuality :: ActionQuality,
        analysisRawArea :: Int,
        analysisThreatAwareArea :: Int,
        analysisRawNextMoves :: Int,
        analysisRobustNextMoves :: Int,
        analysisCanReachTail :: Bool,
        analysisFoodPathDistance :: Maybe Int,
        analysisRecentEdgeVisits :: Int,
        analysisRecentVisits :: Int
    }


-- | Information shared by analysis of all three actions from the same state.
data ActionAnalysisContext = ActionAnalysisContext
    {
        analysisCurrentSafeActions :: [Action],
        analysisCurrentEnemyThreats :: Set.Set Position,
        analysisSearchBase :: MapSearchBase,
        analysisRecentHistory :: [Position]
    }


-- | Complete reusable analysis of one game state.
data StateAnalysis = StateAnalysis
    {
        stateAnalysisRlState :: RLState,
        stateAnalysisLeft :: ActionAnalysis,
        stateAnalysisStraight :: ActionAnalysis,
        stateAnalysisRight :: ActionAnalysis
    }


-- | Creates information shared by analysis of all three candidate actions.
makeActionAnalysisContext :: GameState -> Worm -> ActionAnalysisContext
makeActionAnalysisContext state worm =
    ActionAnalysisContext
        {
            analysisCurrentSafeActions = safeActions state worm,
            analysisCurrentEnemyThreats = possibleEnemyNextHeads state worm,
            analysisSearchBase = makeMapSearchBase (gameMap state),
            analysisRecentHistory = recentHeadPositions state worm
        }


-- | Converts reachable area into an action quality.
qualityFromArea :: Bool -> Worm -> Int -> ActionQuality
qualityFromArea forced worm area
    | 2 * area <= wormLength =
        if forced
            then ForcedCritical
            else Critical

    | area <= wormLength =
        if forced
            then ForcedRestricted
            else Restricted

    | area <= 2 * wormLength =
        if forced
            then ForcedLimited
            else Limited

    | otherwise =
        if forced
            then ForcedOpen
            else Open
  where
    wormLength =
        length (wormBody worm)


-- | Analyses one candidate action using state-level information shared between
-- all three candidate actions.
analyzeActionWithContext :: ActionAnalysisContext -> GameState -> Worm -> Action -> ActionAnalysis
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
        analysisWithQuality
            ( qualityFromArea
                (robustMoveCount == 1)
                movedWorm
                safeArea
            )

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
        state
            {
                gameWorms = replaceWorm movedWorm (gameWorms state)
            }

    futureEnemyThreats =
        possibleEnemyNextHeads hypotheticalState movedWorm

    futureSafeActions =
        safeActions hypotheticalState movedWorm

    robustFutureActions =
        [
            futureAction
            | futureAction <- futureSafeActions,
              headAfterAction movedWorm futureAction `Set.notMember` futureEnemyThreats
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

    safeReachable =
        reachablePositions
            searchContext
            futureEnemyThreats
            (wormHead movedWorm)

    safeArea =
        Set.size safeReachable

    rawReachable =
        reachablePositions
            searchContext
            Set.empty
            (wormHead movedWorm)

    rawArea =
        Set.size rawReachable

    tailReachable =
        tailReachableFrom
            movedWorm
            futureEnemyThreats
            safeReachable

    pathDistance =
        fmap snd
            (foodPathInfoWithContext searchContext movedWorm)

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
            {
                analysisQuality = quality,
                analysisRawArea = 0,
                analysisThreatAwareArea = 0,
                analysisRawNextMoves = 0,
                analysisRobustNextMoves = 0,
                analysisCanReachTail = False,
                analysisFoodPathDistance = Nothing,
                analysisRecentEdgeVisits = edgeVisits,
                analysisRecentVisits = visits
            }

    analysisWithQuality quality =
        ActionAnalysis
            {
                analysisQuality = quality,
                analysisRawArea = rawArea,
                analysisThreatAwareArea = safeArea,
                analysisRawNextMoves = rawMoveCount,
                analysisRobustNextMoves = robustMoveCount,
                analysisCanReachTail = tailReachable,
                analysisFoodPathDistance = pathDistance,
                analysisRecentEdgeVisits = edgeVisits,
                analysisRecentVisits = visits
            }


-- | Analyses the current state once and shares the expensive intermediate data
-- between state encoding, fallback selection and debugging.
analyzeState :: GameState -> Worm -> StateAnalysis
analyzeState state worm =
    StateAnalysis
        {
            stateAnalysisRlState = rlState,
            stateAnalysisLeft = leftAnalysis,
            stateAnalysisStraight = straightAnalysis,
            stateAnalysisRight = rightAnalysis
        }
  where
    context =
        makeActionAnalysisContext state worm

    leftAnalysis =
        analyzeActionWithContext
            context
            state
            worm
            TurnLeft

    straightAnalysis =
        analyzeActionWithContext
            context
            state
            worm
            GoStraight

    rightAnalysis =
        analyzeActionWithContext
            context
            state
            worm
            TurnRight

    currentSearchContext =
        makeSearchContext
            (analysisSearchBase context)
            state
            worm

    currentFoodPath =
        foodPathInfoWithContext
            currentSearchContext
            worm

    currentLoopStatus =
        loopStatusFromHistory
            (analysisRecentHistory context)

    rlState =
        RLState
            {
                qualityLeft = analysisQuality leftAnalysis,
                qualityStraight = analysisQuality straightAnalysis,
                qualityRight = analysisQuality rightAnalysis,
                foodPathDirection = foodPathDirectionFromInfo currentFoodPath,
                loopStatus = currentLoopStatus
            }


-- | Returns all three action analyses from one complete state analysis.
stateActionAnalyses :: StateAnalysis -> [(Action, ActionAnalysis)]
stateActionAnalyses analysis =
    [
        (TurnLeft, stateAnalysisLeft analysis),
        (GoStraight, stateAnalysisStraight analysis),
        (TurnRight, stateAnalysisRight analysis)
    ]


-- -----------------------------------------------------------------------------
-- State encoding
-- -----------------------------------------------------------------------------

-- | Encodes the game using V4 action qualities, path-aware food navigation and
-- loop status.
encodeState :: GameState -> Worm -> RLState
encodeState state worm =
    stateAnalysisRlState
        (analyzeState state worm)


-- -----------------------------------------------------------------------------
-- Reward
-- -----------------------------------------------------------------------------

-- | Computes shaping reward using actual reachable path distance to food.
foodDistanceReward :: GameState -> Worm -> GameState -> Worm -> Double
foodDistanceReward beforeState beforeWorm afterState afterWorm
    | Core.wormFoodDelta beforeWorm afterWorm > 0 =
        0

    | otherwise =
        case
            (
                foodPathDistance beforeState beforeWorm,
                foodPathDistance afterState afterWorm
            )
        of
            (Just beforeDistance, Just afterDistance)
                | afterDistance < beforeDistance -> 2
                | afterDistance > beforeDistance -> -2
                | otherwise -> 0

            (Just _, Nothing) ->
                -2

            (Nothing, Just _) ->
                2

            _ ->
                0


-- | Computes the V4 reward for one transition.
--
-- Numerical reward weights remain equal to V3. Only food-distance shaping now
-- uses actual navigable path distance instead of Manhattan distance.
rewardForStep :: GameState -> Worm -> GameState -> Worm -> Double
rewardForStep beforeState beforeWorm afterState afterWorm =
    foodReward + killReward + deathPenalty + distanceReward + timePenalty
  where
    foodReward =
        50 * fromIntegral (Core.wormFoodDelta beforeWorm afterWorm)

    killReward =
        10 * fromIntegral (Core.wormKillDelta beforeWorm afterWorm)

    deathPenalty =
        if Core.wormDied beforeWorm afterWorm
            then -100
            else 0

    distanceReward =
        foodDistanceReward
            beforeState
            beforeWorm
            afterState
            afterWorm

    timePenalty =
        -1


-- -----------------------------------------------------------------------------
-- V4 Q-learning specification
-- -----------------------------------------------------------------------------

-- | Complete Q-learning specification of version 4.
v4Spec :: Core.QLearningSpec RLState
v4Spec =
    Core.QLearningSpec
        {
            Core.qlEncodeState = encodeState,
            Core.qlRewardForStep = rewardForStep
        }


-- | Q-table used by Q-learning version 4.
type QTable = Core.QTable RLState


-- | Saves a version-4 Q-table.
saveQTable :: FilePath -> QTable -> IO ()
saveQTable =
    Core.saveQTable


-- | Loads a version-4 Q-table.
loadQTable :: FilePath -> IO QTable
loadQTable =
    Core.loadQTable


-- | Creates the greedy pure V4 learned agent.
qLearningAgent :: QTable -> Agent
qLearningAgent =
    Core.qLearningAgent v4Spec


-- -----------------------------------------------------------------------------
-- V4 safety layer and fallback
-- -----------------------------------------------------------------------------

-- | Returns True for action qualities that represent immediate or unavoidable
-- danger and should be discarded whenever any other candidate exists.
isHardUnsafeQuality :: ActionQuality -> Bool
isHardUnsafeQuality Fatal = True
isHardUnsafeQuality Contested = True
isHardUnsafeQuality DeadEnd = True
isHardUnsafeQuality _ = False


-- | Returns True for actions whose resulting accessible area is critically
-- small relative to the worm.
isCriticalQuality :: ActionQuality -> Bool
isCriticalQuality ForcedCritical = True
isCriticalQuality Critical = True
isCriticalQuality _ = False


-- | Removes hard-unsafe actions whenever at least one other action exists.
preferNonHardUnsafe :: [(Action, ActionAnalysis)] -> [(Action, ActionAnalysis)]
preferNonHardUnsafe analyses =
    if null saferActions
        then analyses
        else saferActions
  where
    saferActions =
        filter
            (not . isHardUnsafeQuality . analysisQuality . snd)
            analyses


-- | Removes critically space-restricted actions whenever a non-critical
-- alternative exists.
preferNonCritical :: [(Action, ActionAnalysis)] -> [(Action, ActionAnalysis)]
preferNonCritical analyses =
    if null nonCriticalActions
        then analyses
        else nonCriticalActions
  where
    nonCriticalActions =
        filter
            (not . isCriticalQuality . analysisQuality . snd)
            analyses


-- | Assigns a numerical fallback preference to an action quality.
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


-- | Converts a path distance into a score where shorter reachable paths are
-- preferred.
foodDistanceScore :: Maybe Int -> Int
foodDistanceScore Nothing =
    -1000000

foodDistanceScore (Just pathLength) =
    negate pathLength


-- | Computes the fallback score of one analysed action.
fallbackScore :: ActionAnalysis -> (Int, Int, Int, Int, Int, Int)
fallbackScore analysis =
    (
        qualityScore (analysisQuality analysis),
        analysisRobustNextMoves analysis,
        analysisThreatAwareArea analysis,
        foodDistanceScore (analysisFoodPathDistance analysis),
        negate (analysisRecentEdgeVisits analysis),
        negate (analysisRecentVisits analysis)
    )


-- | Keeps tail-connected actions whenever at least one such action exists.
preferTailConnected :: [(Action, ActionAnalysis)] -> [(Action, ActionAnalysis)]
preferTailConnected analyses =
    if null tailConnected
        then analyses
        else tailConnected
  where
    tailConnected =
        filter
            (analysisCanReachTail . snd)
            analyses


-- | Avoids continuing already traversed movement edges when a repeating loop
-- has been detected.
--
-- Directed edges are preferred over plain position counts because they
-- distinguish continuing the same circuit from leaving it through a new route.
preferLoopBreaking :: LoopStatus -> [(Action, ActionAnalysis)] -> [(Action, ActionAnalysis)]
preferLoopBreaking NotRepeating analyses =
    analyses

preferLoopBreaking RepeatingLoop analyses =
    preferMinimumBy
        analysisRecentVisits
        ( preferMinimumBy
            analysisRecentEdgeVisits
            analyses
        )


-- | Chooses the best action according to detailed V4 fallback diagnostics.
fallbackAction :: [(Action, ActionAnalysis)] -> IO Action
fallbackAction analyses =
    randomChoice bestActions
  where
    bestScore =
        maximum
            (map (fallbackScore . snd) analyses)

    bestActions =
        [
            action
            | (action, analysis) <- analyses,
              fallbackScore analysis == bestScore
        ]


-- | Creates a V4 agent using learned Q-values behind a safety, topology and
-- loop-breaking policy layer.
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
        [
            (action, analysis)
            | (action, analysis) <- policyCandidates,
              Core.hasQValue table state action
        ]

    bestLearnedValue =
        maximum
            [
                Core.qValue table state action
                | (action, _) <- learnedCandidates
            ]

    bestLearnedActions =
        [
            action
            | (action, _) <- learnedCandidates,
              Core.qValue table state action == bestLearnedValue
        ]

-- | Keeps candidates with the minimum value of the selected diagnostic.
preferMinimumBy :: (ActionAnalysis -> Int) -> [(Action, ActionAnalysis)] -> [(Action, ActionAnalysis)]
preferMinimumBy _ [] =
    []

preferMinimumBy selector analyses =
    filter
        ((== minimumValue) . selector . snd)
        analyses
  where
    minimumValue =
        minimum
            (map (selector . snd) analyses)


-- -----------------------------------------------------------------------------
-- V4 debugging
-- -----------------------------------------------------------------------------

-- | Converts the encoded V4 state into human-readable values.
describeState :: RLState -> [(String, String)]
describeState state =
    [
        ( "Quality L/S/R", show (qualityLeft state) ++ " / " ++ show (qualityStraight state) ++ " / " ++ show (qualityRight state) ),
        ( "Food path", show (foodPathDirection state) ),
        ( "Loop status", show (loopStatus state) )
    ]


-- | Formats an optional food-path distance.
showPathDistance :: Maybe Int -> String
showPathDistance Nothing =
    "-"

showPathDistance (Just pathLength) =
    show pathLength


-- | Creates detailed debugging information for a V4 learned agent.
v4DebugProvider :: QTable -> Debug.AgentDebugProvider
v4DebugProvider table gameState worm =
    let completeAnalysis =
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
                ++ " / "
                ++ show (selector straightAnalysis)
                ++ " / "
                ++ show (selector rightAnalysis)

        showDistanceTriple =
            showPathDistance (analysisFoodPathDistance leftAnalysis)
                ++ " / "
                ++ showPathDistance (analysisFoodPathDistance straightAnalysis)
                ++ " / "
                ++ showPathDistance (analysisFoodPathDistance rightAnalysis)

        knownActionCount =
            length
                [
                    action
                    | action <- allActions,
                      Core.hasQValue table state action
                ]

    in
        Debug.AgentDebugInfo
            {
                Debug.debugStateLines =
                    describeState state
                    ++
                    [
                        ( "Raw area L/S/R", showTriple analysisRawArea ),
                        ( "Safe area L/S/R", showTriple analysisThreatAwareArea ),
                        ( "Raw moves L/S/R", showTriple analysisRawNextMoves ),
                        ( "Robust moves L/S/R", showTriple analysisRobustNextMoves ),
                        ( "Tail reachable L/S/R", showTriple analysisCanReachTail ),
                        ( "Food dist L/S/R", showDistanceTriple ),
                        ( "Edge visits L/S/R", showTriple analysisRecentEdgeVisits ),
                        ( "Recent visits L/S/R", showTriple analysisRecentVisits ),
                        ( "Known Q actions", show knownActionCount ++ " / 3" ),
                        ( "Length", show (length (wormBody worm)) )
                    ],

                Debug.debugQValues =
                    Core.qValuesForState table state,

                Debug.debugBestActions =
                    Core.bestQActions table state
            }