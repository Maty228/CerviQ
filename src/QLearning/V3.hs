{-|
Module      : QLearning.V3
Description : Third tabular Q-learning state and fallback policy for CerviQ.

Version 3 replaces V2's separate danger and space fields with one
'ActionQuality' for each candidate action. Each quality combines immediate
collision safety, one-step opponent threats, future manoeuvrability, and
reachable space. Relative food direction from V2 is retained.

V3 also introduces a fallback policy that can use the encoded safety
information when the Q-table has no suitable learned value. The generic
Q-learning algorithm and table operations remain in "QLearning.Core".
-}

module QLearning.V3
    ( ActionQuality(..)
    , ForwardFoodDirection(..)
    , SideFoodDirection(..)
    , RLState(..)

    , encodeState
    , rewardForStep

    , v3Spec
    , QTable

    , saveQTable
    , loadQTable
    , qLearningAgent
    , qLearningAgentWithFallback

    , describeState
    , v3DebugProvider
    ) where

import Agent
import Collision
import Maps
import Movement
import Types

import qualified Data.Set as Set
import qualified QLearning.Core as Core
import qualified QLearning.Debug as Debug


-- -----------------------------------------------------------------------------
-- RL state representation
-- -----------------------------------------------------------------------------

-- | Overall quality of one candidate action.
--
-- The categories form a compact summary of several signals:
--
-- * 'Fatal' means the action is immediately unsafe.
-- * 'Contested' means an opponent could move its head onto the destination
--   during the same simultaneous turn.
-- * 'DeadEnd' means the move is currently safe but leaves no ordinary safe
--   continuation.
-- * 'Threatened' means ordinary continuations exist, but none remain after
--   applying the one-step opponent-threat approximation.
-- * the remaining categories describe threat-aware reachable space, with
--   @Forced@ variants indicating exactly one robust continuation.
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


-- | Position of the nearest food along the worm's forward/backward axis.
data ForwardFoodDirection
    = FoodAhead
    | FoodSameForward
    | FoodBehind
    deriving (Show, Read, Eq, Ord)


-- | Position of the nearest food along the worm's left/right axis.
data SideFoodDirection
    = FoodLeft
    | FoodSameSide
    | FoodRight
    deriving (Show, Read, Eq, Ord)


-- | Version-3 state used as the key of the tabular Q-table.
--
-- Compared with V2, the three separate danger values and three space values are
-- compressed into one 'ActionQuality' per action. Relative food direction is
-- retained unchanged.
data RLState = RLState
    { qualityLeft :: ActionQuality
    , qualityStraight :: ActionQuality
    , qualityRight :: ActionQuality
    , foodForward :: ForwardFoodDirection
    , foodSideways :: SideFoodDirection
    }
    deriving (Show, Read, Eq, Ord)


-- -----------------------------------------------------------------------------
-- Relative food direction
-- -----------------------------------------------------------------------------

-- | Converts the displacement to food into worm-relative coordinates.
--
-- The first returned component is positive in front of the worm and negative
-- behind it. The second is positive to the worm's right and negative to its
-- left.
relativeFoodDeltas :: Worm -> Position -> (Int, Int)
relativeFoodDeltas worm (foodX, foodY) =
    case wormDirection worm of
        North -> (-dy, dx)
        East -> (dx, dy)
        South -> (dy, -dx)
        West -> (-dx, -dy)
  where
    (headX, headY) = wormHead worm
    dx = foodX - headX
    dy = foodY - headY


-- | Returns the nearest food's forward/backward relation to the worm.
forwardFoodDirection :: Worm -> Position -> ForwardFoodDirection
forwardFoodDirection worm food =
    case compare forwardDelta 0 of
        GT -> FoodAhead
        EQ -> FoodSameForward
        LT -> FoodBehind
  where
    (forwardDelta, _) = relativeFoodDeltas worm food


-- | Returns the nearest food's left/right relation to the worm.
sideFoodDirection :: Worm -> Position -> SideFoodDirection
sideFoodDirection worm food =
    case compare sideDelta 0 of
        LT -> FoodLeft
        EQ -> FoodSameSide
        GT -> FoodRight
  where
    (_, sideDelta) = relativeFoodDeltas worm food


-- -----------------------------------------------------------------------------
-- Hypothetical movement
-- -----------------------------------------------------------------------------

-- | Returns all four orthogonal neighbours of a map position.
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
-- Growth is included when the candidate destination currently contains food.
-- Other worms are left at their current positions.
wormAfterAction :: GameState -> Worm -> Action -> Worm
wormAfterAction state worm action =
    moveWormAfterAction grows action worm
  where
    nextHead = headAfterAction worm action
    grows = isFood (gameMap state) nextHead


-- -----------------------------------------------------------------------------
-- One-step opponent threats
-- -----------------------------------------------------------------------------

-- | Returns every cell a living opponent could safely move its head into during
-- the current simultaneous game turn.
--
-- This is deliberately only a one-step threat approximation. It does not know
-- which action an opponent will actually choose and does not perform deeper
-- adversarial search.
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
-- Reachable-space analysis
-- -----------------------------------------------------------------------------

-- | Returns whether a position blocks ordinary reachable-area exploration.
--
-- The controlled worm's head is removed from the occupied set so it can serve
-- as the flood-fill starting position. All remaining living worm segments are
-- treated as obstacles.
blocksReachableArea :: GameState -> Worm -> Position -> Bool
blocksReachableArea state worm position =
    isBlocked currentMap position
        || isPoison currentMap position
        || position `Set.member` occupiedWithoutOwnHead
  where
    currentMap = gameMap state
    aliveWorms = filter wormAlive (gameWorms state)
    occupied = Set.fromList (occupiedPositions aliveWorms)
    occupiedWithoutOwnHead = Set.delete (wormHead worm) occupied


-- | Counts cells reachable from the worm's current head using ordinary
-- flood-fill exploration.
reachableArea :: GameState -> Worm -> Int
reachableArea state worm =
    floodFill Set.empty [wormHead worm]
  where
    floodFill visited [] = Set.size visited
    floodFill visited (position : rest)
        | position `Set.member` visited = floodFill visited rest
        | blocksReachableArea state worm position = floodFill visited rest
        | otherwise =
            floodFill
                (Set.insert position visited)
                (neighbours position ++ rest)


-- | Returns whether a position blocks threat-aware reachable-area exploration.
--
-- In addition to ordinary obstacles, every cell in the supplied one-step
-- opponent-threat set is considered unavailable.
blocksThreatAwareArea :: Set.Set Position -> GameState -> Worm -> Position -> Bool
blocksThreatAwareArea threats state worm position =
    blocksReachableArea state worm position
        || position `Set.member` threats


-- | Counts cells reachable while treating one-step opponent threat positions as
-- blocked.
threatAwareReachableArea :: Set.Set Position -> GameState -> Worm -> Int
threatAwareReachableArea threats state worm =
    floodFill Set.empty [wormHead worm]
  where
    floodFill visited [] = Set.size visited
    floodFill visited (position : rest)
        | position `Set.member` visited = floodFill visited rest
        | blocksThreatAwareArea threats state worm position = floodFill visited rest
        | otherwise =
            floodFill
                (Set.insert position visited)
                (neighbours position ++ rest)


-- -----------------------------------------------------------------------------
-- Action analysis
-- -----------------------------------------------------------------------------

-- | Internal measurements used to classify one candidate action.
--
-- Only 'analysisQuality' becomes part of the learned V3 state. The remaining
-- values are retained for diagnostics and for constructing that quality.
data ActionAnalysis = ActionAnalysis
    { analysisQuality :: ActionQuality
    , analysisRawArea :: Int
    , analysisThreatAwareArea :: Int
    , analysisRawNextMoves :: Int
    , analysisRobustNextMoves :: Int
    }


-- | Converts threat-aware reachable area into a V3 action-quality category.
--
-- The thresholds are relative to the moved worm's length. @Forced@ categories
-- indicate that exactly one robust continuation remains.
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


-- | Returns ordinary immediately safe actions from a hypothetical state.
rawNextActions :: GameState -> Worm -> [Action]
rawNextActions state worm =
    safeActions state worm


-- | Returns immediately safe future actions whose destinations are not part of
-- the supplied one-step opponent-threat set.
--
-- The same threat set calculated for the current turn is reused as a
-- conservative approximation; V3 does not simulate opponents another turn into
-- the future.
robustNextActions :: Set.Set Position -> GameState -> Worm -> [Action]
robustNextActions threats state worm =
    [ action
    | action <- safeActions state worm
    , headAfterAction worm action `Set.notMember` threats
    ]


-- | Analyses one candidate action and assigns its V3 'ActionQuality'.
--
-- Classification proceeds from strongest failure conditions to increasingly
-- coarse space information:
--
-- 1. immediately unsafe action -> 'Fatal';
-- 2. destination reachable by an opponent this turn -> 'Contested';
-- 3. no ordinary safe continuation -> 'DeadEnd';
-- 4. no threat-aware continuation -> 'Threatened';
-- 5. otherwise classify threat-aware reachable area, additionally recording
--    whether exactly one robust continuation remains.
analyzeAction :: GameState -> Worm -> Action -> ActionAnalysis
analyzeAction state worm action
    | action `notElem` currentSafeActions =
        emptyAnalysis Fatal

    | nextHead `Set.member` enemyThreats =
        emptyAnalysis Contested

    | rawMoveCount == 0 =
        analysisWithQuality DeadEnd

    | robustMoveCount == 0 =
        analysisWithQuality Threatened

    | otherwise =
        analysisWithQuality $
            qualityFromArea (robustMoveCount == 1) movedWorm safeArea

  where
    currentSafeActions = safeActions state worm
    enemyThreats = possibleEnemyNextHeads state worm
    nextHead = headAfterAction worm action

    movedWorm = wormAfterAction state worm action
    hypotheticalState = state {gameWorms = replaceWorm movedWorm (gameWorms state)}

    rawArea = reachableArea hypotheticalState movedWorm
    safeArea = threatAwareReachableArea enemyThreats hypotheticalState movedWorm

    rawMoveCount = length (rawNextActions hypotheticalState movedWorm)
    robustMoveCount = length (robustNextActions enemyThreats hypotheticalState movedWorm)

    emptyAnalysis quality =
        ActionAnalysis
            { analysisQuality = quality
            , analysisRawArea = 0
            , analysisThreatAwareArea = 0
            , analysisRawNextMoves = 0
            , analysisRobustNextMoves = 0
            }

    analysisWithQuality quality =
        ActionAnalysis
            { analysisQuality = quality
            , analysisRawArea = rawArea
            , analysisThreatAwareArea = safeArea
            , analysisRawNextMoves = rawMoveCount
            , analysisRobustNextMoves = robustMoveCount
            }

-- -----------------------------------------------------------------------------
-- State encoding
-- -----------------------------------------------------------------------------

-- | Encodes the current game situation from one worm's V3 perspective.
--
-- Each candidate action is independently reduced to one 'ActionQuality'.
-- Relative food direction is retained from V2.
encodeState :: GameState -> Worm -> RLState
encodeState state worm =
    RLState
        { qualityLeft = analysisQuality (analyzeAction state worm TurnLeft)
        , qualityStraight = analysisQuality (analyzeAction state worm GoStraight)
        , qualityRight = analysisQuality (analyzeAction state worm TurnRight)
        , foodForward =
            case nearest of
                Nothing -> FoodSameForward
                Just food -> forwardFoodDirection worm food
        , foodSideways =
            case nearest of
                Nothing -> FoodSameSide
                Just food -> sideFoodDirection worm food
        }
  where
    nearest = nearestFood state worm


-- -----------------------------------------------------------------------------
-- Reward
-- -----------------------------------------------------------------------------

-- | Returns Manhattan distance from the worm to the nearest food.
distanceToNearestFood :: GameState -> Worm -> Maybe Int
distanceToNearestFood state worm =
    fmap (distance (wormHead worm)) (nearestFood state worm)


-- | Computes reward shaping for progress towards the nearest food.
--
-- Eating already has its own larger reward, so distance shaping is suppressed
-- on transitions where food was consumed.
foodDistanceReward :: GameState -> Worm -> GameState -> Worm -> Double
foodDistanceReward beforeState beforeWorm afterState afterWorm
    | Core.wormFoodDelta beforeWorm afterWorm > 0 = 0
    | otherwise =
        case (distanceToNearestFood beforeState beforeWorm, distanceToNearestFood afterState afterWorm) of
            (Just beforeDistance, Just afterDistance)
                | afterDistance < beforeDistance -> 2
                | afterDistance > beforeDistance -> -2
                | otherwise -> 0
            _ -> 0


-- | Computes the V3 reward for one transition.
--
-- V3 intentionally keeps the same shaped reward as V2. This makes the V2/V3
-- comparison primarily a comparison of state representations rather than a
-- simultaneous change of both state and reward.
rewardForStep :: GameState -> Worm -> GameState -> Worm -> Double
rewardForStep beforeState beforeWorm afterState afterWorm =
    foodReward + killReward + deathPenalty + distanceReward + timePenalty
  where
    foodReward = 50 * fromIntegral (Core.wormFoodDelta beforeWorm afterWorm)
    killReward = 10 * fromIntegral (Core.wormKillDelta beforeWorm afterWorm)
    deathPenalty = if Core.wormDied beforeWorm afterWorm then -100 else 0
    distanceReward = foodDistanceReward beforeState beforeWorm afterState afterWorm
    timePenalty = -1


-- -----------------------------------------------------------------------------
-- V3 Q-learning specification
-- -----------------------------------------------------------------------------

-- | Complete version-specific specification passed to the shared Q-learning core.
v3Spec :: Core.QLearningSpec RLState
v3Spec =
    Core.QLearningSpec
        { Core.qlEncodeState = encodeState
        , Core.qlRewardForStep = rewardForStep
        }


-- | Q-table whose keys use the V3 state representation.
type QTable = Core.QTable RLState


-- | Saves a V3 Q-table.
saveQTable :: FilePath -> QTable -> IO ()
saveQTable = Core.saveQTable


-- | Loads a V3 Q-table.
loadQTable :: FilePath -> IO QTable
loadQTable = Core.loadQTable


-- | Creates a greedy learned agent from a V3 Q-table.
--
-- Unseen state-action pairs are numerically treated as having Q-value zero by
-- the generic core. 'qLearningAgentWithFallback' provides an alternative policy
-- for cases where explicit learned values are unavailable.
qLearningAgent :: QTable -> Agent
qLearningAgent = Core.qLearningAgent v3Spec


-- -----------------------------------------------------------------------------
-- V3 fallback policy
-- -----------------------------------------------------------------------------

-- | Returns whether an action quality is acceptable for learned action choice.
--
-- Immediately fatal, contested, and dead-end moves are excluded whenever at
-- least one better alternative exists. 'Threatened' remains acceptable because
-- it is not immediately fatal and may still be the best available route.
isAcceptableQuality :: ActionQuality -> Bool
isAcceptableQuality Fatal = False
isAcceptableQuality Contested = False
isAcceptableQuality DeadEnd = False
isAcceptableQuality _ = True


-- | Assigns an ordering score to V3 action qualities for fallback decisions.
--
-- Higher scores represent more desirable actions. Within the same approximate
-- space category, a non-forced action is preferred because it leaves more than
-- one robust continuation.
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


-- | Returns the encoded quality corresponding to one relative action.
qualityForAction :: RLState -> Action -> ActionQuality
qualityForAction state TurnLeft = qualityLeft state
qualityForAction state GoStraight = qualityStraight state
qualityForAction state TurnRight = qualityRight state


-- | Chooses an action solely from V3 state-quality information.
--
-- This is used when the Q-table contains no learned value for a suitable
-- candidate action. Ties between equally ranked qualities are resolved randomly.
fallbackAction :: RLState -> IO Action
fallbackAction state =
    randomChoice bestActions'
  where
    actionScores =
        [ (action, qualityScore (qualityForAction state action))
        | action <- allActions
        ]

    bestScore = maximum (map snd actionScores)
    bestActions' = [action | (action, score) <- actionScores, score == bestScore]


-- | Creates a V3 agent that combines learned Q-values with a safety fallback.
--
-- The policy works in three stages:
--
-- 1. discard 'Fatal', 'Contested', and 'DeadEnd' actions when at least one
--    acceptable alternative exists;
-- 2. among the remaining candidates, consider only actions explicitly present
--    in the Q-table;
-- 3. if no suitable learned action exists, choose using 'fallbackAction'.
--
-- Checking table membership with 'Core.hasQValue' is important because unseen
-- actions and actions explicitly learned to value zero both return numeric
-- Q-value @0@. Without this distinction, an unseen action could incorrectly
-- appear better than a learned action with a negative value.
qLearningAgentWithFallback :: QTable -> Agent
qLearningAgentWithFallback table gameState worm =
    case learnedCandidateActions of
        [] -> fallbackAction state
        actions -> chooseBestLearnedAction actions
  where
    state = encodeState gameState worm

    acceptableActions =
        [ action
        | action <- allActions
        , isAcceptableQuality (qualityForAction state action)
        ]

    candidateActions =
        if null acceptableActions
            then allActions
            else acceptableActions

    learnedCandidateActions =
        [ action
        | action <- candidateActions
        , Core.hasQValue table state action
        ]

    chooseBestLearnedAction actions =
        randomChoice bestActions'
      where
        actionValues = [(action, Core.qValue table state action) | action <- actions]
        bestValue = maximum (map snd actionValues)
        bestActions' = [action | (action, value) <- actionValues, value == bestValue]


-- -----------------------------------------------------------------------------
-- V3 debugging
-- -----------------------------------------------------------------------------

-- | Converts a V3 encoded state into human-readable diagnostic values.
describeState :: RLState -> [(String, String)]
describeState state =
    [ ("Quality L/S/R", show (qualityLeft state) ++ " / " ++ show (qualityStraight state) ++ " / " ++ show (qualityRight state))
    , ("Food forward", show (foodForward state))
    , ("Food sideways", show (foodSideways state))
    ]


-- | Creates detailed version-independent GUI diagnostics for a V3 agent.
--
-- In addition to the encoded state and Q-values, the debugger exposes the
-- underlying measurements used to construct each 'ActionQuality'. These raw
-- values are diagnostic only and are not additional fields of the learned
-- state.
v3DebugProvider :: QTable -> Debug.AgentDebugProvider
v3DebugProvider table gameState worm =
    Debug.AgentDebugInfo
        { Debug.debugStateLines =
            describeState state
                ++ [ ("Raw area L/S/R", showTriple analysisRawArea)
                   , ("Safe area L/S/R", showTriple analysisThreatAwareArea)
                   , ("Raw moves L/S/R", showTriple analysisRawNextMoves)
                   , ("Robust moves L/S/R", showTriple analysisRobustNextMoves)
                   , ("Length", show (length (wormBody worm)))
                   ]
        , Debug.debugQValues = Core.qValuesForState table state
        , Debug.debugBestActions = Core.bestQActions table state
        }
  where
    state = encodeState gameState worm
    leftAnalysis = analyzeAction gameState worm TurnLeft
    straightAnalysis = analyzeAction gameState worm GoStraight
    rightAnalysis = analyzeAction gameState worm TurnRight

    showTriple selector =
        show (selector leftAnalysis)
            ++ " / " ++ show (selector straightAnalysis)
            ++ " / " ++ show (selector rightAnalysis)