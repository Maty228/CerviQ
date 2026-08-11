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
-- The quality combines immediate collision safety, possible opponent movement,
-- future manoeuvrability, and reachable space.
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


-- | Forward position of the nearest food relative to the worm's orientation.
data ForwardFoodDirection
    = FoodAhead
    | FoodSameForward
    | FoodBehind
    deriving (Show, Read, Eq, Ord)


-- | Side position of the nearest food relative to the worm's orientation.
data SideFoodDirection
    = FoodLeft
    | FoodSameSide
    | FoodRight
    deriving (Show, Read, Eq, Ord)


-- | Version-3 RL state.
--
-- Each candidate action is represented by one compact quality value combining
-- immediate safety, reachable space, manoeuvrability, and opponent threat.
data RLState = RLState
    {
        qualityLeft :: ActionQuality,
        qualityStraight :: ActionQuality,
        qualityRight :: ActionQuality,
        foodForward :: ForwardFoodDirection,
        foodSideways :: SideFoodDirection
    }
    deriving (Show, Read, Eq, Ord)


-- -----------------------------------------------------------------------------
-- Relative food direction
-- -----------------------------------------------------------------------------

-- | Returns the food displacement in coordinates relative to worm orientation.
--
-- The first component is positive in front of the worm.
-- The second component is positive to the worm's right.
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


-- | Computes the forward relation of food to the worm.
forwardFoodDirection :: Worm -> Position -> ForwardFoodDirection
forwardFoodDirection worm food =
    case compare forwardDelta 0 of
        GT -> FoodAhead
        EQ -> FoodSameForward
        LT -> FoodBehind
  where
    (forwardDelta, _) = relativeFoodDeltas worm food


-- | Computes the sideways relation of food to the worm.
sideFoodDirection :: Worm -> Position -> SideFoodDirection
sideFoodDirection worm food =
    case compare sideDelta 0 of
        LT -> FoodLeft
        EQ -> FoodSameSide
        GT -> FoodRight
  where
    (_, sideDelta) = relativeFoodDeltas worm food


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
-- Opponent threat prediction
-- -----------------------------------------------------------------------------

-- | Returns all cells that living opponents could safely move their heads into
-- during the current simultaneous game step.
possibleEnemyNextHeads :: GameState -> Worm -> Set.Set Position
possibleEnemyNextHeads state controlled =
    Set.fromList
        [
            headAfterAction enemy action
            | enemy <- gameWorms state, wormAlive enemy,
            wormId enemy /= wormId controlled, action <- safeActions state enemy
        ]

-- | Returns True if a cell blocks ordinary reachable-area exploration.
blocksReachableArea
    :: GameState
    -> Worm
    -> Position
    -> Bool
blocksReachableArea state worm position =
    isBlocked currentMap position
        || isPoison currentMap position
        || position `Set.member` occupiedWithoutOwnHead
  where
    currentMap =
        gameMap state

    aliveWorms =
        filter wormAlive (gameWorms state)

    occupied =
        Set.fromList
            (occupiedPositions aliveWorms)

    occupiedWithoutOwnHead =
        Set.delete
            (wormHead worm)
            occupied

-- | Counts ordinary cells reachable from the worm's current head.
reachableArea :: GameState -> Worm -> Int
reachableArea state worm =
    floodFill Set.empty [wormHead worm]
  where
    floodFill visited [] = Set.size visited

    floodFill visited (position : rest)
        | position `Set.member` visited = floodFill visited rest
        | blocksReachableArea state worm position = floodFill visited rest
        | otherwise = floodFill (Set.insert position visited) (neighbours position ++ rest)

-- | Returns True if a cell blocks threat-aware reachable-area exploration.
--
-- In addition to ordinary obstacles, cells that an opponent may occupy on the
-- current simultaneous turn are treated as unavailable.
blocksThreatAwareArea :: Set.Set Position -> GameState -> Worm -> Position -> Bool
blocksThreatAwareArea threats state worm position = blocksReachableArea state worm position || position `Set.member` threats


-- | Counts cells reachable while treating possible enemy head positions as
-- blocked.
threatAwareReachableArea :: Set.Set Position -> GameState -> Worm -> Int
threatAwareReachableArea threats state worm = floodFill Set.empty [wormHead worm]
  where
    floodFill visited [] = Set.size visited

    floodFill visited (position : rest)
        | position `Set.member` visited = floodFill visited rest
        | blocksThreatAwareArea threats state worm position = floodFill visited rest
        | otherwise = floodFill (Set.insert position visited)(neighbours position ++ rest)

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
        analysisRobustNextMoves :: Int
    }

-- | Converts reachable area into an action quality.
--
-- A forced action has exactly one threat-aware continuation available.
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
    wormLength = length (wormBody worm)

-- | Returns ordinary safe actions available after the candidate action.
rawNextActions :: GameState -> Worm -> [Action]
rawNextActions state worm = safeActions state worm

-- | Filters future actions whose destination may be occupied by an opponent.
robustNextActions :: Set.Set Position -> GameState -> Worm -> [Action]
robustNextActions threats state worm =
    [
        action
        | action <- safeActions state worm,
        headAfterAction worm action `Set.notMember` threats
    ]

-- | Analyses one candidate action and assigns its version-3 quality.
analyzeAction :: GameState -> Worm -> Action -> ActionAnalysis
analyzeAction state worm action
    | action `notElem` currentSafeActions =
        ActionAnalysis
            {
                analysisQuality = Fatal,
                analysisRawArea = 0,
                analysisThreatAwareArea = 0,
                analysisRawNextMoves = 0,
                analysisRobustNextMoves = 0
            }

    | nextHead `Set.member` enemyThreats =
        ActionAnalysis
            {
                analysisQuality = Contested,
                analysisRawArea = 0,
                analysisThreatAwareArea = 0,
                analysisRawNextMoves = 0,
                analysisRobustNextMoves = 0
            }

    | rawMoveCount == 0 = analysisWithQuality DeadEnd
    | robustMoveCount == 0 = analysisWithQuality Threatened
    | otherwise =
        analysisWithQuality
            ( qualityFromArea
                (robustMoveCount == 1)
                movedWorm
                safeArea
            )

  where
    currentSafeActions = safeActions state worm
    enemyThreats = possibleEnemyNextHeads state worm
    nextHead = headAfterAction worm action
    movedWorm = wormAfterAction state worm action
    hypotheticalState = state { gameWorms = replaceWorm movedWorm (gameWorms state) }

    rawArea = reachableArea hypotheticalState movedWorm

    safeArea = threatAwareReachableArea enemyThreats hypotheticalState movedWorm

    rawMoveCount =
        length
            ( rawNextActions
                hypotheticalState
                movedWorm
            )

    robustMoveCount =
        length
            ( robustNextActions
                enemyThreats
                hypotheticalState
                movedWorm
            )

    analysisWithQuality quality =
        ActionAnalysis
            {
                analysisQuality = quality,
                analysisRawArea = rawArea,
                analysisThreatAwareArea = safeArea,
                analysisRawNextMoves = rawMoveCount,
                analysisRobustNextMoves = robustMoveCount
            }

-- -----------------------------------------------------------------------------
-- State encoding
-- -----------------------------------------------------------------------------

-- | Encodes the game using version-3 action qualities.
encodeState :: GameState -> Worm -> RLState
encodeState state worm =
    RLState
        {
            qualityLeft = analysisQuality (analyzeAction state worm TurnLeft),

            qualityStraight = analysisQuality (analyzeAction state worm GoStraight),

            qualityRight = analysisQuality (analyzeAction state worm TurnRight),

            foodForward =
                case nearest of
                    Nothing -> FoodSameForward
                    Just food -> forwardFoodDirection worm food,

            foodSideways =
                case nearest of
                    Nothing -> FoodSameSide
                    Just food -> sideFoodDirection worm food
        }
  where
    nearest = nearestFood state worm

-- -----------------------------------------------------------------------------
-- Reward
-- -----------------------------------------------------------------------------

-- | Computes the distance from the worm to the nearest food.
distanceToNearestFood :: GameState -> Worm -> Maybe Int
distanceToNearestFood state worm =
    fmap (distance (wormHead worm)) (nearestFood state worm)


-- | Computes shaping reward for moving closer to or farther from food.
foodDistanceReward :: GameState -> Worm -> GameState -> Worm -> Double
foodDistanceReward beforeState beforeWorm afterState afterWorm
    | Core.wormFoodDelta beforeWorm afterWorm > 0 = 0
    | otherwise =
        case
            (
                distanceToNearestFood beforeState beforeWorm,
                distanceToNearestFood afterState afterWorm
            )
        of
            (Just beforeDistance, Just afterDistance)
                | afterDistance < beforeDistance -> 2
                | afterDistance > beforeDistance -> -2
                | otherwise -> 0
            _ -> 0


-- | Computes the version-3 reward for one transition.
--
-- Version 3 intentionally keeps exactly the same reward as version 2 so that
-- evaluation isolates the effect of the improved state representation.
rewardForStep :: GameState -> Worm -> GameState -> Worm -> Double
rewardForStep beforeState beforeWorm afterState afterWorm =
    foodReward + killReward + deathPenalty + distanceReward + timePenalty
  where
    foodReward = 50 * fromIntegral (Core.wormFoodDelta beforeWorm afterWorm)

    killReward = 10 * fromIntegral (Core.wormKillDelta beforeWorm afterWorm)

    deathPenalty =
        if Core.wormDied beforeWorm afterWorm
            then -100
            else 0

    distanceReward = foodDistanceReward beforeState beforeWorm afterState afterWorm

    timePenalty = -1

-- | Returns True if an action quality is acceptable for normal action choice.
isAcceptableQuality :: ActionQuality -> Bool
isAcceptableQuality Fatal = False
isAcceptableQuality Contested = False
isAcceptableQuality DeadEnd = False
isAcceptableQuality _ = True

-- -----------------------------------------------------------------------------
-- V3 Q-learning specification
-- -----------------------------------------------------------------------------

-- | Complete Q-learning specification of version 3.
v3Spec :: Core.QLearningSpec RLState
v3Spec =
    Core.QLearningSpec
        {
            Core.qlEncodeState = encodeState,
            Core.qlRewardForStep = rewardForStep
        }


-- | Q-table used by Q-learning version 3.
type QTable = Core.QTable RLState


-- | Saves a version-3 Q-table.
saveQTable :: FilePath -> QTable -> IO ()
saveQTable = Core.saveQTable


-- | Loads a version-3 Q-table.
loadQTable :: FilePath -> IO QTable
loadQTable = Core.loadQTable


-- | Creates the greedy version-3 learned agent.
qLearningAgent :: QTable -> Agent
qLearningAgent = Core.qLearningAgent v3Spec


-- -----------------------------------------------------------------------------
-- V3 Agent with fallback for dangerous actions
-- -----------------------------------------------------------------------------


-- | Assigns a fallback preference to an action quality.
--
-- Higher values represent safer and more desirable action qualities.
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


-- | Returns the encoded action quality corresponding to an action.
qualityForAction :: RLState -> Action -> ActionQuality
qualityForAction state TurnLeft =
    qualityLeft state

qualityForAction state GoStraight =
    qualityStraight state

qualityForAction state TurnRight =
    qualityRight state

-- | Chooses the safest action according to version-3 state information.
fallbackAction :: RLState -> IO Action
fallbackAction state =
    randomChoice bestActions
  where
    actionScores =
        [
            (action, qualityScore (qualityForAction state action))
            | action <- allActions
        ]

    bestScore =
        maximum (map snd actionScores)

    bestActions =
        [
            action
            | (action, score) <- actionScores,
              score == bestScore
        ]

-- | Creates a V3 agent using learned Q-values with an action-quality fallback.
qLearningAgentWithFallback :: QTable -> Agent
qLearningAgentWithFallback table gameState worm =
    case learnedAcceptableActions of
        [] ->
            fallbackAction state

        actions ->
            chooseBestLearnedAction actions
  where
    state =
        encodeState gameState worm

    acceptableActions =
        [
            action
            | action <- allActions,
              isAcceptableQuality (qualityForAction state action)
        ]

    candidateActions =
        if null acceptableActions
            then allActions
            else acceptableActions

    learnedAcceptableActions =
        [
            action
            | action <- candidateActions,
              Core.hasQValue table state action
        ]

    chooseBestLearnedAction actions =
        randomChoice bestActions
      where
        actionValues =
            [
                (action, Core.qValue table state action)
                | action <- actions
            ]

        bestValue =
            maximum (map snd actionValues)

        bestActions =
            [
                action
                | (action, value) <- actionValues,
                  value == bestValue
            ]

-- -----------------------------------------------------------------------------
-- V3 debugging
-- -----------------------------------------------------------------------------

-- | Converts the encoded version-3 state into human-readable values.
describeState :: RLState -> [(String, String)]
describeState state =
    [
        ( "Quality L/S/R", show (qualityLeft state) ++ " / " ++ show (qualityStraight state) ++ " / " ++ show (qualityRight state) ),
        ( "Food forward", show (foodForward state) ),
        ( "Food sideways", show (foodSideways state) )
    ]

-- | Creates detailed debugging information for a version-3 learned agent.
v3DebugProvider :: QTable -> Debug.AgentDebugProvider
v3DebugProvider table gameState worm =
    let
        state = encodeState gameState worm
        leftAnalysis = analyzeAction gameState worm TurnLeft
        straightAnalysis = analyzeAction gameState worm GoStraight
        rightAnalysis = analyzeAction gameState worm TurnRight
        showTriple selector = show (selector leftAnalysis) ++ " / " ++ show (selector straightAnalysis) ++ " / " ++ show (selector rightAnalysis)

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
                        ( "Length", show (length (wormBody worm)) )
                    ],

                Debug.debugQValues = Core.qValuesForState table state,
                Debug.debugBestActions = Core.bestQActions table state
            }