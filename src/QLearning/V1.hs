{-|
Module      : QLearning.V1
Description : First tabular Q-learning state and reward design for CerviQ.

Version 1 is the initial Q-learning baseline. Its state combines immediate
danger in the three possible actions, the nearest food position on absolute map
axes, the worm's current direction, the number of immediately safe actions,
and one coarse estimate of globally reachable space.

The generic Q-learning algorithm itself is implemented in "QLearning.Core".
This module defines only the V1-specific state representation, reward function,
persistence wrappers, learned agent, and debugging representation.
-}

module QLearning.V1
    ( Danger(..)
    , FoodDirection(..)
    , Mobility(..)
    , SpaceLevel(..)
    , RLState(..)

    , encodeState
    , rewardForStep

    , v1Spec
    , QTable
    , RlStep

    , controlledWorm
    , stepEnvironment
    , stepEnvironmentWithOpponents

    , emptyQTable
    , qValue
    , qValuesForState
    , bestQActions
    , bestQAction
    , chooseActionEpsilonGreedy
    , updateQValue

    , saveQTable
    , loadQTable
    , qLearningAgent

    , describeState
    , v1DebugProvider
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

-- | Binary immediate-danger signal used by Q-learning version 1.
data Danger
    = Safe
    | Dangerous
    deriving (Show, Read, Eq, Ord)



-- | Coarse direction of the nearest food on one absolute map axis.
--
-- V1 represents horizontal and vertical food direction separately using the
-- same type. The values are based on absolute map coordinates rather than on
-- the worm's orientation; 'currentDirection' is therefore also part of the
-- encoded state.
data FoodDirection
    = FoodLeft
    | FoodRight
    | FoodAhead
    | FoodBehind
    | FoodSame
    deriving (Show, Read, Eq, Ord)


-- | Number of immediately safe actions available to the worm.
data Mobility
    = NoMoves
    | OneMove
    | TwoMoves
    | ThreeMoves
    deriving (Show, Read, Eq, Ord)


-- | Coarse estimate of how much free space is reachable from the worm.
data SpaceLevel
    = Trapped
    | Tight
    | Open
    deriving (Show, Read, Eq, Ord)


-- | Version-1 state used as the key of the tabular Q-table.
--
-- The state combines local immediate danger with coarse global information.
-- Food location still uses absolute map axes, which was later replaced by a
-- worm-relative representation in V2.
data RLState = RLState
    {
        dangerLeft :: Danger,
        dangerStraight :: Danger,
        dangerRight :: Danger,
        foodHorizontal :: FoodDirection,
        foodVertical :: FoodDirection,
        currentDirection :: Direction,
        mobilityLevel :: Mobility,
        spaceLevel :: SpaceLevel
    }
    deriving (Show, Read, Eq, Ord)


-- -----------------------------------------------------------------------------
-- State conversion helpers
-- -----------------------------------------------------------------------------

-- | Converts a Boolean danger flag into the V1 danger category.
dangerFromBool :: Bool -> Danger
dangerFromBool True = Dangerous
dangerFromBool False = Safe


-- | Converts the number of safe actions into a coarse mobility category.
mobilityFromCount :: Int -> Mobility
mobilityFromCount 0 = NoMoves
mobilityFromCount 1 = OneMove
mobilityFromCount 2 = TwoMoves
mobilityFromCount _ = ThreeMoves


-- | Converts globally reachable area into the fixed V1 space categories.
spaceFromArea :: Int -> SpaceLevel
spaceFromArea area
    | area <= 3 = Trapped
    | area <= 10 = Tight
    | otherwise = Open


-- -----------------------------------------------------------------------------
-- Absolute food direction
-- -----------------------------------------------------------------------------

-- | Returns the nearest food's horizontal position on the absolute map axis.
horizontalFoodDirection :: Worm -> Position -> FoodDirection
horizontalFoodDirection worm (foodX, _) =
    case compare foodX headX of
        LT -> FoodLeft
        EQ -> FoodSame
        GT -> FoodRight
  where
    (headX, _) = wormHead worm


-- | Returns the nearest food's vertical position on the absolute map axis.
--
-- Smaller Y coordinates are above the worm and are represented as
-- 'FoodAhead'; larger Y coordinates are below and represented as 'FoodBehind'.
-- These names are historical V1 terminology and are not relative to the worm's
-- current orientation.
verticalFoodDirection :: Worm -> Position -> FoodDirection
verticalFoodDirection worm (_, foodY) =
    case compare foodY headY of
        LT -> FoodAhead
        EQ -> FoodSame
        GT -> FoodBehind
  where
    (_, headY) = wormHead worm


-- -----------------------------------------------------------------------------
-- Reachable space
-- -----------------------------------------------------------------------------

-- | Returns all four orthogonal neighbours of a map position.
neighbours :: Position -> [Position]
neighbours (x, y) =
    [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]


-- | Returns whether a position blocks the V1 reachable-area search.
--
-- The controlled worm's current head is removed from the occupied set so it
-- can serve as the flood-fill starting position. Its remaining body and all
-- other living worms remain obstacles.
blocksReachableArea :: GameState -> Worm -> Position -> Bool
blocksReachableArea state worm pos =
    isBlocked currentMap pos
        || isPoison currentMap pos
        || pos `Set.member` occupiedWithoutOwnHead
  where
    currentMap = gameMap state
    aliveWorms = filter wormAlive (gameWorms state)
    occupied = Set.fromList (occupiedPositions aliveWorms)
    occupiedWithoutOwnHead = Set.delete (wormHead worm) occupied


-- | Counts cells reachable from the worm's current head using flood fill.
--
-- V1 uses this as one global anti-trap signal. The resulting count is reduced
-- to 'Trapped', 'Tight', or 'Open' by 'spaceFromArea'.
reachableArea :: GameState -> Worm -> Int
reachableArea state worm =
    floodFill Set.empty [wormHead worm]
  where
    floodFill :: Set.Set Position -> [Position] -> Int
    floodFill visited [] = Set.size visited
    floodFill visited (pos : rest)
        | pos `Set.member` visited = floodFill visited rest
        | blocksReachableArea state worm pos = floodFill visited rest
        | otherwise =
            let newVisited = Set.insert pos visited
                newFrontier = neighbours pos ++ rest
            in floodFill newVisited newFrontier


-- -----------------------------------------------------------------------------
-- State encoding
-- -----------------------------------------------------------------------------

-- | Encodes the current game situation from one worm's V1 perspective.
encodeState :: GameState -> Worm -> RLState
encodeState state worm =
    RLState
        {
            dangerLeft = dangerFromBool (TurnLeft `notElem` safe),
            dangerStraight = dangerFromBool (GoStraight `notElem` safe),
            dangerRight = dangerFromBool (TurnRight `notElem` safe),
            foodHorizontal =
                case nearest of
                    Nothing -> FoodSame
                    Just food -> horizontalFoodDirection worm food,
            foodVertical =
                case nearest of
                    Nothing -> FoodSame
                    Just food -> verticalFoodDirection worm food,
            currentDirection = wormDirection worm,
            mobilityLevel = mobilityFromCount (length safe),
            spaceLevel = spaceFromArea (reachableArea state worm)
        }
  where
    safe = safeActions state worm
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
-- on a transition in which food was consumed.
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


-- | Computes the V1 reward for one game transition.
--
-- The shaped reward combines:
--
-- * +50 for each food item eaten,
-- * +10 for each kill,
-- * -100 for dying,
-- * +2/-2 for moving closer to/farther from the nearest food,
-- * -1 for every simulated tick.
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
-- V1 Q-learning specification
-- -----------------------------------------------------------------------------

-- | Complete version-specific specification passed to the shared Q-learning core.
v1Spec :: Core.QLearningSpec RLState
v1Spec =
    Core.QLearningSpec
        {
            Core.qlEncodeState = encodeState,
            Core.qlRewardForStep = rewardForStep
        }


-- | Q-table whose keys use the V1 state representation.
type QTable = Core.QTable RLState


-- | Result of one V1 reinforcement-learning environment step.
type RlStep = Core.RlStep RLState


-- -----------------------------------------------------------------------------
-- V1 core API
-- -----------------------------------------------------------------------------

-- | Finds the controlled worm by its identifier.
controlledWorm :: Int -> GameState -> Maybe Worm
controlledWorm = Core.controlledWorm


-- | Performs one V1 environment step without explicit opponent controllers.
stepEnvironment :: Int -> Action -> GameState -> IO RlStep
stepEnvironment = Core.stepEnvironment v1Spec


-- | Performs one V1 environment step while opponents use assigned controllers.
stepEnvironmentWithOpponents :: Int -> Action -> [(Int, Controller)] -> GameState -> IO RlStep
stepEnvironmentWithOpponents = Core.stepEnvironmentWithOpponents v1Spec


-- | Empty V1 Q-table.
emptyQTable :: QTable
emptyQTable = Core.emptyQTable


-- | Returns one V1 state-action value.
qValue :: QTable -> RLState -> Action -> Double
qValue = Core.qValue


-- | Returns all action values in a V1 state.
qValuesForState :: QTable -> RLState -> [(Action, Double)]
qValuesForState = Core.qValuesForState


-- | Returns all actions tied for the highest Q-value in a V1 state.
bestQActions :: QTable -> RLState -> [Action]
bestQActions = Core.bestQActions


-- | Randomly selects one action tied for the highest Q-value.
bestQAction :: QTable -> RLState -> IO Action
bestQAction = Core.bestQAction


-- | Selects a V1 action using epsilon-greedy exploration.
chooseActionEpsilonGreedy :: Double -> QTable -> RLState -> IO Action
chooseActionEpsilonGreedy = Core.chooseActionEpsilonGreedy


-- | Applies one Q-learning update to a V1 state-action pair.
updateQValue :: Double -> Double -> RLState -> Action -> Double -> Maybe RLState -> QTable -> QTable
updateQValue = Core.updateQValue


-- | Saves a V1 Q-table.
saveQTable :: FilePath -> QTable -> IO ()
saveQTable = Core.saveQTable


-- | Loads a V1 Q-table.
loadQTable :: FilePath -> IO QTable
loadQTable = Core.loadQTable


-- | Creates a greedy learned agent from a V1 Q-table.
qLearningAgent :: QTable -> Agent
qLearningAgent = Core.qLearningAgent v1Spec


-- -----------------------------------------------------------------------------
-- V1 debugging
-- -----------------------------------------------------------------------------

-- | Converts a V1 encoded state into human-readable diagnostic values.
describeState :: RLState -> [(String, String)]
describeState state =
    [ ("Danger L/S/R", show (dangerLeft state) ++ " / " ++ show (dangerStraight state) ++ " / " ++ show (dangerRight state))
    , ("Food", show (foodHorizontal state) ++ " / " ++ show (foodVertical state))
    , ("Direction", show (currentDirection state))
    , ("Mobility", show (mobilityLevel state))
    , ("Space", show (spaceLevel state))
    ]


-- | Creates version-independent GUI diagnostics for a V1 learned agent.
v1DebugProvider :: QTable -> Debug.AgentDebugProvider
v1DebugProvider =
    Debug.makeQDebugProvider v1Spec describeState