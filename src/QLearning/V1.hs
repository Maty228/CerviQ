module QLearning.V1
    ( Danger(..)
    , RelativeDirection(..)
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

-- | Binary danger signal used in the RL state.
data Danger
    = Safe
    | Dangerous
    deriving (Show, Read, Eq, Ord)


-- | Relative direction around the worm.
data RelativeDirection
    = RelLeft
    | RelStraight
    | RelRight
    deriving (Show, Read, Eq, Ord)


-- | Direction of the closest food relative to the worm.
data FoodDirection
    = FoodLeft
    | FoodRight
    | FoodAhead
    | FoodBehind
    | FoodSame
    deriving (Show, Read, Eq, Ord)


-- | Number of safe actions available from the current state.
data Mobility
    = NoMoves
    | OneMove
    | TwoMoves
    | ThreeMoves
    deriving (Show, Read, Eq, Ord)


-- | Coarse estimate of how much free space the worm has.
data SpaceLevel
    = Trapped
    | Tight
    | Open
    deriving (Show, Read, Eq, Ord)


-- | Compact representation of the game state for tabular Q-learning.
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
-- Conversion helpers
-- -----------------------------------------------------------------------------

-- | Converts a Boolean danger flag into a Danger value.
dangerFromBool :: Bool -> Danger
dangerFromBool True = Dangerous
dangerFromBool False = Safe


-- | Converts number of available safe moves into a coarse mobility category
mobilityFromCount :: Int -> Mobility
mobilityFromCount 0 = NoMoves
mobilityFromCount 1 = OneMove
mobilityFromCount 2 = TwoMoves
mobilityFromCount _ = ThreeMoves


-- | Converts an approximate reachable area into a coarse space category.
spaceFromArea :: Int -> SpaceLevel
spaceFromArea area
    | area <= 3 = Trapped
    | area <= 10 = Tight
    | otherwise = Open


-- -----------------------------------------------------------------------------
-- Food direction
-- -----------------------------------------------------------------------------

-- | Computes horizontal direction of the target relative to the worm.
horizontalFoodDirection :: Worm -> Position -> FoodDirection
horizontalFoodDirection worm (foodX, _) =
    let (headX, _) = wormHead worm
    in
        case compare foodX headX of
            LT -> FoodLeft
            EQ -> FoodSame
            GT -> FoodRight


-- | Computes vertical direction of the target relative to the worm.
verticalFoodDirection :: Worm -> Position -> FoodDirection
verticalFoodDirection worm (_, foodY) =
    let (_, headY) = wormHead worm
    in
        case compare foodY headY of
            LT -> FoodAhead
            EQ -> FoodSame
            GT -> FoodBehind



-- -----------------------------------------------------------------------------
-- Reachable area
-- -----------------------------------------------------------------------------

-- | Returns all four neighbouring positions of a map cell.
neighbours :: Position -> [Position]
neighbours (x, y) =
    [ 
        (x + 1, y),
        (x - 1, y),
        (x, y + 1),
        (x, y - 1)
    ]


-- | Returns True if the position blocks movement for reachable-area search.
--
-- The controlled worm's head is allowed as the starting position, but its body
-- and all other living worms still block movement.
blocksReachableArea :: GameState -> Worm -> Position -> Bool
blocksReachableArea state worm pos =
    isBlocked currentMap pos
        || isPoison currentMap pos
        || pos `Set.member` occupiedWithoutOwnHead
  where
    currentMap = gameMap state

    aliveWorms = filter wormAlive (gameWorms state)

    occupied = Set.fromList (occupiedPositions aliveWorms)

    occupiedWithoutOwnHead =
        Set.delete (wormHead worm) occupied


-- | Counts how many cells are reachable from the worm's current head position.
--
-- This is a coarse anti-trap signal: a low reachable area means the worm is
-- probably inside a narrow or closed region.
reachableArea :: GameState -> Worm -> Int
reachableArea state worm =
    floodFill Set.empty [wormHead worm]
  where
    floodFill :: Set.Set Position -> [Position] -> Int
    floodFill visited [] =
        Set.size visited

    floodFill visited (pos : rest)
        | pos `Set.member` visited =
            floodFill visited rest

        | blocksReachableArea state worm pos =
            floodFill visited rest

        | otherwise =
            let newVisited = Set.insert pos visited
                newFrontier = neighbours pos ++ rest
            in floodFill newVisited newFrontier

-- -----------------------------------------------------------------------------
-- State encoding
-- -----------------------------------------------------------------------------


-- | Encodes the current game state from the perspective of one worm.
encodeState :: GameState -> Worm -> RLState
encodeState state worm =
    let safe = safeActions state worm
        nearest = nearestFood state worm
    in
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


-- | Computes the distance from a worm to the nearest food.
distanceToNearestFood :: GameState -> Worm -> Maybe Int
distanceToNearestFood state worm =
    fmap (distance (wormHead worm)) (nearestFood state worm)


-- | Computes the shaping reward for moving closer to or farther from food.
foodDistanceReward :: GameState -> Worm -> GameState -> Worm -> Double
foodDistanceReward beforeState beforeWorm afterState afterWorm
    | wormFoodDelta beforeWorm afterWorm > 0 = 0

    | otherwise =
        case (distanceToNearestFood beforeState beforeWorm, distanceToNearestFood afterState afterWorm) of
            (Just beforeDistance, Just afterDistance)
                | afterDistance < beforeDistance -> 2
                | afterDistance > beforeDistance -> -2
                | otherwise -> 0

            _ -> 0


-- | Computes the reward obtained by the controlled worm during one step.
--
-- The reward is intentionally shaped:
-- * eating food is strongly positive,
-- * killing another worm is positive,
-- * dying is strongly negative,
-- * taking time has a small penalty,
-- * moving closer to food is slightly rewarded.
rewardForStep :: GameState -> Worm -> GameState -> Worm -> Double
rewardForStep beforeState beforeWorm afterState afterWorm =
    foodReward + killReward + deathPenalty + distanceReward + timePenalty
  where
    foodReward = 50 * fromIntegral (wormFoodDelta beforeWorm afterWorm)

    killReward = 10 * fromIntegral (wormKillDelta beforeWorm afterWorm)

    deathPenalty = if wormDied beforeWorm afterWorm then -100 else 0

    distanceReward = foodDistanceReward beforeState beforeWorm afterState afterWorm

    timePenalty = -1

-- -----------------------------------------------------------------------------
-- V1 Q-learning specification
-- -----------------------------------------------------------------------------

-- | Complete Q-learning specification of version 1.
v1Spec :: Core.QLearningSpec RLState
v1Spec =
    Core.QLearningSpec
        {
            Core.qlEncodeState = encodeState,
            Core.qlRewardForStep = rewardForStep
        }


-- | Q-table used by Q-learning version 1.
type QTable =
    Core.QTable RLState


-- | Result of one version-1 reinforcement-learning step.
type RlStep =
    Core.RlStep RLState


-- -----------------------------------------------------------------------------
-- V1 compatibility API
-- -----------------------------------------------------------------------------

-- | Finds a worm by its identifier.
controlledWorm :: Int -> GameState -> Maybe Worm
controlledWorm = Core.controlledWorm


-- | Returns the food-count difference between two worm states.
wormFoodDelta :: Worm -> Worm -> Int
wormFoodDelta = Core.wormFoodDelta


-- | Returns the kill-count difference between two worm states.
wormKillDelta :: Worm -> Worm -> Int
wormKillDelta = Core.wormKillDelta


-- | Returns True if the worm died during the transition.
wormDied :: Worm -> Worm -> Bool
wormDied = Core.wormDied




-- | Performs one V1 environment step without explicit opponent controllers.
stepEnvironment :: Int -> Action -> GameState -> IO RlStep
stepEnvironment = Core.stepEnvironment v1Spec


-- | Performs one V1 environment step with opponent controllers.
stepEnvironmentWithOpponents :: Int -> Action -> [(Int, Controller)] -> GameState -> IO RlStep
stepEnvironmentWithOpponents = Core.stepEnvironmentWithOpponents v1Spec


-- | Empty V1 Q-table.
emptyQTable :: QTable
emptyQTable = Core.emptyQTable


-- | Returns a V1 Q-value.
qValue :: QTable -> RLState -> Action -> Double
qValue = Core.qValue


-- | Returns all V1 action values for a state.
qValuesForState :: QTable -> RLState -> [(Action, Double)]
qValuesForState = Core.qValuesForState


-- | Returns all best V1 actions.
bestQActions :: QTable -> RLState -> [Action]
bestQActions = Core.bestQActions


-- | Chooses one best V1 action.
bestQAction :: QTable -> RLState -> IO Action
bestQAction = Core.bestQAction


-- | Chooses an action using the V1 Q-table and epsilon-greedy exploration.
chooseActionEpsilonGreedy :: Double -> QTable -> RLState -> IO Action
chooseActionEpsilonGreedy = Core.chooseActionEpsilonGreedy


-- | Updates one V1 Q-value.
updateQValue :: Double -> Double -> RLState -> Action -> Double -> Maybe RLState -> QTable -> QTable
updateQValue = Core.updateQValue


-- | Saves a version-1 Q-table.
saveQTable :: FilePath -> QTable -> IO ()
saveQTable = Core.saveQTable


-- | Loads a version-1 Q-table.
loadQTable :: FilePath -> IO QTable
loadQTable = Core.loadQTable


-- | Creates the greedy version-1 Q-learning agent.
qLearningAgent :: QTable -> Agent
qLearningAgent = Core.qLearningAgent v1Spec


-- -----------------------------------------------------------------------------
-- V1 debugging
-- -----------------------------------------------------------------------------

-- | Converts a version-1 RL state into human-readable debug values.
describeState :: RLState -> [(String, String)]
describeState state =
    [
        ( "Danger L/S/R", show (dangerLeft state) ++ " / " ++ show (dangerStraight state) ++ " / " ++ show (dangerRight state)),
        ( "Food", show (foodHorizontal state) ++ " / " ++ show (foodVertical state)),
        ( "Direction", show (currentDirection state)),
        ( "Mobility", show (mobilityLevel state)),
        ( "Space", show (spaceLevel state))
    ]


-- | Creates debugging information for a version-1 learned agent.
v1DebugProvider :: QTable -> Debug.AgentDebugProvider
v1DebugProvider = Debug.makeQDebugProvider v1Spec describeState