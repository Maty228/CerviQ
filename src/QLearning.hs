module QLearning where


import Agent
import Collision
import Config
import Controller
import Game
import Maps
import Movement
import Types

import qualified Data.Set as Set
import qualified Data.Map as Map
import System.Random (randomRIO)


-- -----------------------------------------------------------------------------
-- RL state representation
-- -----------------------------------------------------------------------------

-- | Binary danger signal used in the RL state.
data Danger
    = Safe
    | Dangerous
    deriving (Show, Eq, Ord)


-- | Relative direction around the worm.
data RelativeDirection
    = RelLeft
    | RelStraight
    | RelRight
    deriving (Show, Eq, Ord)


-- | Direction of the closest food relative to the worm.
data FoodDirection
    = FoodLeft
    | FoodRight
    | FoodAhead
    | FoodBehind
    | FoodSame
    deriving (Show, Eq, Ord)


-- | Number of safe actions available from the current state.
data Mobility
    = NoMoves
    | OneMove
    | TwoMoves
    | ThreeMoves
    deriving (Show, Eq, Ord)


-- | Coarse estimate of how much free space the worm has.
data SpaceLevel
    = Trapped
    | Tight
    | Open
    deriving (Show, Eq, Ord)


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
    deriving (Show, Eq, Ord)


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


-- -----------------------------------------------------------------------------
-- RL environment
-- -----------------------------------------------------------------------------

-- | Result of one reinforcement-learning environment step.
data RlStep = RlStep
    { 
        rlNextGameState :: GameState, 
        rlNextRlState :: Maybe RLState, 
        rlReward :: Double, 
        rlDone :: Bool
    }
    deriving (Show, Eq)


-- | Finds a worm by its Id in the given game state.
controlledWorm :: Int -> GameState -> Maybe Worm
controlledWorm targetId state =
    case filter ((== targetId) . wormId) (gameWorms state) of
        worm : _ -> Just worm
        [] -> Nothing


-- | Returns how many food items the worm gained between two states.
wormFoodDelta :: Worm -> Worm -> Int
wormFoodDelta before after =
    foodEaten (wormStats after) - foodEaten (wormStats before)


-- | Returns how many kills the worm gained between two states.
wormKillDelta :: Worm -> Worm -> Int
wormKillDelta before after =
    kills (wormStats after) - kills (wormStats before)


-- | Returns True if the worm was alive before the step and dead after it.
wormDied :: Worm -> Worm -> Bool
wormDied before after =
    wormAlive before && not (wormAlive after)


-- | Computes the distance from a worm to the nearest food.
distanceToNearestFood :: GameState -> Worm -> Maybe Int
distanceToNearestFood state worm =
    fmap (distance (wormHead worm)) (nearestFood state worm)


-- | Computes the shaping reward for moving closer to or farther from food.
foodDistanceReward :: GameState -> Worm -> GameState -> Worm -> Double
foodDistanceReward beforeState beforeWorm afterState afterWorm
    | wormFoodDelta beforeWorm afterWorm > 0 =
        0

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
    foodReward
        + killReward
        + deathPenalty
        + distanceReward
        + timePenalty
  where
    foodReward =
        50 * fromIntegral (wormFoodDelta beforeWorm afterWorm)

    killReward =
        10 * fromIntegral (wormKillDelta beforeWorm afterWorm)

    deathPenalty =
        if wormDied beforeWorm afterWorm then -100 else 0

    distanceReward =
        foodDistanceReward beforeState beforeWorm afterState afterWorm

    timePenalty =
        -1

-- | Performs one environment step for the controlled worm without opponent agents.
--
-- This is mainly useful for simple tests. Training should usually use
-- 'stepEnvironmentWithOpponents'.
stepEnvironment :: Int -> Action -> GameState -> IO RlStep
stepEnvironment controlledId action state =
    stepEnvironmentWithOpponents controlledId action [] state


-- | Performs one environment step for the controlled worm while other worms
-- are controlled by the provided controllers.
--
-- The controlled worm's action is supplied explicitly by the learning algorithm.
-- Opponent controllers are used only for the other worms.
stepEnvironmentWithOpponents :: Int -> Action -> [(Int, Controller)] -> GameState -> IO RlStep
stepEnvironmentWithOpponents controlledId action opponentControllers state =
    case controlledWorm controlledId state of
        Nothing ->
            pure
                RlStep
                    { 
                        rlNextGameState = state, 
                        rlNextRlState = Nothing, 
                        rlReward = -100, 
                        rlDone = True
                    }
        Just beforeWorm -> do
            opponentActions <- collectActions opponentControllers state
            let filteredOpponentActions =
                    filter
                        (\(wormId', _) -> wormId' /= controlledId) opponentActions
                actions = (controlledId, action) : filteredOpponentActions

                steppedGameState = stepGame actions state

            nextGameState <- maintainFoodCount maxFoodCount steppedGameState

            let maybeAfterWorm = controlledWorm controlledId nextGameState

            case maybeAfterWorm of
                Nothing ->
                    pure
                        RlStep
                            { 
                                rlNextGameState = nextGameState, 
                                rlNextRlState = Nothing, 
                                rlReward = -100, 
                                rlDone = True
                            }
                Just afterWorm ->
                    pure
                        RlStep
                            {   rlNextGameState = nextGameState, 
                                rlNextRlState =
                                    if wormAlive afterWorm
                                        then Just (encodeState nextGameState afterWorm)
                                        else Nothing, 
                                rlReward = rewardForStep state beforeWorm nextGameState afterWorm, 
                                rlDone = not (wormAlive afterWorm)
                            }


-- -----------------------------------------------------------------------------
-- Q-table
-- -----------------------------------------------------------------------------

-- | Maps every observed state-action pair to its learned Q-value.
type QTable = Map.Map (RLState, Action) Double


-- | An empty Q-table containing no learned values.
emptyQTable :: QTable
emptyQTable = Map.empty


-- | Returns the Q-value of the given state-action pair.
--
-- Unseen pairs have the initial value zero.
qValue :: QTable -> RLState -> Action -> Double
qValue table state action =
    Map.findWithDefault 0 (state, action) table

-- | Stores a Q-value for the given state-action pair.
setQValue :: RLState -> Action -> Double -> QTable -> QTable
setQValue state action value table =
    Map.insert (state, action) value table

-- | Returns all actions together with their Q-values in the given state.
qValuesForState :: QTable -> RLState -> [(Action, Double)]
qValuesForState table state =
    [ (action, qValue table state action)
    | action <- allActions
    ]

-- | Returns all actions with the highest Q-value in the given state.
bestQActions :: QTable -> RLState -> [Action]
bestQActions table state =
    let actionValues = qValuesForState table state
        bestValue = maximum (map snd actionValues)
    in
        [ action
        | (action, value) <- actionValues
        , value == bestValue
        ]

-- | Chooses one of the actions with the highest Q-value.
--
-- Ties are resolved randomly to avoid always preferring the first constructor.
bestQAction :: QTable -> RLState -> IO Action
bestQAction table state =
    randomChoice (bestQActions table state)

-- | Chooses an action using the epsilon-greedy exploration strategy.
--
-- With probability epsilon, a random action is selected.
-- Otherwise, the action with the highest known Q-value is selected.
chooseActionEpsilonGreedy :: Double -> QTable -> RLState -> IO Action
chooseActionEpsilonGreedy epsilon table state = do
    randomValue <- randomRIO (0.0, 1.0)

    if randomValue < epsilon
        then randomChoice allActions
        else bestQAction table state

-- | Returns the highest Q-value available in the given state.
maxQValue :: QTable -> RLState -> Double
maxQValue table state =
    maximum
        [ qValue table state action
        | action <- allActions
        ]

-- | Updates one Q-value using the Q-learning update rule.
--
-- Alpha is the learning rate and gamma is the discount factor.
-- A missing next state represents a terminal transition.
updateQValue :: Double -> Double -> RLState -> Action -> Double -> Maybe RLState -> QTable -> QTable
updateQValue alpha gamma state action reward nextState table =
    let oldValue =
            qValue table state action

        bestFutureValue =
            case nextState of
                Nothing -> 0
                Just next -> maxQValue table next

        target =
            reward + gamma * bestFutureValue

        newValue =
            oldValue + alpha * (target - oldValue)
    in
        setQValue state action newValue table