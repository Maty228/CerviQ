module QLearning.Core
    ( StateEncoder
    , RewardFunction
    , QLearningSpec(..)

    , RlStep
    , rlNextGameState
    , rlNextRlState
    , rlReward
    , rlDone

    , controlledWorm
    , wormFoodDelta
    , wormKillDelta
    , wormDied

    , stepEnvironment
    , stepEnvironmentWithOpponents

    , QTable
    , emptyQTable
    , qValue
    , setQValue
    , qValuesForState
    , bestQActions
    , bestQAction
    , maxQValue
    , chooseActionEpsilonGreedy
    , updateQValue

    , saveQTable
    , loadQTable
    , qLearningAgent
    ) where

import Agent (allActions, randomChoice)
import Config (maxFoodCount)
import Controller (collectActions)
import Game (maintainFoodCount, stepGame)
import Types

import qualified Data.Map as Map

import System.Random (randomRIO)
import Text.Read (readMaybe)


-- -----------------------------------------------------------------------------
-- Q-learning specification
-- -----------------------------------------------------------------------------

-- | Converts a game state into a version-specific reinforcement-learning state.
type StateEncoder state = GameState -> Worm -> state


-- | Computes the reward obtained by one worm during a game transition.
type RewardFunction = GameState -> Worm -> GameState -> Worm -> Double


-- | Defines the version-specific parts of a Q-learning agent.
--
-- Different Q-learning versions can use different state representations and
-- reward functions while sharing the same learning algorithm.
data QLearningSpec state = QLearningSpec
    {
        qlEncodeState :: StateEncoder state,
        qlRewardForStep :: RewardFunction
    }


-- -----------------------------------------------------------------------------
-- RL environment
-- -----------------------------------------------------------------------------

-- | Result of one reinforcement-learning environment step.
data RlStep state = RlStep
    {
        rlNextGameState :: GameState,
        rlNextRlState :: Maybe state,
        rlReward :: Double,
        rlDone :: Bool
    }
    deriving (Show, Eq)


-- | Finds a worm by its identifier in the given game state.
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


-- | Returns True if the worm was alive before the step and dead afterwards.
wormDied :: Worm -> Worm -> Bool
wormDied before after =
    wormAlive before && not (wormAlive after)


-- | Performs one environment step without explicitly controlled opponents.
stepEnvironment :: QLearningSpec state -> Int -> Action -> GameState -> IO (RlStep state)
stepEnvironment spec controlledId action state =
    stepEnvironmentWithOpponents spec controlledId action [] state


-- | Performs one environment step while other worms use their controllers.
stepEnvironmentWithOpponents :: QLearningSpec state -> Int -> Action -> [(Int, Controller)] -> GameState -> IO (RlStep state)
stepEnvironmentWithOpponents spec controlledId action opponentControllers state =
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
                            (\(wormId', _) -> wormId' /= controlledId)
                            opponentActions

                    actions = (controlledId, action) : filteredOpponentActions

                    steppedGameState = stepGame actions state

                nextGameState <-
                    maintainFoodCount maxFoodCount steppedGameState

                case controlledWorm controlledId nextGameState of
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
                                {
                                    rlNextGameState = nextGameState,
                                    rlNextRlState =
                                        if wormAlive afterWorm
                                            then
                                                Just
                                                    ( qlEncodeState spec
                                                        nextGameState
                                                        afterWorm
                                                    )
                                            else Nothing,
                                    rlReward = qlRewardForStep spec state beforeWorm nextGameState afterWorm,
                                    rlDone = not (wormAlive afterWorm)
                                }


-- -----------------------------------------------------------------------------
-- Q-table
-- -----------------------------------------------------------------------------

-- | Maps state-action pairs to learned Q-values.
type QTable state = Map.Map (state, Action) Double


-- | Empty Q-table containing no learned information.
emptyQTable :: QTable state
emptyQTable = Map.empty


-- | Returns the Q-value of a state-action pair.
--
-- Unseen pairs have value zero.
qValue :: Ord state => QTable state -> state -> Action -> Double
qValue table state action = Map.findWithDefault 0 (state, action) table


-- | Stores a Q-value for a state-action pair.
setQValue :: Ord state => state -> Action -> Double -> QTable state -> QTable state
setQValue state action value table = Map.insert (state, action) value table


-- | Returns all actions and their Q-values for the given state.
qValuesForState :: Ord state => QTable state -> state -> [(Action, Double)]
qValuesForState table state =
    [
        (action, qValue table state action)
        | action <- allActions
    ]


-- | Returns all actions having the highest Q-value in the given state.
bestQActions :: Ord state => QTable state -> state -> [Action]
bestQActions table state =
    let actionValues = qValuesForState table state
        bestValue = maximum (map snd actionValues)
    in
        [
            action | (action, value) <- actionValues,
            value == bestValue
        ]


-- | Randomly chooses one action having the highest Q-value.
bestQAction :: Ord state => QTable state -> state -> IO Action
bestQAction table state = randomChoice (bestQActions table state)


-- | Chooses an action using epsilon-greedy exploration.
chooseActionEpsilonGreedy :: Ord state => Double -> QTable state -> state -> IO Action
chooseActionEpsilonGreedy epsilon table state = do
    randomValue <- randomRIO (0.0, 1.0)

    if randomValue < epsilon
        then randomChoice allActions
        else bestQAction table state


-- | Returns the highest Q-value available from the given state.
maxQValue :: Ord state => QTable state -> state -> Double
maxQValue table state = maximum [qValue table state action | action <- allActions]


-- | Updates one state-action pair using the Q-learning update rule.
updateQValue :: Ord state => Double -> Double -> state -> Action -> Double -> Maybe state -> QTable state -> QTable state
updateQValue alpha gamma state action reward nextState table =
        let oldValue = qValue table state action
            bestFutureValue =
                case nextState of
                    Nothing ->
                        0
                    Just next ->
                        maxQValue table next

            target = reward + gamma * bestFutureValue
            newValue = oldValue + alpha * (target - oldValue)
        in
            setQValue state action newValue table


-- -----------------------------------------------------------------------------
-- Persistence
-- -----------------------------------------------------------------------------

-- | Saves a Q-table to a text file.
saveQTable :: Show state => FilePath -> QTable state -> IO ()
saveQTable filePath qTable = writeFile filePath (show qTable)


-- | Loads a Q-table from a text file.
loadQTable :: (Read state, Ord state) => FilePath -> IO (QTable state)
loadQTable filePath = do
    contents <- readFile filePath

    case readMaybe contents of
        Just qTable -> pure qTable
        Nothing ->
            fail ( "Could not parse Q-table from file: " ++ filePath)


-- -----------------------------------------------------------------------------
-- Learned policy
-- -----------------------------------------------------------------------------

-- | Creates a greedy agent from a Q-learning specification and learned Q-table.
qLearningAgent :: Ord state => QLearningSpec state -> QTable state -> Agent
qLearningAgent spec table state worm =
    bestQAction table (qlEncodeState spec state worm)