{-|
Module      : QLearning.Core
Description : Version-independent Q-learning algorithm and RL environment helpers.

This module contains the reusable reinforcement-learning infrastructure shared
by all CerviQ Q-learning versions. Version-specific modules provide a state
encoder and reward function through 'QLearningSpec', while this module handles
environment stepping, epsilon-greedy action selection, Q-table updates,
persistence, and construction of learned agents.

Keeping the learning algorithm independent of a particular state
representation makes it possible to compare V1, V2, V3, and V4 while reusing
the same core implementation.
-}

module QLearning.Core
    ( StateEncoder
    , RewardFunction
    , QLearningSpec(..)

    , RlStep(..)
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
    , hasQValue

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

-- | Converts the complete game state and controlled worm into a
-- version-specific reinforcement-learning state.
type StateEncoder state = GameState -> Worm -> state


-- | Computes the reward obtained by one worm during a game transition.
type RewardFunction = GameState -> Worm -> GameState -> Worm -> Double


-- | Defines the parts of Q-learning that differ between individual versions.
--
-- Each version supplies its own state representation and reward function while
-- the learning algorithm itself remains in this module.
data QLearningSpec state = QLearningSpec
    {
        -- | Encodes the current game situation into the version-specific state.
        qlEncodeState :: StateEncoder state,

        -- | Evaluates one transition from the controlled worm's perspective.
        qlRewardForStep :: RewardFunction
    }


-- -----------------------------------------------------------------------------
-- RL environment
-- -----------------------------------------------------------------------------

-- | Result of one reinforcement-learning environment step.
--
-- 'rlNextRlState' is 'Nothing' for a terminal transition in which the
-- controlled worm died. The complete resulting 'GameState' is retained even
-- for terminal steps so statistics and diagnostics can still inspect it.
data RlStep state = RlStep
    {
        -- | Complete game state after performing the step.
        rlNextGameState :: GameState,

        -- | Encoded next state, or 'Nothing' when the episode terminated.
        rlNextRlState :: Maybe state,

        -- | Reward obtained by the controlled worm during the transition.
        rlReward :: Double,

        -- | Whether the controlled worm's episode has ended.
        rlDone :: Bool
    }
    deriving (Show, Eq)


-- | Finds a worm by its identifier, regardless of whether it is still alive.
--
-- Dead worms deliberately remain accessible because the reward function needs
-- their final statistics and alive status after a fatal transition.
controlledWorm :: Int -> GameState -> Maybe Worm
controlledWorm targetId state =
    case filter ((== targetId) . wormId) (gameWorms state) of
        worm : _ -> Just worm
        [] -> Nothing


-- | Returns how many food items a worm gained between two states.
wormFoodDelta :: Worm -> Worm -> Int
wormFoodDelta before after =
    foodEaten (wormStats after) - foodEaten (wormStats before)


-- | Returns how many kills a worm gained between two states.
wormKillDelta :: Worm -> Worm -> Int
wormKillDelta before after =
    kills (wormStats after) - kills (wormStats before)


-- | Returns whether a worm was alive before a transition and dead afterwards.
wormDied :: Worm -> Worm -> Bool
wormDied before after =
    wormAlive before && not (wormAlive after)


-- | Performs one environment step without explicitly controlled opponents.
stepEnvironment :: QLearningSpec state -> Int -> Action -> GameState -> IO (RlStep state)
stepEnvironment spec controlledId action state =
    stepEnvironmentWithOpponents spec controlledId action [] state


-- | Performs one environment step while other worms use assigned controllers.
--
-- One RL step:
--
-- 1. obtains the opponents' actions from their controllers;
-- 2. combines them with the controlled worm's selected action;
-- 3. advances the normal game engine by one simultaneous tick;
-- 4. restores the configured food count;
-- 5. computes reward, termination, and the encoded next state.
--
-- Any opponent assignment for the controlled worm itself is ignored so the
-- explicitly supplied learning action always takes precedence.
stepEnvironmentWithOpponents :: QLearningSpec state -> Int -> Action -> [(Int, Controller)] -> GameState -> IO (RlStep state)
stepEnvironmentWithOpponents spec controlledId action opponentControllers state =
    case controlledWorm controlledId state of
        Nothing ->
            pure
                RlStep
                    { rlNextGameState = state
                    , rlNextRlState = Nothing
                    , rlReward = -100
                    , rlDone = True
                    }

        Just beforeWorm -> do
            opponentActions <- collectActions opponentControllers state

            let filteredOpponentActions =
                    filter ((/= controlledId) . fst) opponentActions

                actions =
                    (controlledId, action) : filteredOpponentActions

                steppedState =
                    stepGame actions state

            nextGameState <- maintainFoodCount maxFoodCount steppedState

            case controlledWorm controlledId nextGameState of
                Nothing ->
                    pure
                        RlStep
                            { rlNextGameState = nextGameState
                            , rlNextRlState = Nothing
                            , rlReward = -100
                            , rlDone = True
                            }

                Just afterWorm ->
                    pure
                        RlStep
                            { rlNextGameState = nextGameState
                            , rlNextRlState =
                                if wormAlive afterWorm
                                    then Just (qlEncodeState spec nextGameState afterWorm)
                                    else Nothing
                            , rlReward = qlRewardForStep spec state beforeWorm nextGameState afterWorm
                            , rlDone = not (wormAlive afterWorm)
                            }


-- -----------------------------------------------------------------------------
-- Q-table
-- -----------------------------------------------------------------------------

-- | Table mapping encoded state-action pairs to learned expected returns.
type QTable state = Map.Map (state, Action) Double


-- | Empty Q-table containing no learned state-action values.
emptyQTable :: QTable state
emptyQTable = Map.empty


-- | Returns the Q-value of a state-action pair.
--
-- Unseen pairs are treated numerically as having value zero. Use 'hasQValue'
-- when it is necessary to distinguish an unseen pair from one explicitly
-- learned to have value zero.
qValue :: Ord state => QTable state -> state -> Action -> Double
qValue table state action =
    Map.findWithDefault 0 (state, action) table


-- | Stores a Q-value for one state-action pair.
setQValue :: Ord state => state -> Action -> Double -> QTable state -> QTable state
setQValue state action value table =
    Map.insert (state, action) value table


-- | Returns every available action together with its Q-value in a state.
qValuesForState :: Ord state => QTable state -> state -> [(Action, Double)]
qValuesForState table state =
    [(action, qValue table state action) | action <- allActions]


-- | Returns all actions tied for the highest Q-value in a state.
bestQActions :: Ord state => QTable state -> state -> [Action]
bestQActions table state =
    [action | (action, value) <- actionValues, value == bestValue]
  where
    actionValues = qValuesForState table state
    bestValue = maximum (map snd actionValues)


-- | Randomly chooses one of the actions tied for the highest Q-value.
--
-- Random tie-breaking avoids introducing an artificial preference for
-- 'TurnLeft', 'GoStraight', or 'TurnRight'. In a completely unseen state all
-- three values are zero, so the greedy policy also chooses randomly.
bestQAction :: Ord state => QTable state -> state -> IO Action
bestQAction table state =
    randomChoice (bestQActions table state)


-- | Returns the highest Q-value available from a state.
maxQValue :: Ord state => QTable state -> state -> Double
maxQValue table state =
    maximum [qValue table state action | action <- allActions]


-- | Chooses an action using epsilon-greedy exploration.
--
-- With probability epsilon a uniformly random action is chosen. Otherwise the
-- agent follows one of the actions with maximal current Q-value.
chooseActionEpsilonGreedy :: Ord state => Double -> QTable state -> state -> IO Action
chooseActionEpsilonGreedy epsilon table state = do
    randomValue <- randomRIO (0.0, 1.0)

    if randomValue < epsilon
        then randomChoice allActions
        else bestQAction table state


-- | Updates one state-action pair using the standard Q-learning update rule.
--
-- The update is:
--
-- @
-- Q(s,a) <- Q(s,a) + alpha * (r + gamma * max Q(s',a') - Q(s,a))
-- @
--
-- For terminal transitions 'nextState' is 'Nothing', so the future value is
-- zero and the update depends only on the observed terminal reward.
updateQValue :: Ord state => Double -> Double -> state -> Action -> Double -> Maybe state -> QTable state -> QTable state
updateQValue alpha gamma state action reward nextState table =
    setQValue state action newValue table
  where
    oldValue = qValue table state action

    bestFutureValue =
        case nextState of
            Nothing -> 0
            Just next -> maxQValue table next

    target = reward + gamma * bestFutureValue
    newValue = oldValue + alpha * (target - oldValue)


-- | Returns whether a state-action pair is explicitly present in the Q-table.
--
-- This differs from checking 'qValue', because both unseen pairs and pairs
-- learned to have value zero return the numeric value @0@.
hasQValue :: Ord state => QTable state -> state -> Action -> Bool
hasQValue table state action =
    Map.member (state, action) table


-- -----------------------------------------------------------------------------
-- Persistence
-- -----------------------------------------------------------------------------

-- | Saves a Q-table using its textual 'Show' representation.
saveQTable :: Show state => FilePath -> QTable state -> IO ()
saveQTable filePath table =
    writeFile filePath (show table)


-- | Loads a Q-table previously written by 'saveQTable'.
--
-- State types used for persisted Q-tables therefore need compatible 'Read' and
-- 'Ord' instances.
loadQTable :: (Read state, Ord state) => FilePath -> IO (QTable state)
loadQTable filePath = do
    contents <- readFile filePath

    case readMaybe contents of
        Just table -> pure table
        Nothing -> fail ("Could not parse Q-table from file: " ++ filePath)


-- -----------------------------------------------------------------------------
-- Learned policy
-- -----------------------------------------------------------------------------

-- | Creates a greedy agent from a Q-learning specification and learned table.
--
-- Training uses 'chooseActionEpsilonGreedy'; the resulting gameplay agent uses
-- the learned policy greedily without exploration.
qLearningAgent :: Ord state => QLearningSpec state -> QTable state -> Agent
qLearningAgent spec table state worm =
    bestQAction table (qlEncodeState spec state worm)