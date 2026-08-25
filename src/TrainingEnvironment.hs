module TrainingEnvironment where

import Agent
import Config
import Game
import Scenario
import Scenarios
import Training
import Types

import qualified Data.Map as Map

import System.Random (randomRIO)


-- -----------------------------------------------------------------------------
-- Training opponents
-- -----------------------------------------------------------------------------

-- | One opponent configuration available during mixed training.
data TrainingOpponent = TrainingOpponent
    {
        trainingOpponentName :: String,
        trainingOpponentController :: Controller
    }


-- | Default weighted opponent pool.
defaultTrainingOpponents :: [(Int, TrainingOpponent)]
defaultTrainingOpponents =
    [
        (40, TrainingOpponent "Safe Greedy Food" (AI safeGreedyFoodAgent)),
        (30, TrainingOpponent "Safe Hunter" (AI safeHunterAgent)),
        (20, TrainingOpponent "Safe Random" (AI safeRandomAgent)),
        (10, TrainingOpponent "Random" (AI randomAgent))
    ]


-- -----------------------------------------------------------------------------
-- Training scenarios
-- -----------------------------------------------------------------------------

-- | Default weighted scenario pool.
defaultTrainingScenarios :: [(Int, Scenario)]
defaultTrainingScenarios =
    [
        (40, arenaScenario),
        (30, caveScenario),
        (30, corridorScenario)
    ]


-- -----------------------------------------------------------------------------
-- Weighted random selection
-- -----------------------------------------------------------------------------

-- | Selects one value according to positive integer weights.
weightedRandomChoice :: [(Int, value)] -> IO (Maybe value)
weightedRandomChoice values =
    case positiveValues of
        [] ->
            pure Nothing

        _ -> do
            target <- randomRIO (1, totalWeight)
            pure (selectWeighted target positiveValues)
  where
    positiveValues = filter ((> 0) . fst) values
    totalWeight = sum (map fst positiveValues)


-- | Finds the value corresponding to one weighted random position.
selectWeighted :: Int -> [(Int, value)] -> Maybe value
selectWeighted _ [] =
    Nothing

selectWeighted target ((weight, value) : remaining)
    | target <= weight =
        Just value

    | otherwise =
        selectWeighted (target - weight) remaining


-- -----------------------------------------------------------------------------
-- Initial state variation
-- -----------------------------------------------------------------------------

-- | Removes all food tiles while preserving every other map tile.
clearFood :: GameMap -> GameMap
clearFood gameMap' =
    gameMap'
        {
            mapTiles = Map.map clearTile (mapTiles gameMap')
        }
  where
    clearTile Food = Empty
    clearTile tile = tile


-- | Removes predefined food and randomly fills the map to the configured amount.
randomizeInitialFood :: GameState -> IO GameState
randomizeInitialFood state =
    maintainFoodCount
        maxFoodCount
        state
            {
                gameMap = clearFood (gameMap state)
            }

-- | Swaps the starting body and direction of the first two worms while keeping
-- their identifiers unchanged.
-- | Swaps the starting body and direction of the first two worms while keeping
-- their identifiers unchanged.
swapFirstTwoWormStarts :: GameState -> GameState
swapFirstTwoWormStarts state =
    resetHeadHistory
        ( case gameWorms state of
            firstWorm : secondWorm : remainingWorms ->
                state
                    {
                        gameWorms =
                            [
                                firstWorm
                                    {
                                        wormBody = wormBody secondWorm,
                                        wormDirection = wormDirection secondWorm
                                    },

                                secondWorm
                                    {
                                        wormBody = wormBody firstWorm,
                                        wormDirection = wormDirection firstWorm
                                    }
                            ]
                            ++ remainingWorms
                    }

            _ -> state
        )


-- | Randomly keeps or swaps the first two starting positions.
randomizeStartVariant :: GameState -> IO (GameState, String)
randomizeStartVariant state = do
    shouldSwap <- randomRIO (False, True)

    if shouldSwap
        then
            pure (swapFirstTwoWormStarts state, "Swapped")
        else
            pure (state, "Original")


-- -----------------------------------------------------------------------------
-- Mixed training
-- -----------------------------------------------------------------------------

-- | Creates one diverse training episode by sampling map, opponent, start side
-- and initial food positions.
mixedTrainingEpisode :: Int -> Int -> IO TrainingEpisodeSetup
mixedTrainingEpisode controlledId _ = do
    maybeScenario <- weightedRandomChoice defaultTrainingScenarios
    maybeOpponent <- weightedRandomChoice defaultTrainingOpponents

    case (maybeScenario, maybeOpponent) of
        (Just scenario, Just opponent) -> do
            let baseState = scenarioInitialState scenario

            (startState, startVariant) <- randomizeStartVariant baseState
            initialState <- randomizeInitialFood startState

            let opponentIds =
                    [
                        wormId worm | worm <- gameWorms initialState,
                        wormId worm /= controlledId
                    ]

                opponentControllers =
                    [
                        (opponentId, trainingOpponentController opponent) | opponentId <- opponentIds
                    ]

            pure
                TrainingEpisodeSetup
                    {
                        trainingSetupInitialState = initialState,
                        trainingSetupOpponentControllers = opponentControllers,
                        trainingSetupScenarioName = scenarioName scenario,
                        trainingSetupOpponentName = trainingOpponentName opponent,
                        trainingSetupStartVariant = startVariant
                    }

        _ ->
            fail "Mixed training requires at least one scenario and one opponent."


-- | Creates the default sampler used for diverse Q-learning training.
mixedTrainingSampler :: Int -> TrainingEpisodeSampler
mixedTrainingSampler controlledId =
    mixedTrainingEpisode controlledId