{-|
Module      : TrainingEnvironment
Description : Diverse environment sampling for CerviQ Q-learning training.

This module defines the distribution of scenarios, opponents, starting sides,
and initial food positions used by diverse Q-learning training. It implements
weighted random selection and produces 'TrainingEpisodeSetup' values consumed
by the generic trainer in "Training".

The current sampler deliberately constructs each scenario with only its two
default worms. Controller assignment is generic over all non-controlled worm
IDs, but the supplied final training configuration is therefore one-versus-one
and does not use the additional interactive starts.

Keeping environment sampling outside the training loop allows the same generic
Q-learning implementation to be trained under either fixed or varied
conditions.
-}

module TrainingEnvironment where

import Agent
import Config
import Game
import Scenario
import Scenarios
import Training
import Types

import System.Random (randomRIO)


-- -----------------------------------------------------------------------------
-- Training opponents
-- -----------------------------------------------------------------------------

-- | One opponent configuration available during diverse training.
data TrainingOpponent = TrainingOpponent
    {
        -- | Human-readable label stored in training statistics.
        trainingOpponentName :: String,

        -- | Controller assigned to every opponent worm in the episode.
        trainingOpponentController :: Controller
    }


-- | Default weighted opponent distribution used by diverse training.
--
-- The weights correspond to probabilities of 40%, 30%, 20%, and 10%
-- respectively because they sum to 100.
defaultTrainingOpponents :: [(Int, TrainingOpponent)]
defaultTrainingOpponents =
    [ (40, TrainingOpponent "Safe Greedy Food" (AI safeGreedyFoodAgent))
    , (30, TrainingOpponent "Safe Hunter" (AI safeHunterAgent))
    , (20, TrainingOpponent "Safe Random" (AI safeRandomAgent))
    , (10, TrainingOpponent "Random" (AI randomAgent))
    ]


-- -----------------------------------------------------------------------------
-- Training scenarios
-- -----------------------------------------------------------------------------

-- | Default weighted scenario distribution used by diverse training.
--
-- Arena is sampled slightly more often, while Cave and Corridors each account
-- for 30% of episodes.
defaultTrainingScenarios :: [(Int, Scenario)]
defaultTrainingScenarios =
    [ (40, arenaScenario)
    , (30, caveScenario)
    , (30, corridorScenario)
    ]


-- -----------------------------------------------------------------------------
-- Weighted random selection
-- -----------------------------------------------------------------------------

-- | Randomly selects one value according to positive integer weights.
--
-- Entries with zero or negative weight are ignored. 'Nothing' is returned when
-- no positively weighted value remains.
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


-- | Finds the value containing one position in a weighted distribution.
selectWeighted :: Int -> [(Int, value)] -> Maybe value
selectWeighted _ [] =
    Nothing

selectWeighted target ((weight, value) : remaining)
    | target <= weight = Just value
    | otherwise = selectWeighted (target - weight) remaining


-- -----------------------------------------------------------------------------
-- Initial state variation
-- -----------------------------------------------------------------------------



-- | Removes predefined food and randomly fills the map to the fixed training target.
randomizeInitialFood :: GameState -> IO GameState
randomizeInitialFood =
    randomizeFoodCount maxFoodCount

-- | Swaps the starting body and direction of the first two worms while keeping
-- their identifiers unchanged.
--
-- The learned worm therefore remains associated with the same ID and
-- controller, but experiences both physical starting sides. Head history is
-- reset afterwards to match the modified initial positions.
swapFirstTwoWormStarts :: GameState -> GameState
swapFirstTwoWormStarts state =
    resetHeadHistory $
        case gameWorms state of
            firstWorm : secondWorm : remainingWorms ->
                state
                    { gameWorms =
                        [ firstWorm
                            { wormBody = wormBody secondWorm
                            , wormDirection = wormDirection secondWorm
                            }
                        , secondWorm
                            { wormBody = wormBody firstWorm
                            , wormDirection = wormDirection firstWorm
                            }
                        ]
                            ++ remainingWorms
                    }

            _ ->
                state


-- | Randomly keeps or swaps the first two worm starting positions.
--
-- Both variants are selected with equal probability.
randomizeStartVariant :: GameState -> IO (GameState, String)
randomizeStartVariant state = do
    shouldSwap <- randomRIO (False, True)

    if shouldSwap
        then pure (swapFirstTwoWormStarts state, "Swapped")
        else pure (state, "Original")


-- -----------------------------------------------------------------------------
-- Diverse training
-- -----------------------------------------------------------------------------

-- | Creates one diverse training episode by independently sampling scenario,
-- opponent, starting side, and initial food placement.
--
-- The episode number is intentionally unused by the current sampler. It remains
-- part of the interface so future curricula could vary the distribution over
-- the course of training.
mixedTrainingEpisode :: Int -> Int -> IO TrainingEpisodeSetup
mixedTrainingEpisode controlledId _episodeNumber = do
    maybeScenario <- weightedRandomChoice defaultTrainingScenarios
    maybeOpponent <- weightedRandomChoice defaultTrainingOpponents

    case (maybeScenario, maybeOpponent) of
        (Just scenario, Just opponent) -> do
            let baseState = scenarioInitialState scenario

            (startState, startVariant) <- randomizeStartVariant baseState
            initialState <- randomizeInitialFood startState

            let opponentIds =
                    [ wormId worm
                    | worm <- gameWorms initialState
                    , wormId worm /= controlledId
                    ]

                opponentControllers =
                    [ (opponentId, trainingOpponentController opponent)
                    | opponentId <- opponentIds
                    ]

            pure
                TrainingEpisodeSetup
                    { trainingSetupInitialState = initialState
                    , trainingSetupOpponentControllers = opponentControllers
                    , trainingSetupScenarioName = scenarioName scenario
                    , trainingSetupOpponentName = trainingOpponentName opponent
                    , trainingSetupStartVariant = startVariant
                    }

        _ ->
            fail "Diverse training requires at least one scenario and one opponent."


-- | Creates the default diverse-training sampler for one controlled worm ID.
mixedTrainingSampler :: Int -> TrainingEpisodeSampler
mixedTrainingSampler controlledId =
    mixedTrainingEpisode controlledId
