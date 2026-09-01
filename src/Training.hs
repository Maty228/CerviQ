{-|
Module      : Training
Description : Generic tabular Q-learning training loop and training diagnostics.

This module implements version-independent training for CerviQ Q-learning
agents. Individual Q-learning versions provide a 'Core.QLearningSpec', while
this module handles epsilon-greedy exploration, environment stepping, Q-value
updates, episode termination, environment sampling, and collection of training
statistics.

The trainer does not know how a particular RL state is represented and does not
choose specific maps or opponents. Those concerns are supplied through
'Core.QLearningSpec' and 'TrainingEpisodeSampler' respectively.
-}

module Training where

import Data.List (intercalate, sort)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Text.Printf (printf)

import qualified QLearning.Core as Core
import Types


-- -----------------------------------------------------------------------------
-- Training configuration
-- -----------------------------------------------------------------------------

-- | Configuration of one complete Q-learning training run.
data TrainingConfig = TrainingConfig
    {
        -- | ID of the worm whose policy is being learned.
        trainingWormId :: Int,

        -- | Number of training episodes.
        trainingEpisodes :: Int,

        -- | Maximum number of simulated ticks in one episode.
        maxEpisodeTicks :: Int,

        -- | Q-learning learning rate alpha.
        learningRate :: Double,

        -- | Q-learning discount factor gamma.
        discountFactor :: Double,

        -- | Exploration probability used in the first episode.
        epsilonStart :: Double,

        -- | Minimum exploration probability retained during training.
        epsilonMinimum :: Double,

        -- | Multiplicative epsilon decay applied after every episode.
        epsilonDecay :: Double,

        -- | Number of episodes between progress reports.
        --
        -- Non-positive values disable periodic progress output.
        progressInterval :: Int
    }
    deriving (Show, Eq)


-- | Default configuration used by the original Q-learning experiments.
defaultTrainingConfig :: TrainingConfig
defaultTrainingConfig =
    TrainingConfig
        { trainingWormId = 1
        , trainingEpisodes = 10000
        , maxEpisodeTicks = 500
        , learningRate = 0.1
        , discountFactor = 0.9
        , epsilonStart = 1.0
        , epsilonMinimum = 0.05
        , epsilonDecay = 0.9995
        , progressInterval = 200
        }


-- -----------------------------------------------------------------------------
-- Training episode setup
-- -----------------------------------------------------------------------------

-- | Complete environment configuration selected for one training episode.
--
-- Besides the actual initial state and opponent controllers, descriptive labels
-- are retained so mixed training runs can report which scenarios, opponents,
-- and starting variants were sampled.
data TrainingEpisodeSetup = TrainingEpisodeSetup
    {
        -- | Initial game state of the episode.
        trainingSetupInitialState :: GameState,

        -- | Controllers assigned to opponent worms.
        trainingSetupOpponentControllers :: [(Int, Controller)],

        -- | Human-readable scenario label used in training statistics.
        trainingSetupScenarioName :: String,

        -- | Human-readable opponent label used in training statistics.
        trainingSetupOpponentName :: String,

        -- | Human-readable starting-position variant.
        trainingSetupStartVariant :: String
    }


-- | Produces the environment configuration for one training episode.
--
-- The episode number can be used by samplers that want training conditions to
-- change over time, although the current diverse sampler primarily chooses
-- environments randomly.
type TrainingEpisodeSampler = Int -> IO TrainingEpisodeSetup


-- | Creates a fixed setup compatible with the original training interface.
fixedTrainingEpisodeSetup :: [(Int, Controller)] -> GameState -> TrainingEpisodeSetup
fixedTrainingEpisodeSetup opponentControllers initialState =
    TrainingEpisodeSetup
        { trainingSetupInitialState = initialState
        , trainingSetupOpponentControllers = opponentControllers
        , trainingSetupScenarioName = "Fixed"
        , trainingSetupOpponentName = "Fixed"
        , trainingSetupStartVariant = "Fixed"
        }


-- -----------------------------------------------------------------------------
-- Training statistics
-- -----------------------------------------------------------------------------

-- | Statistics collected from one completed training episode.
data TrainingEpisodeStats = TrainingEpisodeStats
    {
        -- | One-based episode number.
        trainingEpisode :: Int,

        -- | Sum of all rewards obtained during the episode.
        trainingEpisodeReward :: Double,

        -- | Number of simulated game ticks.
        trainingEpisodeTicks :: Int,

        -- | Exploration probability used throughout this episode.
        trainingEpisodeEpsilon :: Double,

        -- | Whether the controlled worm died before the tick limit.
        trainingEpisodeDied :: Bool,

        -- | Total food eaten by the controlled worm.
        trainingEpisodeFoodEaten :: Int,

        -- | Total kills made by the controlled worm.
        trainingEpisodeKills :: Int,

        -- | Final body length of the controlled worm.
        trainingEpisodeFinalLength :: Int,

        -- | Whether the controlled worm is the sole living worm in the final
        -- episode state.
        --
        -- This metric describes the final state only; it does not record
        -- whether the worm became last standing at an earlier point.
        trainingEpisodeLastStanding :: Bool,

        -- | Scenario label of the sampled environment.
        trainingEpisodeScenario :: String,

        -- | Opponent label of the sampled environment.
        trainingEpisodeOpponent :: String,

        -- | Starting-position variant of the sampled environment.
        trainingEpisodeStartVariant :: String
    }
    deriving (Show, Eq)


-- | Creates episode statistics from its final game state.
makeTrainingEpisodeStats :: TrainingConfig -> TrainingEpisodeSetup -> Int -> Double -> Double -> Int -> Bool -> GameState -> TrainingEpisodeStats
makeTrainingEpisodeStats config setup episodeNumber epsilon totalReward ticks died finalState =
    TrainingEpisodeStats
        { trainingEpisode = episodeNumber
        , trainingEpisodeReward = totalReward
        , trainingEpisodeTicks = ticks
        , trainingEpisodeEpsilon = epsilon
        , trainingEpisodeDied = died
        , trainingEpisodeFoodEaten = finalFood
        , trainingEpisodeKills = finalKills
        , trainingEpisodeFinalLength = finalLength
        , trainingEpisodeLastStanding = lastStanding
        , trainingEpisodeScenario = trainingSetupScenarioName setup
        , trainingEpisodeOpponent = trainingSetupOpponentName setup
        , trainingEpisodeStartVariant = trainingSetupStartVariant setup
        }
  where
    controlledId = trainingWormId config
    finalWorm = Core.controlledWorm controlledId finalState

    finalFood =
        maybe 0 (foodEaten . wormStats) finalWorm

    finalKills =
        maybe 0 (kills . wormStats) finalWorm

    finalLength =
        maybe 0 (length . wormBody) finalWorm

    livingOpponents =
        [ worm
        | worm <- gameWorms finalState
        , wormId worm /= controlledId
        , wormAlive worm
        ]

    lastStanding =
        case finalWorm of
            Just worm -> wormAlive worm && null livingOpponents
            Nothing -> False


-- -----------------------------------------------------------------------------
-- Episode training
-- -----------------------------------------------------------------------------

-- | Computes the exploration probability for the next training episode.
--
-- Epsilon decays multiplicatively once per episode and never falls below
-- 'epsilonMinimum'.
nextEpsilon :: TrainingConfig -> Double -> Double
nextEpsilon config currentEpsilon =
    max
        (epsilonMinimum config)
        (currentEpsilon * epsilonDecay config)


-- | Trains one Q-learning version for one complete episode in a fixed
-- environment.
--
-- This compatibility wrapper converts the fixed environment into a
-- 'TrainingEpisodeSetup' and delegates to 'trainEpisodeWithSetup'.
trainEpisode :: Ord state => Core.QLearningSpec state -> TrainingConfig -> Int -> [(Int, Controller)] -> GameState -> Double -> Core.QTable state -> IO (Core.QTable state, TrainingEpisodeStats)
trainEpisode spec config episodeNumber opponentControllers initialState epsilon initialQTable =
    trainEpisodeWithSetup
        spec
        config
        episodeNumber
        (fixedTrainingEpisodeSetup opponentControllers initialState)
        epsilon
        initialQTable


-- | Trains one Q-learning version for one complete sampled episode.
--
-- Every non-terminal tick performs the standard training sequence:
--
-- 1. choose an action using epsilon-greedy exploration;
-- 2. advance the normal game environment with opponent controllers;
-- 3. observe reward and the optional next encoded state;
-- 4. update the selected state-action Q-value;
-- 5. continue until death or the configured tick limit.
--
-- A single epsilon value is used throughout the episode. Epsilon decay happens
-- between episodes in 'trainEpisodesWithSampler'.
trainEpisodeWithSetup :: Ord state => Core.QLearningSpec state -> TrainingConfig -> Int -> TrainingEpisodeSetup -> Double -> Core.QTable state -> IO (Core.QTable state, TrainingEpisodeStats)
trainEpisodeWithSetup spec config episodeNumber setup epsilon initialQTable =
    case Core.controlledWorm controlledId initialState of
        Nothing ->
            invalidInitialEpisode

        Just worm
            | not (wormAlive worm) ->
                invalidInitialEpisode

            | otherwise ->
                trainingLoop
                    0
                    0
                    initialState
                    (Core.qlEncodeState spec initialState worm)
                    initialQTable

  where
    controlledId =
        trainingWormId config

    initialState =
        trainingSetupInitialState setup

    opponentControllers =
        trainingSetupOpponentControllers setup

    -- | Returns a terminal result for an invalid initial environment.
    invalidInitialEpisode =
        pure
            ( initialQTable
            , makeTrainingEpisodeStats
                config
                setup
                episodeNumber
                epsilon
                0
                0
                True
                initialState
            )

    -- | Repeatedly performs Q-learning updates until the episode terminates.
    trainingLoop ticks totalReward currentState currentRlState qTable
        | ticks >= maxEpisodeTicks config =
            pure
                ( qTable
                , makeTrainingEpisodeStats
                    config
                    setup
                    episodeNumber
                    epsilon
                    totalReward
                    ticks
                    False
                    currentState
                )

        | otherwise = do
            action <-
                Core.chooseActionEpsilonGreedy
                    epsilon
                    qTable
                    currentRlState

            step <-
                Core.stepEnvironmentWithOpponents
                    spec
                    controlledId
                    action
                    opponentControllers
                    currentState

            let reward =
                    Core.rlReward step

                nextGameState =
                    Core.rlNextGameState step

                updatedQTable =
                    Core.updateQValue
                        (learningRate config)
                        (discountFactor config)
                        currentRlState
                        action
                        reward
                        (Core.rlNextRlState step)
                        qTable

                newTotalReward =
                    totalReward + reward

                newTicks =
                    ticks + 1

            if Core.rlDone step
                then
                    pure
                        ( updatedQTable
                        , makeTrainingEpisodeStats
                            config
                            setup
                            episodeNumber
                            epsilon
                            newTotalReward
                            newTicks
                            True
                            nextGameState
                        )

                else
                    case Core.rlNextRlState step of
                        Nothing ->
                            pure
                                ( updatedQTable
                                , makeTrainingEpisodeStats
                                    config
                                    setup
                                    episodeNumber
                                    epsilon
                                    newTotalReward
                                    newTicks
                                    True
                                    nextGameState
                                )

                        Just nextRlState ->
                            trainingLoop
                                newTicks
                                newTotalReward
                                nextGameState
                                nextRlState
                                updatedQTable


-- -----------------------------------------------------------------------------
-- Multi-episode training
-- -----------------------------------------------------------------------------

-- | Trains one Q-learning version using the same environment for every episode.
trainEpisodes :: Ord state => Core.QLearningSpec state -> TrainingConfig -> [(Int, Controller)] -> GameState -> IO (Core.QTable state, [TrainingEpisodeStats])
trainEpisodes spec config opponentControllers initialState =
    trainEpisodesWithSampler spec config sampler
  where
    sampler _ =
        pure (fixedTrainingEpisodeSetup opponentControllers initialState)


-- | Trains one Q-learning version while sampling a fresh environment
-- configuration for every episode.
--
-- The learned Q-table is carried across all episodes. Epsilon is updated once
-- after each episode, while progress statistics are accumulated and printed in
-- independent blocks.
trainEpisodesWithSampler :: Ord state => Core.QLearningSpec state -> TrainingConfig -> TrainingEpisodeSampler -> IO (Core.QTable state, [TrainingEpisodeStats])
trainEpisodesWithSampler spec config sampler = do
    startTime <- getCurrentTime
    trainingLoop 1 (epsilonStart config) Core.emptyQTable [] [] startTime startTime

  where
    -- | Repeatedly samples and trains episodes while carrying over the Q-table.
    trainingLoop episodeNumber epsilon qTable collectedStats blockStats blockStartTime trainingStartTime
        | episodeNumber > trainingEpisodes config =
            pure (qTable, reverse collectedStats)

        | otherwise = do
            setup <-
                sampler episodeNumber

            (updatedQTable, episodeStats) <-
                trainEpisodeWithSetup
                    spec
                    config
                    episodeNumber
                    setup
                    epsilon
                    qTable

            let updatedStats =
                    episodeStats : collectedStats

                updatedBlockStats =
                    episodeStats : blockStats

                interval =
                    progressInterval config

                shouldPrintProgress =
                    interval > 0
                        && ( episodeNumber `mod` interval == 0
                                || episodeNumber == trainingEpisodes config
                           )

                nextEpisode =
                    episodeNumber + 1

                nextEpisodeEpsilon =
                    nextEpsilon config epsilon

            if shouldPrintProgress
                then do
                    now <- getCurrentTime

                    let blockSeconds =
                            realToFrac (diffUTCTime now blockStartTime)

                        totalSeconds =
                            realToFrac (diffUTCTime now trainingStartTime)

                    printTrainingProgress
                        episodeNumber
                        (trainingEpisodes config)
                        updatedBlockStats
                        updatedQTable
                        blockSeconds
                        totalSeconds

                    trainingLoop
                        nextEpisode
                        nextEpisodeEpsilon
                        updatedQTable
                        updatedStats
                        []
                        now
                        trainingStartTime

                else
                    trainingLoop
                        nextEpisode
                        nextEpisodeEpsilon
                        updatedQTable
                        updatedStats
                        updatedBlockStats
                        blockStartTime
                        trainingStartTime

-- -----------------------------------------------------------------------------
-- General statistics helpers
-- -----------------------------------------------------------------------------

-- | Computes the arithmetic mean of a list of values.
averageDouble :: [Double] -> Double
averageDouble [] = 0
averageDouble values = sum values / fromIntegral (length values)


-- | Computes the median of a list of values.
medianDouble :: [Double] -> Double
medianDouble [] = 0
medianDouble values
    | odd count = sortedValues !! middle
    | otherwise = (sortedValues !! (middle - 1) + sortedValues !! middle) / 2
  where
    sortedValues = sort values
    count = length sortedValues
    middle = count `div` 2


-- | Computes an approximate percentile of a list of values.
--
-- The requested percentile is clamped to the interval @[0,1]@. The result is
-- selected directly from the sorted observations without interpolation.
percentileDouble :: Double -> [Double] -> Double
percentileDouble _ [] = 0
percentileDouble percentile values =
    sortedValues !! index
  where
    sortedValues = sort values
    clampedPercentile = max 0 (min 1 percentile)
    index = floor (clampedPercentile * fromIntegral (length sortedValues - 1))


-- -----------------------------------------------------------------------------
-- Training statistics helpers
-- -----------------------------------------------------------------------------

-- | Computes the average accumulated reward over the given episodes.
averageTrainingReward :: [TrainingEpisodeStats] -> Double
averageTrainingReward stats =
    averageDouble (map trainingEpisodeReward stats)


-- | Computes the median accumulated reward over the given episodes.
medianTrainingReward :: [TrainingEpisodeStats] -> Double
medianTrainingReward stats =
    medianDouble (map trainingEpisodeReward stats)


-- | Returns the minimum and maximum accumulated reward in the given episodes.
trainingRewardRange :: [TrainingEpisodeStats] -> (Double, Double)
trainingRewardRange [] = (0, 0)
trainingRewardRange stats =
    (minimum rewards, maximum rewards)
  where
    rewards = map trainingEpisodeReward stats


-- | Computes the average episode length in simulated ticks.
averageTrainingTicks :: [TrainingEpisodeStats] -> Double
averageTrainingTicks stats =
    averageDouble (map (fromIntegral . trainingEpisodeTicks) stats)


-- | Computes the median episode length in simulated ticks.
medianTrainingTicks :: [TrainingEpisodeStats] -> Double
medianTrainingTicks stats =
    medianDouble (map (fromIntegral . trainingEpisodeTicks) stats)


-- | Computes the approximate 90th percentile of episode lengths.
p90TrainingTicks :: [TrainingEpisodeStats] -> Double
p90TrainingTicks stats =
    percentileDouble 0.9 (map (fromIntegral . trainingEpisodeTicks) stats)


-- | Returns the longest episode in the given training block.
maxTrainingTicks :: [TrainingEpisodeStats] -> Int
maxTrainingTicks [] = 0
maxTrainingTicks stats =
    maximum (map trainingEpisodeTicks stats)


-- | Computes the proportion of episodes in which the controlled worm died
-- before reaching the configured tick limit.
trainingDeathRate :: [TrainingEpisodeStats] -> Double
trainingDeathRate [] = 0
trainingDeathRate stats =
    fromIntegral deaths / fromIntegral (length stats)
  where
    deaths = length (filter trainingEpisodeDied stats)


-- | Computes the proportion of episodes in which the controlled worm survived
-- until the configured tick limit.
trainingSurvivalRate :: [TrainingEpisodeStats] -> Double
trainingSurvivalRate [] = 0
trainingSurvivalRate stats =
    1 - trainingDeathRate stats


-- | Computes the average amount of food eaten per episode.
averageTrainingFood :: [TrainingEpisodeStats] -> Double
averageTrainingFood stats =
    averageDouble (map (fromIntegral . trainingEpisodeFoodEaten) stats)


-- | Computes total food eaten per 100 simulated training ticks.
trainingFoodPer100Ticks :: [TrainingEpisodeStats] -> Double
trainingFoodPer100Ticks [] = 0
trainingFoodPer100Ticks stats
    | totalTicks == 0 = 0
    | otherwise = 100 * fromIntegral totalFood / fromIntegral totalTicks
  where
    totalFood = sum (map trainingEpisodeFoodEaten stats)
    totalTicks = sum (map trainingEpisodeTicks stats)


-- | Computes the average number of kills made per episode.
averageTrainingKills :: [TrainingEpisodeStats] -> Double
averageTrainingKills stats =
    averageDouble (map (fromIntegral . trainingEpisodeKills) stats)


-- | Computes the average final body length of the controlled worm.
averageTrainingFinalLength :: [TrainingEpisodeStats] -> Double
averageTrainingFinalLength stats =
    averageDouble (map (fromIntegral . trainingEpisodeFinalLength) stats)


-- | Computes the proportion of episodes that ended with the controlled worm as
-- the only living worm.
trainingLastStandingRate :: [TrainingEpisodeStats] -> Double
trainingLastStandingRate [] = 0
trainingLastStandingRate stats =
    fromIntegral lastStandingEpisodes / fromIntegral (length stats)
  where
    lastStandingEpisodes =
        length (filter trainingEpisodeLastStanding stats)


-- -----------------------------------------------------------------------------
-- Q-table statistics
-- -----------------------------------------------------------------------------

-- | Returns the number of explicitly learned state-action pairs in a Q-table.
qTableEntryCount :: Core.QTable state -> Int
qTableEntryCount =
    Map.size


-- | Returns the number of distinct encoded states represented in a Q-table.
qTableStateCount :: Ord state => Core.QTable state -> Int
qTableStateCount table =
    Set.size (Set.fromList [state | (state, _) <- Map.keys table])


-- -----------------------------------------------------------------------------
-- Timing helpers
-- -----------------------------------------------------------------------------

-- | Formats a duration in seconds into a compact human-readable representation.
formatDuration :: Double -> String
formatDuration seconds
    | totalSeconds >= 3600 =
        printf "%dh %02dm %02ds" hours minutes remainingSeconds

    | totalSeconds >= 60 =
        printf "%dm %02ds" minutes remainingSeconds

    | otherwise =
        printf "%.2fs" seconds
  where
    totalSeconds = max 0 (round seconds) :: Int
    hours = totalSeconds `div` 3600
    minutes = (totalSeconds `mod` 3600) `div` 60
    remainingSeconds = totalSeconds `mod` 60


-- -----------------------------------------------------------------------------
-- Progress output
-- -----------------------------------------------------------------------------

-- | Counts occurrences of one descriptive label in episode statistics.
trainingLabelCounts :: (TrainingEpisodeStats -> String) -> [TrainingEpisodeStats] -> [(String, Int)]
trainingLabelCounts getLabel stats =
    Map.toList $
        Map.fromListWith
            (+)
            [(getLabel stat, 1) | stat <- stats]


-- | Formats descriptive-label counts for the training progress report.
formatTrainingLabelCounts :: [(String, Int)] -> String
formatTrainingLabelCounts counts =
    intercalate ", " [label ++ "=" ++ show count | (label, count) <- counts]


-- | Prints a detailed progress report for the most recent training block.
--
-- Block-local statistics describe only episodes since the previous report,
-- while Q-table size and elapsed time describe the complete training run.
printTrainingProgress :: Ord state => Int -> Int -> [TrainingEpisodeStats] -> Core.QTable state -> Double -> Double -> IO ()
printTrainingProgress currentEpisode totalEpisodes recentStats qTable blockSeconds totalSeconds =
    case recentStats of
        [] ->
            pure ()

        latestStats : _ -> do
            let (minimumReward, maximumReward) = trainingRewardRange recentStats
                blockEpisodeCount = length recentStats
                blockTickCount = sum (map trainingEpisodeTicks recentStats)

                episodesPerSecond =
                    if blockSeconds > 0
                        then fromIntegral blockEpisodeCount / blockSeconds
                        else 0

                ticksPerSecond =
                    if blockSeconds > 0
                        then fromIntegral blockTickCount / blockSeconds
                        else 0

                remainingEpisodes =
                    totalEpisodes - currentEpisode

                secondsPerEpisode =
                    if blockEpisodeCount > 0
                        then blockSeconds / fromIntegral blockEpisodeCount
                        else 0

                estimatedRemainingSeconds =
                    secondsPerEpisode * fromIntegral remainingEpisodes

            putStrLn $ "Episode " ++ show currentEpisode ++ " / " ++ show totalEpisodes
            putStrLn $ printf "  average reward:       %.2f" (averageTrainingReward recentStats)
            putStrLn $ printf "  median reward:        %.2f" (medianTrainingReward recentStats)
            putStrLn $ printf "  reward range:         %.2f .. %.2f" minimumReward maximumReward
            putStrLn $ printf "  average ticks:        %.2f" (averageTrainingTicks recentStats)
            putStrLn $ printf "  median ticks:         %.1f" (medianTrainingTicks recentStats)
            putStrLn $ printf "  p90 ticks:            %.1f" (p90TrainingTicks recentStats)
            putStrLn $ "  max ticks:            " ++ show (maxTrainingTicks recentStats)
            putStrLn $ printf "  death / survival:     %.1f %% / %.1f %%" (100 * trainingDeathRate recentStats) (100 * trainingSurvivalRate recentStats)
            putStrLn $ printf "  average food:         %.2f" (averageTrainingFood recentStats)
            putStrLn $ printf "  food / 100 ticks:     %.2f" (trainingFoodPer100Ticks recentStats)
            putStrLn $ printf "  average kills:        %.3f" (averageTrainingKills recentStats)
            putStrLn $ printf "  average final length: %.2f" (averageTrainingFinalLength recentStats)
            putStrLn $ printf "  last standing rate:   %.1f %%" (100 * trainingLastStandingRate recentStats)
            putStrLn $ "  scenarios:            " ++ formatTrainingLabelCounts (trainingLabelCounts trainingEpisodeScenario recentStats)
            putStrLn $ "  opponents:            " ++ formatTrainingLabelCounts (trainingLabelCounts trainingEpisodeOpponent recentStats)
            putStrLn $ "  starts:               " ++ formatTrainingLabelCounts (trainingLabelCounts trainingEpisodeStartVariant recentStats)
            putStrLn $ printf "  epsilon:              %.5f" (trainingEpisodeEpsilon latestStats)
            putStrLn $ "  Q-table states:       " ++ show (qTableStateCount qTable)
            putStrLn $ "  Q-table entries:      " ++ show (qTableEntryCount qTable)
            putStrLn $ "  block episodes:       " ++ show blockEpisodeCount
            putStrLn $ "  block time:           " ++ formatDuration blockSeconds
            putStrLn $ "  total elapsed:        " ++ formatDuration totalSeconds
            putStrLn $ printf "  training speed:       %.2f episodes/s" episodesPerSecond
            putStrLn $ printf "  simulation speed:     %.0f ticks/s" ticksPerSecond
            putStrLn $ "  estimated remaining:  " ++ formatDuration estimatedRemainingSeconds
            putStrLn "======================================"
            putStrLn ""