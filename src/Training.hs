module Training where

import qualified QLearning.Core as Core
import Types

-- -----------------------------------------------------------------------------
-- Training configuration
-- -----------------------------------------------------------------------------

-- | Configuration of the Q-learning training process.
data TrainingConfig = TrainingConfig {
    trainingWormId :: Int,
    trainingEpisodes :: Int,
    maxEpisodeTicks :: Int,
    learningRate :: Double,
    discountFactor :: Double,
    epsilonStart :: Double,
    epsilonMinimum :: Double,
    epsilonDecay :: Double,
    progressInterval :: Int

} deriving (Show, Eq)

-- | Default configuration used for initial Q-learning experiments.
defaultTrainingConfig :: TrainingConfig
defaultTrainingConfig = 
    TrainingConfig {
        trainingWormId = 1,
        trainingEpisodes = 10000,
        maxEpisodeTicks = 500,
        learningRate = 0.1,
        discountFactor = 0.9,
        epsilonStart = 1.0,
        epsilonMinimum = 0.05,
        epsilonDecay = 0.9995,
        progressInterval = 200
    }


-- -----------------------------------------------------------------------------
-- Training statistics
-- -----------------------------------------------------------------------------

-- | Statistics collected from one training episode.
data TrainingEpisodeStats = TrainingEpisodeStats {
    trainingEpisode :: Int,
    trainingEpisodeReward :: Double,
    trainingEpisodeTicks :: Int,
    trainingEpisodeEpsilon :: Double,
    trainingEpisodeDied :: Bool
} deriving (Show, Eq)


-- -----------------------------------------------------------------------------
-- Episode training
-- -----------------------------------------------------------------------------

-- | Computes epsilon for the next training episode.
nextEpsilon :: TrainingConfig -> Double -> Double
nextEpsilon config currentEpsilon = 
    max (epsilonMinimum config) (currentEpsilon * epsilonDecay config)

-- | Trains one Q-learning version for one complete episode.
--
-- The Q-learning specification determines how the game state is encoded and
-- how rewards are computed. The episode ends when the controlled worm dies or
-- when the configured maximum number of ticks is reached.
trainEpisode :: Ord state => Core.QLearningSpec state -> TrainingConfig -> Int -> [(Int, Controller)] -> GameState -> Double -> Core.QTable state -> IO (Core.QTable state, TrainingEpisodeStats)
trainEpisode spec config episodeNumber opponentControllers initialState epsilon initialQTable =
        case Core.controlledWorm controlledId initialState of
            Nothing ->
                pure
                    (
                        initialQTable,
                        TrainingEpisodeStats
                            {
                                trainingEpisode = episodeNumber,
                                trainingEpisodeReward = 0,
                                trainingEpisodeTicks = 0,
                                trainingEpisodeEpsilon = epsilon,
                                trainingEpisodeDied = True
                            }
                    )

            Just worm | not (wormAlive worm) ->
                    pure
                        (
                            initialQTable,
                            TrainingEpisodeStats
                                {
                                    trainingEpisode = episodeNumber,
                                    trainingEpisodeReward = 0,
                                    trainingEpisodeTicks = 0,
                                    trainingEpisodeEpsilon = epsilon,
                                    trainingEpisodeDied = True
                                }
                        )

                | otherwise -> trainingLoop 0 0 initialState (Core.qlEncodeState spec initialState worm) initialQTable
  where
    controlledId = trainingWormId config

    -- Repeatedly performs Q-learning steps until the episode terminates.
    trainingLoop ticks totalReward currentState currentRlState qTable
        | ticks >= maxEpisodeTicks config =
            pure
                (
                    qTable,
                    TrainingEpisodeStats
                        {
                            trainingEpisode = episodeNumber,
                            trainingEpisodeReward = totalReward,
                            trainingEpisodeTicks = ticks,
                            trainingEpisodeEpsilon = epsilon,
                            trainingEpisodeDied = False
                        }
                )

        | otherwise = do
            action <- Core.chooseActionEpsilonGreedy epsilon qTable currentRlState
            step <- Core.stepEnvironmentWithOpponents spec controlledId action opponentControllers currentState

            let reward = Core.rlReward step
                updatedQTable = Core.updateQValue (learningRate config) (discountFactor config) currentRlState action reward (Core.rlNextRlState step) qTable
                newTotalReward = totalReward + reward
                newTicks = ticks + 1

            if Core.rlDone step
                then
                    pure
                        (
                            updatedQTable,
                            TrainingEpisodeStats
                                {
                                    trainingEpisode = episodeNumber,
                                    trainingEpisodeReward = newTotalReward,
                                    trainingEpisodeTicks = newTicks,
                                    trainingEpisodeEpsilon = epsilon,
                                    trainingEpisodeDied = True
                                }
                        )

                else
                    case Core.rlNextRlState step of
                        Nothing ->
                            pure
                                (
                                    updatedQTable,
                                    TrainingEpisodeStats
                                        {
                                            trainingEpisode = episodeNumber,
                                            trainingEpisodeReward = newTotalReward,
                                            trainingEpisodeTicks = newTicks,
                                            trainingEpisodeEpsilon = epsilon,
                                            trainingEpisodeDied = True
                                        }
                                )

                        Just nextRlState -> trainingLoop newTicks newTotalReward (Core.rlNextGameState step) nextRlState updatedQTable

-- -----------------------------------------------------------------------------
-- Multi-episode training
-- -----------------------------------------------------------------------------

-- | Trains one Q-learning version over all configured training episodes.
--
-- The learned Q-table is carried from one episode to the next, while epsilon
-- gradually decreases according to the configured decay schedule.
trainEpisodes :: Ord state => Core.QLearningSpec state -> TrainingConfig -> [(Int, Controller)] -> GameState -> IO (Core.QTable state, [TrainingEpisodeStats])
trainEpisodes spec config opponentControllers initialState =
    trainingLoop 1 (epsilonStart config) Core.emptyQTable []
  where
    -- Repeatedly trains episodes while carrying over the learned Q-table.
    trainingLoop episodeNumber epsilon qTable collectedStats
        | episodeNumber > trainingEpisodes config =
            pure (qTable, reverse collectedStats)

        | otherwise = do
            (updatedQTable, episodeStats) <- trainEpisode spec config episodeNumber opponentControllers initialState epsilon qTable

            let updatedStats = episodeStats : collectedStats
                interval = progressInterval config
                shouldPrintProgress = 
                    interval > 0 && ( episodeNumber `mod` interval == 0 || episodeNumber == trainingEpisodes config)

                recentStats = take interval updatedStats

            if shouldPrintProgress
                then
                    printTrainingProgress episodeNumber (trainingEpisodes config) recentStats
                else
                    pure ()

            trainingLoop (episodeNumber + 1) (nextEpsilon config epsilon) updatedQTable updatedStats

-- -----------------------------------------------------------------------------
-- Training statistics helpers
-- -----------------------------------------------------------------------------

-- | Computes the arithmetic mean of a list of Double values.
averageDouble :: [Double] -> Double
averageDouble [] =
    0

averageDouble values =
    sum values / fromIntegral (length values)

-- | Computes the average reward over the given training episodes.
averageTrainingReward :: [TrainingEpisodeStats] -> Double
averageTrainingReward stats =
    averageDouble (map trainingEpisodeReward stats)

-- | Computes the average episode length over the given training episodes.
averageTrainingTicks :: [TrainingEpisodeStats] -> Double
averageTrainingTicks stats =
    averageDouble (map (fromIntegral . trainingEpisodeTicks) stats)

-- | Computes the proportion of training episodes in which the controlled worm died
trainingDeathRate :: [TrainingEpisodeStats] -> Double
trainingDeathRate [] = 0
trainingDeathRate stats = 
    fromIntegral deaths / fromIntegral (length stats)
  where
    deaths = length (filter trainingEpisodeDied stats)

-- | Prints a summary of the most recent block of training episodes.
printTrainingProgress :: Int -> Int -> [TrainingEpisodeStats] -> IO ()
printTrainingProgress currentEpisode totalEpisodes recentStats =
    case recentStats of
        [] -> pure ()
        latestStats : _ -> do
            putStrLn $
                "Episode "
                    ++ show currentEpisode
                    ++ " / "
                    ++ show totalEpisodes
            putStrLn $
                "  average reward: "
                    ++ show (averageTrainingReward recentStats)
            putStrLn $
                "  average ticks:  "
                    ++ show (averageTrainingTicks recentStats)
            putStrLn $
                "  death rate:     " 
                    ++ show (100 * trainingDeathRate recentStats)
                    ++ " %"
            putStrLn $
                "  epislon:        "
                ++ show (trainingEpisodeEpsilon latestStats)
            putStrLn ""