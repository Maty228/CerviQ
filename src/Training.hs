module Training where

import QLearning
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

-- | Trains the Q-learning agent for one complete episode.
--
-- The episode ends when the controlled worm dies or when the configured maximum number of ticks is reached.
trainEpisode :: TrainingConfig -> Int -> [(Int, Controller)] -> GameState -> Double -> QTable -> IO (QTable, TrainingEpisodeStats)
trainEpisode config episodeNumber opponentControllers initialState epsilon initialQTable =
    case controlledWorm controlledId initialState of
        Nothing -> 
            pure 
            (
                initialQTable,
                TrainingEpisodeStats {
                    trainingEpisode = episodeNumber,
                            trainingEpisodeReward = 0,
                            trainingEpisodeTicks = 0,
                            trainingEpisodeEpsilon = epsilon,
                            trainingEpisodeDied = True
                }
            )

        Just worm 
            | not (wormAlive worm) -> pure (
                initialQTable,
                TrainingEpisodeStats {
                    trainingEpisode = episodeNumber,
                            trainingEpisodeReward = 0,
                            trainingEpisodeTicks = 0,
                            trainingEpisodeEpsilon = epsilon,
                            trainingEpisodeDied = True
                }
            )
            | otherwise -> trainingLoop 0 0 initialState (encodeState initialState worm) initialQTable
  where
    controlledId = trainingWormId config

    -- | Repeatedly performs Q-learning steps until the episode terminates.
    trainingLoop :: Int -> Double -> GameState -> RLState -> QTable -> IO (QTable, TrainingEpisodeStats)
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
            action <- chooseActionEpsilonGreedy epsilon qTable currentRlState
            step <- stepEnvironmentWithOpponents controlledId action opponentControllers currentState
            let reward = rlReward step
                updatedQTable = updateQValue (learningRate config) (discountFactor config) currentRlState action reward (rlNextRlState step) qTable
                newTotalReward = totalReward + reward
                newTicks = ticks + 1
            if rlDone step
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
                    case rlNextRlState step of
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
                        Just nextRlState -> trainingLoop newTicks newTotalReward (rlNextGameState step) nextRlState updatedQTable


-- -----------------------------------------------------------------------------
-- Multi-episode training
-- -----------------------------------------------------------------------------

-- | Trains a Q-learning agent over all configured training episodes.
--
-- The learned Q-table is carried from one episode to the next, while epsilon gradually decreases according to the configured decay schedule.
trainEpisodes :: TrainingConfig -> [(Int, Controller)] -> GameState -> IO (QTable, [TrainingEpisodeStats])
trainEpisodes config opponentControllers initialState =
    trainingLoop 1 (epsilonStart config) emptyQTable []
  where
    trainingLoop :: Int -> Double -> QTable -> [TrainingEpisodeStats] -> IO (QTable, [TrainingEpisodeStats])
    trainingLoop episodeNumber epsilon qTable collectedStats
        | episodeNumber > trainingEpisodes config = pure (qTable, reverse collectedStats)
        | otherwise = do 
            (updatedQTable, episodeStats) <- trainEpisode config episodeNumber opponentControllers initialState epsilon qTable
            let updatedStats = episodeStats : collectedStats
                interval = progressInterval config
                shouldPrintProgress = interval > 0 && ( episodeNumber `mod` interval == 0 || episodeNumber == trainingEpisodes config)
                recentStats = take interval updatedStats
            if shouldPrintProgress
                then
                    printTrainingProgress episodeNumber (trainingEpisodes config) recentStats
                else pure()
            trainingLoop (episodeNumber + 1) (nextEpsilon config epsilon) updatedQTable (episodeStats : collectedStats)


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