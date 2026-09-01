{-|
Module      : Main
Description : Training executable for the final diverse CerviQ V4 model.

This executable configures the final 30,000-episode V4 training run, uses the
diverse environment sampler from "TrainingEnvironment", and saves both the
resulting Q-table and raw per-episode statistics.

The training algorithm itself is implemented in "Training" and
"QLearning.Core"; this module only specifies one concrete experiment.
-}

module Main (main) where

import Training
import TrainingEnvironment

import qualified QLearning.V4 as V4


-- | Configuration of the final 30,000-episode V4 training run.
v4TrainingConfig :: TrainingConfig
v4TrainingConfig =
    defaultTrainingConfig
        { trainingEpisodes = 30000
        , maxEpisodeTicks = 500
        , learningRate = 0.1
        , discountFactor = 0.9
        , epsilonStart = 1.0
        , epsilonMinimum = 0.05
        , epsilonDecay = 0.99985
        , progressInterval = 200
        }


-- | File containing the learned final V4 Q-table.
v4ModelPath :: FilePath
v4ModelPath =
    "data/models/qlearning_v4_reformed_diverse_30k.txt"


-- | File containing raw statistics from every training episode.
v4StatsPath :: FilePath
v4StatsPath =
    "data/models/qlearning_v4_reformed_diverse_30k_stats.txt"


-- | Trains the final diverse V4 model and saves its outputs.
main :: IO ()
main = do
    (qTable, trainingStats) <-
        trainEpisodesWithSampler
            V4.v4Spec
            v4TrainingConfig
            (mixedTrainingSampler (trainingWormId v4TrainingConfig))

    V4.saveQTable v4ModelPath qTable
    writeFile v4StatsPath (show trainingStats)