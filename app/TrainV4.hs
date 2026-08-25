module Main (main) where

import Training
import TrainingEnvironment

import qualified QLearning.V4 as V4


-- | Trains and saves the diverse Q-learning V4 agent.
main :: IO ()
main = do
    let v4ReformedConfig =
            defaultTrainingConfig { trainingEpisodes = 30000, maxEpisodeTicks = 500, learningRate = 0.1, discountFactor = 0.9, epsilonStart = 1.0, epsilonMinimum = 0.05, epsilonDecay = 0.99985, progressInterval = 200 }

    (v4ReformedTable, v4ReformedStats) <- trainEpisodesWithSampler V4.v4Spec v4ReformedConfig (mixedTrainingSampler 1)


    V4.saveQTable "data/models/qlearning_v4_reformed_diverse_30k.txt" v4ReformedTable


    writeFile "data/models/qlearning_v4_reformed_diverse_30k_stats.txt" (show v4ReformedStats)

