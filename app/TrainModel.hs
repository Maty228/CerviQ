{-|
Module      : Main
Description : Configurable command-line runner for CerviQ Q-learning training.

This executable provides a lightweight interface for short experiments,
demonstrations, and configurable custom training runs across Q-learning
versions V1-V4. Runs still use process-global randomness and do not expose a
seed, so identical command-line options do not guarantee identical tables.

The dedicated TrainV4 executable remains the historical recipe used to train
the final V4 model. This runner is intentionally separate so experimental
settings cannot silently replace that recorded configuration.

Each run writes the trained Q-table together with raw per-episode statistics,
CSV statistics, and a compact human-readable summary.
-}

module Main (main) where

import Training
import TrainingEnvironment

import qualified QLearning.Core as Core
import qualified QLearning.V1 as V1
import qualified QLearning.V2 as V2
import qualified QLearning.V3 as V3
import qualified QLearning.V4 as V4

import Data.Char (toLower)
import Data.List (isSuffixOf)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Text.Printf (printf)
import Text.Read (readMaybe)


-- -----------------------------------------------------------------------------
-- Model selection
-- -----------------------------------------------------------------------------

-- | Q-learning implementation that should be trained.
data ModelVersion
    = ModelV1
    | ModelV2
    | ModelV3
    | ModelV4
    deriving (Show, Eq)


-- | Parses a short command-line model identifier.
parseModelVersion :: String -> Maybe ModelVersion
parseModelVersion value =
    case map toLower value of
        "v1" -> Just ModelV1
        "v2" -> Just ModelV2
        "v3" -> Just ModelV3
        "v4" -> Just ModelV4
        _ -> Nothing


-- | Returns a short lowercase identifier suitable for file names.
modelVersionSlug :: ModelVersion -> String
modelVersionSlug ModelV1 = "v1"
modelVersionSlug ModelV2 = "v2"
modelVersionSlug ModelV3 = "v3"
modelVersionSlug ModelV4 = "v4"


-- | Returns a human-readable model version name.
modelVersionName :: ModelVersion -> String
modelVersionName ModelV1 = "Q-learning V1"
modelVersionName ModelV2 = "Q-learning V2"
modelVersionName ModelV3 = "Q-learning V3"
modelVersionName ModelV4 = "Q-learning V4"


-- -----------------------------------------------------------------------------
-- Training options
-- -----------------------------------------------------------------------------

-- | Complete command-line configuration of one experimental training run.
data TrainOptions = TrainOptions
    { optionModelVersion :: ModelVersion
    , optionEpisodes :: Int
    , optionMaxTicks :: Int
    , optionLearningRate :: Double
    , optionDiscountFactor :: Double
    , optionEpsilonStart :: Double
    , optionEpsilonMinimum :: Double
    , optionEpsilonDecay :: Double
    , optionProgressInterval :: Int
    , optionOutputPath :: FilePath
    }
    deriving (Show, Eq)


-- | Top-level command selected by the command-line arguments.
data TrainCommand
    = TrainHelp
    | TrainRun TrainOptions


-- | Creates safe experimental defaults without overwriting any final model.
defaultTrainOptions :: ModelVersion -> TrainOptions
defaultTrainOptions version =
    TrainOptions
        { optionModelVersion = version
        , optionEpisodes = 1000
        , optionMaxTicks = maxEpisodeTicks defaultTrainingConfig
        , optionLearningRate = learningRate defaultTrainingConfig
        , optionDiscountFactor = discountFactor defaultTrainingConfig
        , optionEpsilonStart = epsilonStart defaultTrainingConfig
        , optionEpsilonMinimum = epsilonMinimum defaultTrainingConfig
        , optionEpsilonDecay = epsilonDecay defaultTrainingConfig
        , optionProgressInterval = 100
        , optionOutputPath = "data/models/demo_" ++ modelVersionSlug version ++ ".txt"
        }


-- | Parses the complete training command.
parseTrainCommand :: [String] -> Either String TrainCommand
parseTrainCommand [] =
    Right TrainHelp

parseTrainCommand args
    | "--help" `elem` args = Right TrainHelp

parseTrainCommand (versionArgument : remaining) =
    case parseModelVersion versionArgument of
        Nothing ->
            Left "First argument must be one of: v1, v2, v3, v4."

        Just version -> do
            options <-
                parseTrainOptions
                    (defaultTrainOptions version)
                    remaining

            validateTrainOptions options
            pure (TrainRun options)


-- | Parses optional training flags after the model version.
parseTrainOptions :: TrainOptions -> [String] -> Either String TrainOptions
parseTrainOptions options [] =
    Right options

parseTrainOptions options ("--episodes" : value : remaining) = do
    parsed <- parsePositiveInt "--episodes" value
    parseTrainOptions options {optionEpisodes = parsed} remaining

parseTrainOptions options ("--max-ticks" : value : remaining) = do
    parsed <- parsePositiveInt "--max-ticks" value
    parseTrainOptions options {optionMaxTicks = parsed} remaining

parseTrainOptions options ("--alpha" : value : remaining) = do
    parsed <- parseDouble "--alpha" value
    parseTrainOptions options {optionLearningRate = parsed} remaining

parseTrainOptions options ("--gamma" : value : remaining) = do
    parsed <- parseDouble "--gamma" value
    parseTrainOptions options {optionDiscountFactor = parsed} remaining

parseTrainOptions options ("--epsilon-start" : value : remaining) = do
    parsed <- parseDouble "--epsilon-start" value
    parseTrainOptions options {optionEpsilonStart = parsed} remaining

parseTrainOptions options ("--epsilon-min" : value : remaining) = do
    parsed <- parseDouble "--epsilon-min" value
    parseTrainOptions options {optionEpsilonMinimum = parsed} remaining

parseTrainOptions options ("--epsilon-decay" : value : remaining) = do
    parsed <- parseDouble "--epsilon-decay" value
    parseTrainOptions options {optionEpsilonDecay = parsed} remaining

parseTrainOptions options ("--progress" : value : remaining) = do
    parsed <- parseNonNegativeInt "--progress" value
    parseTrainOptions options {optionProgressInterval = parsed} remaining

parseTrainOptions options ("--output" : value : remaining) =
    parseTrainOptions options {optionOutputPath = value} remaining

parseTrainOptions _ [flag] =
    Left ("Missing value after " ++ flag ++ ".")

parseTrainOptions _ (flag : _) =
    Left ("Unknown training option: " ++ flag)


-- | Parses a strictly positive integer command-line value.
parsePositiveInt :: String -> String -> Either String Int
parsePositiveInt optionName value =
    case readMaybe value of
        Just parsed
            | parsed > 0 -> Right parsed

        _ ->
            Left (optionName ++ " must be a positive integer.")


-- | Parses a non-negative integer command-line value.
parseNonNegativeInt :: String -> String -> Either String Int
parseNonNegativeInt optionName value =
    case readMaybe value of
        Just parsed
            | parsed >= 0 -> Right parsed

        _ ->
            Left (optionName ++ " must be a non-negative integer.")


-- | Parses a floating-point command-line value.
parseDouble :: String -> String -> Either String Double
parseDouble optionName value =
    case readMaybe value of
        Just parsed -> Right parsed
        Nothing -> Left (optionName ++ " must be a number.")


-- | Validates relationships and ranges between parsed training settings.
validateTrainOptions :: TrainOptions -> Either String ()
validateTrainOptions options
    | optionLearningRate options <= 0 || optionLearningRate options > 1 =
        Left "--alpha must be in the interval (0, 1]."

    | optionDiscountFactor options < 0 || optionDiscountFactor options > 1 =
        Left "--gamma must be in the interval [0, 1]."

    | optionEpsilonStart options < 0 || optionEpsilonStart options > 1 =
        Left "--epsilon-start must be in the interval [0, 1]."

    | optionEpsilonMinimum options < 0 || optionEpsilonMinimum options > 1 =
        Left "--epsilon-min must be in the interval [0, 1]."

    | optionEpsilonMinimum options > optionEpsilonStart options =
        Left "--epsilon-min must not exceed --epsilon-start."

    | optionEpsilonDecay options <= 0 || optionEpsilonDecay options > 1 =
        Left "--epsilon-decay must be in the interval (0, 1]."

    | null (optionOutputPath options) =
        Left "--output must not be empty."

    | otherwise =
        Right ()


-- | Converts command-line settings into the existing training configuration.
trainingConfigFromOptions :: TrainOptions -> TrainingConfig
trainingConfigFromOptions options =
    defaultTrainingConfig
        { trainingEpisodes = optionEpisodes options
        , maxEpisodeTicks = optionMaxTicks options
        , learningRate = optionLearningRate options
        , discountFactor = optionDiscountFactor options
        , epsilonStart = optionEpsilonStart options
        , epsilonMinimum = optionEpsilonMinimum options
        , epsilonDecay = optionEpsilonDecay options
        , progressInterval = optionProgressInterval options
        }


-- -----------------------------------------------------------------------------
-- Output helpers
-- -----------------------------------------------------------------------------

-- | Removes a final .txt extension when deriving related output file names.
outputStem :: FilePath -> FilePath
outputStem path
    | ".txt" `isSuffixOf` path =
        take (length path - 4) path

    | otherwise =
        path


-- | Returns the raw per-episode statistics path for one model output.
rawStatsPath :: TrainOptions -> FilePath
rawStatsPath options =
    outputStem (optionOutputPath options) ++ "_stats.txt"


-- | Returns the CSV per-episode statistics path for one model output.
csvStatsPath :: TrainOptions -> FilePath
csvStatsPath options =
    outputStem (optionOutputPath options) ++ "_stats.csv"


-- | Returns the human-readable summary path for one model output.
summaryPath :: TrainOptions -> FilePath
summaryPath options =
    outputStem (optionOutputPath options) ++ "_summary.txt"


-- | Escapes one value for a simple CSV file.
csvField :: String -> String
csvField value
    | any (`elem` [',', '"', '\n']) value =
        "\"" ++ concatMap escapeCharacter value ++ "\""

    | otherwise =
        value
  where
    -- | Escapes a quote inside a CSV field.
    escapeCharacter :: Char -> String
    escapeCharacter '"' = "\"\""
    escapeCharacter character = [character]


-- | Converts collected training statistics into a portable CSV representation.
trainingStatsCsv :: [TrainingEpisodeStats] -> String
trainingStatsCsv stats =
    unlines
        ( header
            : map trainingStatRow stats
        )
  where
    header =
        "episode,reward,ticks,epsilon,died,food_eaten,kills,final_length,last_standing,scenario,opponent,start_variant"

    -- | Converts one training episode into a CSV row.
    trainingStatRow :: TrainingEpisodeStats -> String
    trainingStatRow stat =
        concatWithComma
            [ show (trainingEpisode stat)
            , show (trainingEpisodeReward stat)
            , show (trainingEpisodeTicks stat)
            , show (trainingEpisodeEpsilon stat)
            , show (trainingEpisodeDied stat)
            , show (trainingEpisodeFoodEaten stat)
            , show (trainingEpisodeKills stat)
            , show (trainingEpisodeFinalLength stat)
            , show (trainingEpisodeLastStanding stat)
            , csvField (trainingEpisodeScenario stat)
            , csvField (trainingEpisodeOpponent stat)
            , csvField (trainingEpisodeStartVariant stat)
            ]

    -- | Joins already formatted CSV values with commas.
    concatWithComma :: [String] -> String
    concatWithComma [] = ""
    concatWithComma [value] = value
    concatWithComma (value : remaining) =
        value ++ "," ++ concatWithComma remaining


-- | Builds a compact summary of a finished training run.
renderTrainingSummary :: Ord state => TrainOptions -> TrainingConfig -> Core.QTable state -> [TrainingEpisodeStats] -> Double -> String
renderTrainingSummary options config qTable stats elapsedSeconds =
    unlines
        [ "CerviQ training summary"
        , "======================="
        , ""
        , "Model:                 " ++ modelVersionName (optionModelVersion options)
        , "Episodes:              " ++ show (trainingEpisodes config)
        , "Max ticks / episode:   " ++ show (maxEpisodeTicks config)
        , "Learning rate alpha:   " ++ show (learningRate config)
        , "Discount gamma:        " ++ show (discountFactor config)
        , "Epsilon start:         " ++ show (epsilonStart config)
        , "Epsilon minimum:       " ++ show (epsilonMinimum config)
        , "Epsilon decay:         " ++ show (epsilonDecay config)
        , "Progress interval:     " ++ show (progressInterval config)
        , ""
        , "Average reward:        " ++ printf "%.3f" (averageTrainingReward stats)
        , "Median reward:         " ++ printf "%.3f" (medianTrainingReward stats)
        , "Average ticks:         " ++ printf "%.2f" (averageTrainingTicks stats)
        , "Median ticks:          " ++ printf "%.2f" (medianTrainingTicks stats)
        , "P90 ticks:             " ++ printf "%.2f" (p90TrainingTicks stats)
        , "Survival rate:         " ++ printf "%.2f%%" (100 * trainingSurvivalRate stats)
        , "Average food:          " ++ printf "%.3f" (averageTrainingFood stats)
        , "Food / 100 ticks:      " ++ printf "%.3f" (trainingFoodPer100Ticks stats)
        , "Average kills:         " ++ printf "%.3f" (averageTrainingKills stats)
        , "Average final length:  " ++ printf "%.3f" (averageTrainingFinalLength stats)
        , "Last-standing rate:    " ++ printf "%.2f%%" (100 * trainingLastStandingRate stats)
        , ""
        , "Q-table states:        " ++ show (qTableStateCount qTable)
        , "Q-table entries:       " ++ show (qTableEntryCount qTable)
        , "Elapsed time:          " ++ formatDuration elapsedSeconds
        , ""
        , "Scenarios:             " ++ formatTrainingLabelCounts (trainingLabelCounts trainingEpisodeScenario stats)
        , "Opponents:             " ++ formatTrainingLabelCounts (trainingLabelCounts trainingEpisodeOpponent stats)
        , "Start variants:        " ++ formatTrainingLabelCounts (trainingLabelCounts trainingEpisodeStartVariant stats)
        , ""
        , "Model file:            " ++ optionOutputPath options
        , "Raw statistics:        " ++ rawStatsPath options
        , "CSV statistics:        " ++ csvStatsPath options
        , "Summary:               " ++ summaryPath options
        ]


-- | Prints the selected training configuration before the expensive run begins.
printTrainingConfiguration :: TrainOptions -> TrainingConfig -> IO ()
printTrainingConfiguration options config = do
    putStrLn "CerviQ configurable training"
    putStrLn "============================"
    putStrLn $ "Model:             " ++ modelVersionName (optionModelVersion options)
    putStrLn $ "Episodes:          " ++ show (trainingEpisodes config)
    putStrLn $ "Max ticks:         " ++ show (maxEpisodeTicks config)
    putStrLn $ "Alpha:             " ++ show (learningRate config)
    putStrLn $ "Gamma:             " ++ show (discountFactor config)
    putStrLn $ "Epsilon start:     " ++ show (epsilonStart config)
    putStrLn $ "Epsilon minimum:   " ++ show (epsilonMinimum config)
    putStrLn $ "Epsilon decay:     " ++ show (epsilonDecay config)
    putStrLn $ "Progress interval: " ++ show (progressInterval config)
    putStrLn $ "Output model:      " ++ optionOutputPath options
    putStrLn ""


-- -----------------------------------------------------------------------------
-- Training execution
-- -----------------------------------------------------------------------------

-- | Trains one concrete Q-learning state representation and writes all outputs.
trainVersion :: Ord state => TrainOptions -> Core.QLearningSpec state -> (FilePath -> Core.QTable state -> IO ()) -> IO ()
trainVersion options spec saveTable = do
    let config =
            trainingConfigFromOptions options

    printTrainingConfiguration options config

    startTime <-
        getCurrentTime

    (qTable, stats) <-
        trainEpisodesWithSampler
            spec
            config
            (mixedTrainingSampler (trainingWormId config))

    endTime <-
        getCurrentTime

    let elapsedSeconds =
            realToFrac (diffUTCTime endTime startTime)

        summary =
            renderTrainingSummary
                options
                config
                qTable
                stats
                elapsedSeconds

    saveTable
        (optionOutputPath options)
        qTable

    writeFile
        (rawStatsPath options)
        (show stats ++ "\n")

    writeFile
        (csvStatsPath options)
        (trainingStatsCsv stats)

    writeFile
        (summaryPath options)
        summary

    putStrLn ""
    putStrLn summary


-- | Dispatches training to the selected Q-learning implementation.
runTraining :: TrainOptions -> IO ()
runTraining options =
    case optionModelVersion options of
        ModelV1 ->
            trainVersion options V1.v1Spec V1.saveQTable

        ModelV2 ->
            trainVersion options V2.v2Spec V2.saveQTable

        ModelV3 ->
            trainVersion options V3.v3Spec V3.saveQTable

        ModelV4 ->
            trainVersion options V4.v4Spec V4.saveQTable


-- -----------------------------------------------------------------------------
-- Command-line interface
-- -----------------------------------------------------------------------------

-- | Prints usage and available experimental hyperparameters.
printUsage :: IO ()
printUsage = do
    putStrLn "Usage:"
    putStrLn "  stack run cerviq-train -- VERSION [OPTIONS]"
    putStrLn ""
    putStrLn "Versions:"
    putStrLn "  v1 | v2 | v3 | v4"
    putStrLn ""
    putStrLn "Options:"
    putStrLn "  --episodes N          Number of training episodes (default 1000)"
    putStrLn "  --max-ticks N         Maximum ticks per episode (default 500)"
    putStrLn "  --alpha X             Learning rate"
    putStrLn "  --gamma X             Discount factor"
    putStrLn "  --epsilon-start X     Initial exploration probability"
    putStrLn "  --epsilon-min X       Minimum exploration probability"
    putStrLn "  --epsilon-decay X     Multiplicative epsilon decay"
    putStrLn "  --progress N          Progress interval; 0 disables output"
    putStrLn "  --output PATH         Q-table output path"
    putStrLn "  --help                Show this help"
    putStrLn ""
    putStrLn "Example:"
    putStrLn "  stack run cerviq-train -- v4 --episodes 500 --output data/models/demo_v4.txt"


-- | Prints an argument error followed by command usage and exits unsuccessfully.
failWithUsage :: String -> IO ()
failWithUsage message = do
    putStrLn $ "Error: " ++ message
    putStrLn ""
    printUsage
    exitFailure


-- | Runs the configurable training command-line application.
main :: IO ()
main = do
    args <-
        getArgs

    case parseTrainCommand args of
        Left message ->
            failWithUsage message

        Right TrainHelp ->
            printUsage

        Right (TrainRun options) ->
            runTraining options
