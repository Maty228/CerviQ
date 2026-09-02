{-|
Module      : Main
Description : Command-line pairwise evaluation of CerviQ agents.

This executable runs controlled two-worm benchmarks between selectable agents.
An agent may be referenced by its registered GUI name or by a direct Q-table
file reference such as @v4:data/models/demo_v4.txt@.

Evaluation deliberately uses the original two-worm scenario configuration and
the fixed training/evaluation food target rather than the adaptive multi-worm
interactive food system. Initial food and the two starting sides are randomized
for every episode to reduce dependence on one fixed setup.
-}

module Main (main) where

import AgentRegistry
import Evaluation
import Scenario
import Scenarios
import TrainingEnvironment
import Types

import qualified QLearning.V1 as V1
import qualified QLearning.V2 as V2
import qualified QLearning.V3 as V3
import qualified QLearning.V4 as V4

import Control.Monad (replicateM)
import Data.Char (toLower)
import Data.List (find)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Text.Printf (printf)
import Text.Read (readMaybe)


-- -----------------------------------------------------------------------------
-- Evaluation options
-- -----------------------------------------------------------------------------

-- | Complete configuration of one pairwise evaluation run.
data EvaluationOptions = EvaluationOptions
    { optionAgentSource :: Maybe String
    , optionOpponentSource :: Maybe String
    , optionScenarioName :: String
    , optionEpisodes :: Int
    , optionMaxTicks :: Int
    , optionOutputPath :: Maybe FilePath
    }
    deriving (Show, Eq)


-- | Top-level evaluator command.
data EvaluationCommand
    = EvaluationHelp
    | EvaluationListAgents
    | EvaluationListScenarios
    | EvaluationRun EvaluationOptions


-- | Default pairwise evaluation settings.
defaultEvaluationOptions :: EvaluationOptions
defaultEvaluationOptions =
    EvaluationOptions
        { optionAgentSource = Nothing
        , optionOpponentSource = Nothing
        , optionScenarioName = "Arena"
        , optionEpisodes = 100
        , optionMaxTicks = 500
        , optionOutputPath = Nothing
        }


-- | Parses the top-level evaluator command.
parseEvaluationCommand :: [String] -> Either String EvaluationCommand
parseEvaluationCommand args
    | null args = Right EvaluationHelp
    | "--help" `elem` args = Right EvaluationHelp
    | args == ["--list-agents"] = Right EvaluationListAgents
    | args == ["--list-scenarios"] = Right EvaluationListScenarios
    | otherwise = do
        options <-
            parseEvaluationOptions
                defaultEvaluationOptions
                args

        validateEvaluationOptions options
        pure (EvaluationRun options)


-- | Parses options of a pairwise evaluation run.
parseEvaluationOptions :: EvaluationOptions -> [String] -> Either String EvaluationOptions
parseEvaluationOptions options [] =
    Right options

parseEvaluationOptions options ("--agent" : value : remaining) =
    parseEvaluationOptions options {optionAgentSource = Just value} remaining

parseEvaluationOptions options ("--opponent" : value : remaining) =
    parseEvaluationOptions options {optionOpponentSource = Just value} remaining

parseEvaluationOptions options ("--scenario" : value : remaining) =
    parseEvaluationOptions options {optionScenarioName = value} remaining

parseEvaluationOptions options ("--episodes" : value : remaining) = do
    parsed <- parsePositiveInt "--episodes" value
    parseEvaluationOptions options {optionEpisodes = parsed} remaining

parseEvaluationOptions options ("--max-ticks" : value : remaining) = do
    parsed <- parsePositiveInt "--max-ticks" value
    parseEvaluationOptions options {optionMaxTicks = parsed} remaining

parseEvaluationOptions options ("--output" : value : remaining) =
    parseEvaluationOptions options {optionOutputPath = Just value} remaining

parseEvaluationOptions _ [flag] =
    Left ("Missing value after " ++ flag ++ ".")

parseEvaluationOptions _ (flag : _) =
    Left ("Unknown evaluation option: " ++ flag)


-- | Parses a strictly positive integer evaluator argument.
parsePositiveInt :: String -> String -> Either String Int
parsePositiveInt optionName value =
    case readMaybe value of
        Just parsed
            | parsed > 0 -> Right parsed

        _ ->
            Left (optionName ++ " must be a positive integer.")


-- | Ensures both compared agents were supplied.
validateEvaluationOptions :: EvaluationOptions -> Either String ()
validateEvaluationOptions options =
    case (optionAgentSource options, optionOpponentSource options) of
        (Nothing, _) ->
            Left "Missing required --agent SOURCE."

        (_, Nothing) ->
            Left "Missing required --opponent SOURCE."

        _ ->
            Right ()


-- -----------------------------------------------------------------------------
-- Lookup helpers
-- -----------------------------------------------------------------------------

-- | Normalizes a user-facing identifier for case-insensitive lookup.
normalizeName :: String -> String
normalizeName =
    map toLower


-- | Finds a scenario by name without requiring matching capitalization.
findScenario :: String -> Maybe Scenario
findScenario targetName =
    find
        (\scenario -> normalizeName (scenarioName scenario) == normalizeName targetName)
        allScenarios


-- | Interprets a source as a direct model reference when it has a known prefix.
parseModelReference :: String -> Maybe (String, FilePath)
parseModelReference source =
    case break (== ':') source of
        (prefix, ':' : path)
            | normalizeName prefix `elem` supportedPrefixes
            , not (null path) ->
                Just (normalizeName prefix, path)

        _ ->
            Nothing
  where
    supportedPrefixes =
        ["v1", "v2", "v3", "v3f", "v4", "v4f"]


-- | Resolves either a registered agent name or a direct Q-table file reference.
resolveAgentSource :: [AgentOption] -> String -> IO (Either String (String, Controller))
resolveAgentSource registeredOptions source =
    case parseModelReference source of
        Just ("v1", path) -> do
            table <- V1.loadQTable path
            pure $ Right ("Q-learning V1 [" ++ path ++ "]", AI (V1.qLearningAgent table))

        Just ("v2", path) -> do
            table <- V2.loadQTable path
            pure $ Right ("Q-learning V2 [" ++ path ++ "]", AI (V2.qLearningAgent table))

        Just ("v3", path) -> do
            table <- V3.loadQTable path
            pure $ Right ("Q-learning V3 [" ++ path ++ "]", AI (V3.qLearningAgent table))

        Just ("v3f", path) -> do
            table <- V3.loadQTable path
            pure $ Right ("Q-learning V3 + fallback [" ++ path ++ "]", AI (V3.qLearningAgentWithFallback table))

        Just ("v4", path) -> do
            table <- V4.loadQTable path
            pure $ Right ("Q-learning V4 [" ++ path ++ "]", AI (V4.qLearningAgent table))

        Just ("v4f", path) -> do
            table <- V4.loadQTable path
            pure $ Right ("Q-learning V4 + fallback [" ++ path ++ "]", AI (V4.qLearningAgentWithFallback table))

        Just _ ->
            pure $ Left ("Unsupported model reference: " ++ source)

        Nothing ->
            pure $
                case
                    find
                        (\option -> normalizeName (agentOptionName option) == normalizeName source)
                        registeredOptions
                of
                    Just option ->
                        Right
                            ( agentOptionName option
                            , agentOptionController option
                            )

                    Nothing ->
                        Left
                            ( "Unknown agent \""
                                ++ source
                                ++ "\". Use --list-agents or a direct model reference such as v4:path."
                            )


-- -----------------------------------------------------------------------------
-- Evaluation execution
-- -----------------------------------------------------------------------------

-- | Runs one randomized benchmark episode using the original two-worm scenario.
runRandomizedEpisode :: Int -> [(Int, Controller)] -> Scenario -> IO EpisodeResult
runRandomizedEpisode maxTicks controllers scenario = do
    let baseState =
            scenarioInitialState scenario

    (startState, _) <-
        randomizeStartVariant baseState

    initialState <-
        randomizeInitialFood startState

    runEpisode
        maxTicks
        controllers
        initialState


-- | Renders one per-worm evaluation summary.
renderWormSummary :: String -> WormEvaluationSummary -> [String]
renderWormSummary label summary =
    [ label
    , replicate (length label) '-'
    , "Worm ID:               " ++ show (evaluatedWormId summary)
    , "Average food:          " ++ printf "%.3f" (wormAvgFoodEaten summary)
    , "Food / 100 ticks:      " ++ printf "%.3f" (wormFoodPer100Ticks summary)
    , "Average age:           " ++ printf "%.2f" (wormAvgAge summary)
    , "Median age:            " ++ printf "%.2f" (wormMedianAge summary)
    , "P90 age:               " ++ show (wormP90Age summary)
    , "Maximum age:           " ++ show (wormMaxAge summary)
    , "Average kills:         " ++ printf "%.3f" (wormAvgKills summary)
    , "Average final length:  " ++ printf "%.3f" (wormAvgFinalLength summary)
    , "Maximum final length:  " ++ show (wormMaxFinalLength summary)
    , "Survival rate:         " ++ printf "%.2f%%" (100 * wormSurvivalRate summary)
    , "Last-standing rate:    " ++ printf "%.2f%%" (100 * wormLastStandingRate summary)
    , "Wall death rate:       " ++ printf "%.2f%%" (100 * wormWallDeathRate summary)
    , "Poison death rate:     " ++ printf "%.2f%%" (100 * wormPoisonDeathRate summary)
    , "Own-body death rate:   " ++ printf "%.2f%%" (100 * wormOwnBodyDeathRate summary)
    , "Other-body death rate: " ++ printf "%.2f%%" (100 * wormOtherBodyDeathRate summary)
    , "Head-to-head rate:     " ++ printf "%.2f%%" (100 * wormHeadToHeadDeathRate summary)
    ]


-- | Builds the complete human-readable report of one pairwise benchmark.
renderEvaluationReport :: String -> String -> Scenario -> EvaluationOptions -> [EpisodeResult] -> String
renderEvaluationReport agentName opponentName scenario options results =
    unlines
        ( [ "CerviQ pairwise evaluation"
          , "==========================="
          , ""
          , "Agent:                  " ++ agentName
          , "Opponent:               " ++ opponentName
          , "Scenario:               " ++ scenarioName scenario
          , "Episodes:               " ++ show (optionEpisodes options)
          , "Maximum ticks:          " ++ show (optionMaxTicks options)
          , "Starting configuration: original two-worm scenario"
          , "Start side:             randomized each episode"
          , "Initial food:           randomized fixed evaluation target"
          , ""
          , "Global episode statistics"
          , "-------------------------"
          , "Average episode length: " ++ printf "%.2f" (avgEpisodeLength globalSummary)
          , "Average total food:     " ++ printf "%.3f" (avgFoodEaten globalSummary)
          , "Average total kills:    " ++ printf "%.3f" (avgKills globalSummary)
          , ""
          ]
            ++ renderWormSummary ("Agent: " ++ agentName) agentSummary
            ++ [""]
            ++ renderWormSummary ("Opponent: " ++ opponentName) opponentSummary
        )
  where
    globalSummary =
        summarizeResults results

    agentSummary =
        summarizeWormResults 1 results

    opponentSummary =
        summarizeWormResults 2 results


-- | Executes one configured two-agent benchmark.
runEvaluation :: EvaluationOptions -> IO ()
runEvaluation options = do
    registeredAgents <-
        loadAgentOptions

    case
        ( optionAgentSource options
        , optionOpponentSource options
        , findScenario (optionScenarioName options)
        )
        of
        (Just agentSource, Just opponentSource, Just scenario) -> do
            maybeAgent <-
                resolveAgentSource registeredAgents agentSource

            maybeOpponent <-
                resolveAgentSource registeredAgents opponentSource

            case (maybeAgent, maybeOpponent) of
                (Left message, _) ->
                    failWithUsage message

                (_, Left message) ->
                    failWithUsage message

                (Right (agentName, agentController), Right (opponentName, opponentController)) -> do
                    putStrLn "Running CerviQ pairwise evaluation..."
                    putStrLn $ "  Agent:     " ++ agentName
                    putStrLn $ "  Opponent:  " ++ opponentName
                    putStrLn $ "  Scenario:  " ++ scenarioName scenario
                    putStrLn $ "  Episodes:  " ++ show (optionEpisodes options)
                    putStrLn ""

                    results <-
                        replicateM
                            (optionEpisodes options)
                            ( runRandomizedEpisode
                                (optionMaxTicks options)
                                [ (1, agentController)
                                , (2, opponentController)
                                ]
                                scenario
                            )

                    let report =
                            renderEvaluationReport
                                agentName
                                opponentName
                                scenario
                                options
                                results

                    putStrLn ""
                    putStrLn report

                    case optionOutputPath options of
                        Nothing ->
                            pure ()

                        Just path -> do
                            writeFile path report
                            putStrLn $ "Saved report to " ++ path

        (_, _, Nothing) ->
            failWithUsage
                ( "Unknown scenario \""
                    ++ optionScenarioName options
                    ++ "\". Use --list-scenarios."
                )

        _ ->
            failWithUsage "Both --agent and --opponent are required."


-- -----------------------------------------------------------------------------
-- Listing and command-line interface
-- -----------------------------------------------------------------------------

-- | Prints every registered GUI agent name accepted by --agent and --opponent.
printRegisteredAgents :: IO ()
printRegisteredAgents = do
    agents <-
        loadAgentOptions

    putStrLn "Registered agents:"
    mapM_ (putStrLn . ("  " ++) . agentOptionName) agents
    putStrLn ""
    putStrLn "Direct Q-table references are also supported:"
    putStrLn "  v1:path"
    putStrLn "  v2:path"
    putStrLn "  v3:path"
    putStrLn "  v3f:path    V3 with fallback"
    putStrLn "  v4:path"
    putStrLn "  v4f:path    V4 with fallback"


-- | Prints every built-in scenario accepted by --scenario.
printScenarios :: IO ()
printScenarios = do
    putStrLn "Scenarios:"
    mapM_ (putStrLn . ("  " ++) . scenarioName) allScenarios


-- | Prints command-line usage.
printUsage :: IO ()
printUsage = do
    putStrLn "Usage:"
    putStrLn "  stack run cerviq-evaluate -- --agent SOURCE --opponent SOURCE [OPTIONS]"
    putStrLn ""
    putStrLn "Options:"
    putStrLn "  --agent SOURCE        Registered name or model reference"
    putStrLn "  --opponent SOURCE     Registered name or model reference"
    putStrLn "  --scenario NAME       Scenario name (default Arena)"
    putStrLn "  --episodes N          Number of episodes (default 100)"
    putStrLn "  --max-ticks N         Maximum ticks per episode (default 500)"
    putStrLn "  --output PATH         Save the text report"
    putStrLn "  --list-agents         List registered agents"
    putStrLn "  --list-scenarios      List scenarios"
    putStrLn "  --help                Show this help"
    putStrLn ""
    putStrLn "Examples:"
    putStrLn "  stack run cerviq-evaluate -- --agent \"Q-learning V4 Reformed 30k + fallback\" --opponent \"Safe Greedy Food\" --scenario Arena --episodes 100"
    putStrLn ""
    putStrLn "  stack run cerviq-evaluate -- --agent v4f:data/models/demo_v4.txt --opponent \"Safe Hunter\" --scenario Cave --episodes 100"


-- | Prints an evaluator error followed by command usage and exits unsuccessfully.
failWithUsage :: String -> IO ()
failWithUsage message = do
    putStrLn $ "Error: " ++ message
    putStrLn ""
    printUsage
    exitFailure


-- | Runs the pairwise evaluation command-line application.
main :: IO ()
main = do
    args <-
        getArgs

    case parseEvaluationCommand args of
        Left message ->
            failWithUsage message

        Right EvaluationHelp ->
            printUsage

        Right EvaluationListAgents ->
            printRegisteredAgents

        Right EvaluationListScenarios ->
            printScenarios

        Right (EvaluationRun options) ->
            runEvaluation options