{-|
Module      : AgentRegistry
Description : Loading and registration of agents available in the CerviQ GUI.

This module is the central catalogue of agents selectable from the application
menus. It registers heuristic agents directly and loads the persisted Q-tables
needed by trained agents. Each selectable option also carries optional reward
and debug information used by the visualization interface.

Model loading is performed once when the application starts, allowing the same
tables to be reused by subsequently created games.
-}

module AgentRegistry where

import Agent
import Gui
import Types

import qualified QLearning.Core as Core
import qualified QLearning.Debug as Debug
import qualified QLearning.V1 as V1
import qualified QLearning.V2 as V2
import qualified QLearning.V3 as V3
import qualified QLearning.V4 as V4

import Graphics.Gloss (Color)


-- -----------------------------------------------------------------------------
-- Agent options
-- -----------------------------------------------------------------------------

-- | One agent exposed as a selectable option in the application.
data AgentOption = AgentOption
    {
        -- | Human-readable name shown in menus and the GUI.
        agentOptionName :: String,

        -- | Controller used to obtain actions from the agent.
        agentOptionController :: Controller,

        -- | Optional reward function used for visualization and diagnostics.
        agentOptionRewardFunction :: Maybe Core.RewardFunction,

        -- | Optional provider of Q-learning-specific debug information.
        agentOptionDebugProvider :: Maybe Debug.AgentDebugProvider
    }


-- | Creates a menu option for a heuristic agent without Q-learning diagnostics.
heuristicAgentOption :: String -> Agent -> AgentOption
heuristicAgentOption name agent =
    AgentOption
        { agentOptionName = name
        , agentOptionController = AI agent
        , agentOptionRewardFunction = Nothing
        , agentOptionDebugProvider = Nothing
        }


-- | Creates a menu option for an agent with reward and debug information.
debuggableAgentOption :: String -> Agent -> Core.RewardFunction -> Debug.AgentDebugProvider -> AgentOption
debuggableAgentOption name agent rewardFunction debugProvider =
    AgentOption
        { agentOptionName = name
        , agentOptionController = AI agent
        , agentOptionRewardFunction = Just rewardFunction
        , agentOptionDebugProvider = Just debugProvider
        }


-- | Converts a selectable agent into the metadata required by the GUI.
makeGuiAgent :: Int -> Color -> AgentOption -> GuiAgent
makeGuiAgent targetId agentColor option =
    GuiAgent
        { guiAgentWormId = targetId
        , guiAgentName = agentOptionName option
        , guiAgentController = agentOptionController option
        , guiAgentColor = agentColor
        , guiAgentRewardFunction = agentOptionRewardFunction option
        , guiAgentDebugProvider = agentOptionDebugProvider option
        }


-- -----------------------------------------------------------------------------
-- Agent loading
-- -----------------------------------------------------------------------------

-- | Loads all heuristic and trained agents available in the application menus.
loadAgentOptions :: IO [AgentOption]
loadAgentOptions = do
    v1Table <- V1.loadQTable "data/models/qlearning_v1.txt"
    v2Table <- V2.loadQTable "data/models/qlearning_v2.txt"
    v3Table <- V3.loadQTable "data/models/qlearning_v3.txt"
    v3Table20k <- V3.loadQTable "data/models/qlearning_v3_20k.txt"
    v3DiverseTable <- V3.loadQTable "data/models/qlearning_v3_diverse_30k.txt"
    v4Table <- V4.loadQTable "data/models/qlearning_v4_diverse_30k.txt"
    v4ReformedTable <- V4.loadQTable "data/models/qlearning_v4_reformed_diverse_30k.txt"

    pure
        [ heuristicAgentOption "Random" randomAgent
        , heuristicAgentOption "Safe Random" safeRandomAgent
        , heuristicAgentOption "Greedy Food" greedyFoodAgent
        , heuristicAgentOption "Safe Greedy Food" safeGreedyFoodAgent
        , heuristicAgentOption "Safe Hunter" safeHunterAgent

        , debuggableAgentOption
            "Q-learning V1"
            (V1.qLearningAgent v1Table)
            V1.rewardForStep
            (V1.v1DebugProvider v1Table)

        , debuggableAgentOption
            "Q-learning V2"
            (V2.qLearningAgent v2Table)
            V2.rewardForStep
            (V2.v2DebugProvider v2Table)

        , debuggableAgentOption
            "Q-learning V3 10k"
            (V3.qLearningAgent v3Table)
            V3.rewardForStep
            (V3.v3DebugProvider v3Table)

        , debuggableAgentOption
            "Q-learning V3 20k"
            (V3.qLearningAgent v3Table20k)
            V3.rewardForStep
            (V3.v3DebugProvider v3Table20k)

        , debuggableAgentOption
            "Q-learning V3 20k + fallback"
            (V3.qLearningAgentWithFallback v3Table20k)
            V3.rewardForStep
            (V3.v3DebugProvider v3Table20k)

        , debuggableAgentOption
            "Q-learning V3 Diverse 30k"
            (V3.qLearningAgent v3DiverseTable)
            V3.rewardForStep
            (V3.v3DebugProvider v3DiverseTable)

        , debuggableAgentOption
            "Q-learning V3 Diverse 30k + fallback"
            (V3.qLearningAgentWithFallback v3DiverseTable)
            V3.rewardForStep
            (V3.v3DebugProvider v3DiverseTable)

        , debuggableAgentOption
            "Q-learning V4 Diverse 30k"
            (V4.qLearningAgent v4Table)
            V4.rewardForStep
            (V4.v4DebugProvider v4Table)

        , debuggableAgentOption
            "Q-learning V4 Diverse 30k + fallback"
            (V4.qLearningAgentWithFallback v4Table)
            V4.rewardForStep
            (V4.v4DebugProvider v4Table)

        , debuggableAgentOption
            "Q-learning V4 Reformed 30k"
            (V4.qLearningAgent v4ReformedTable)
            V4.rewardForStep
            (V4.v4DebugProvider v4ReformedTable)

        , debuggableAgentOption
            "Q-learning V4 Reformed 30k + fallback"
            (V4.qLearningAgentWithFallback v4ReformedTable)
            V4.rewardForStep
            (V4.v4DebugProvider v4ReformedTable)
        ]