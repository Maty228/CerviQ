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

-- | One agent that can be selected from the application menu.
data AgentOption = AgentOption
    {
        agentOptionName :: String,
        agentOptionController :: Controller,
        agentOptionRewardFunction :: Maybe Core.RewardFunction,
        agentOptionDebugProvider :: Maybe Debug.AgentDebugProvider
    }


-- | Converts a selectable agent into the metadata required by the GUI.
makeGuiAgent :: Int -> Color -> AgentOption -> GuiAgent
makeGuiAgent targetId agentColor option =
    GuiAgent
        {
            guiAgentWormId = targetId,
            guiAgentName = agentOptionName option,
            guiAgentController = agentOptionController option,
            guiAgentColor = agentColor,
            guiAgentRewardFunction = agentOptionRewardFunction option,
            guiAgentDebugProvider = agentOptionDebugProvider option
        }


-- -----------------------------------------------------------------------------
-- Agent loading
-- -----------------------------------------------------------------------------

-- | Loads all agents available in the GUI menu.
loadAgentOptions :: IO [AgentOption]
loadAgentOptions = do
    v1Table <-
        V1.loadQTable "data/models/qlearning_v1.txt"

    v2Table <-
        V2.loadQTable "data/models/qlearning_v2.txt"

    v3Table <-
        V3.loadQTable "data/models/qlearning_v3.txt"

    v3Table20k <-
        V3.loadQTable "data/models/qlearning_v3_20k.txt"

    v3DiverseTable <-
        V3.loadQTable "data/models/qlearning_v3_diverse_30k.txt"
    
    v4Table <-
        V4.loadQTable "data/models/qlearning_v4_diverse_30k.txt"

    v4ReformedTable <-
        V4.loadQTable "data/models/qlearning_v4_reformed_diverse_30k.txt"

    pure
        [
            AgentOption
                {
                    agentOptionName = "Random",
                    agentOptionController = AI randomAgent,
                    agentOptionRewardFunction = Nothing,
                    agentOptionDebugProvider = Nothing
                },

            AgentOption
                {
                    agentOptionName = "Safe Random",
                    agentOptionController = AI safeRandomAgent,
                    agentOptionRewardFunction = Nothing,
                    agentOptionDebugProvider = Nothing
                },

            AgentOption
                {
                    agentOptionName = "Greedy Food",
                    agentOptionController = AI greedyFoodAgent,
                    agentOptionRewardFunction = Nothing,
                    agentOptionDebugProvider = Nothing
                },

            AgentOption
                {
                    agentOptionName = "Safe Greedy Food",
                    agentOptionController = AI safeGreedyFoodAgent,
                    agentOptionRewardFunction = Nothing,
                    agentOptionDebugProvider = Nothing
                },

            AgentOption
                {
                    agentOptionName = "Safe Hunter",
                    agentOptionController = AI safeHunterAgent,
                    agentOptionRewardFunction = Nothing,
                    agentOptionDebugProvider = Nothing
                },

            AgentOption
                {
                    agentOptionName = "Q-learning V1",
                    agentOptionController = AI (V1.qLearningAgent v1Table),
                    agentOptionRewardFunction = Just V1.rewardForStep,
                    agentOptionDebugProvider = Just (V1.v1DebugProvider v1Table)
                },

            AgentOption
                {
                    agentOptionName = "Q-learning V2",
                    agentOptionController = AI (V2.qLearningAgent v2Table),
                    agentOptionRewardFunction = Just V2.rewardForStep,
                    agentOptionDebugProvider = Just (V2.v2DebugProvider v2Table)
                },

            AgentOption
                {
                    agentOptionName = "Q-learning V3 10k",
                    agentOptionController = AI (V3.qLearningAgent v3Table),
                    agentOptionRewardFunction = Just V3.rewardForStep,
                    agentOptionDebugProvider = Just (V3.v3DebugProvider v3Table)
                },

            AgentOption
                {
                    agentOptionName = "Q-learning V3 20k",
                    agentOptionController = AI (V3.qLearningAgent v3Table20k),
                    agentOptionRewardFunction = Just V3.rewardForStep,
                    agentOptionDebugProvider = Just (V3.v3DebugProvider v3Table20k)
                },

            AgentOption
                {
                    agentOptionName = "Q-learning V3 20k + fallback",
                    agentOptionController = AI (V3.qLearningAgentWithFallback v3Table20k),
                    agentOptionRewardFunction = Just V3.rewardForStep,
                    agentOptionDebugProvider = Just (V3.v3DebugProvider v3Table20k)
                },
            AgentOption
                {
                    agentOptionName = "Q-learning V3 Diverse 30k",
                    agentOptionController = AI (V3.qLearningAgent v3DiverseTable),
                    agentOptionRewardFunction = Just V3.rewardForStep,
                    agentOptionDebugProvider = Just (V3.v3DebugProvider v3DiverseTable)
                },

            AgentOption
                {
                    agentOptionName = "Q-learning V3 Diverse 30k + fallback",
                    agentOptionController = AI (V3.qLearningAgentWithFallback v3DiverseTable),
                    agentOptionRewardFunction = Just V3.rewardForStep,
                    agentOptionDebugProvider = Just (V3.v3DebugProvider v3DiverseTable)
                },
            AgentOption
                {
                    agentOptionName = "Q-learning V4 Diverse 30k",
                    agentOptionController = AI (V4.qLearningAgent v4Table),
                    agentOptionRewardFunction = Just V4.rewardForStep,
                    agentOptionDebugProvider = Just (V4.v4DebugProvider v4Table)
                },

            AgentOption
                {
                    agentOptionName = "Q-learning V4 Diverse 30k + fallback",
                    agentOptionController = AI (V4.qLearningAgentWithFallback v4Table),
                    agentOptionRewardFunction = Just V4.rewardForStep,
                    agentOptionDebugProvider = Just (V4.v4DebugProvider v4Table)
                },
            AgentOption
                {
                    agentOptionName = "Q-learning V4 Reformed 30k",
                    agentOptionController = AI (V4.qLearningAgent v4ReformedTable),
                    agentOptionRewardFunction = Just V4.rewardForStep,
                    agentOptionDebugProvider = Just (V4.v4DebugProvider v4ReformedTable)
                },

            AgentOption
                {
                    agentOptionName = "Q-learning V4 Reformed 30k + fallback",
                    agentOptionController = AI (V4.qLearningAgentWithFallback v4ReformedTable),
                    agentOptionRewardFunction = Just V4.rewardForStep,
                    agentOptionDebugProvider = Just (V4.v4DebugProvider v4ReformedTable)
                }
        ]