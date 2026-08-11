module Main (main, gameLoop) where

import Config
import Game
import Render
import Controller
import TestData
import Types
import Gui
import qualified QLearning.V2 as V2
import qualified QLearning.V3 as V3


import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Graphics.Gloss


-- -----------------------------------------------------------------------------
-- Game loop
-- -----------------------------------------------------------------------------

-- | Runs the interactive terminal version of the game.
--
-- Each iteration:
--   * renders the current game state,
--   * reads player input,
--   * advances the simulation,
--   * replenishes food if necessary.
gameLoop :: GameState -> IO ()
gameLoop state = do
    printGameAscii state

    if all (not . wormAlive) (gameWorms state)
        then putStrLn "Game over!"
        else do
            actions <- collectActions controllers state
            let newState = stepGame actions state
        
            stateWithFood <- maintainFoodCount maxFoodCount newState
            when (not (hasHumanController controllers)) $
                threadDelay tickDelay
            gameLoop stateWithFood




-- -----------------------------------------------------------------------------
-- Entry point
-- -----------------------------------------------------------------------------

-- | Starts the CerviQ debugger with Q-learning V1 against V2.
main :: IO ()
main = do
    v2Table <-
        V2.loadQTable
            "data/models/qlearning_v2.txt"

    v3Table <-
        V3.loadQTable
            "data/models/qlearning_v3_20K.txt"

    runGui
        [
            GuiAgent
                {
                    guiAgentWormId = 1,
                    guiAgentName = "Q-learning V2",
                    guiAgentController =
                        AI (V2.qLearningAgent v2Table),
                    guiAgentColor =
                        makeColorI 50 140 255 255,
                    guiAgentRewardFunction =
                        Just V2.rewardForStep,
                    guiAgentDebugProvider =
                        Just (V2.v2DebugProvider v2Table)
                },

            GuiAgent
                {
                    guiAgentWormId = 2,
                    guiAgentName = "Q-learning V3 20K Fallback",
                    guiAgentController =
                        AI (V3.qLearningAgentWithFallback v3Table),
                    guiAgentColor =
                        makeColorI 180 80 255 255,
                    guiAgentRewardFunction =
                        Just V3.rewardForStep,
                    guiAgentDebugProvider =
                        Just (V3.v3DebugProvider v3Table)
                }
        ]
        testGame
