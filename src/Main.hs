module Main (main, gameLoop) where

import Config
import Game
import Render
import Controller
import TestData
import Types
import Gui
import Agent
import qualified QLearning.V1 as V1

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

-- | Starts the CerviQ Q-learning debugger.
main :: IO ()
main = do
    qTable <- V1.loadQTable "data/models/qlearning_v1.txt"

    runGui
        [
            GuiAgent
                {
                    guiAgentWormId = 1,
                    guiAgentName = "Q-learning V1",
                    guiAgentController = AI (V1.qLearningAgent qTable),
                    guiAgentColor = makeColorI 50 140 255 255,
                    guiAgentRewardFunction = Just V1.rewardForStep,
                    guiAgentDebugProvider = Just (V1.v1DebugProvider qTable)
                },

            GuiAgent
                {
                    guiAgentWormId = 2,
                    guiAgentName = "SafeGreedyFood",
                    guiAgentController = AI safeGreedyFoodAgent,
                    guiAgentColor = makeColorI 255 145 40 255,
                    guiAgentRewardFunction = Nothing,
                    guiAgentDebugProvider = Nothing
                }
        ]
        testGame
