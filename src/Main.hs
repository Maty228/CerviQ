module Main where

import Config
import Game
import Render
import Controller
import TestData
import Types
import Gui
import Agent
import QLearning

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

-- | Entry point of the application.
-- main :: IO ()
-- main = do
--     initialState <- maintainFoodCount maxFoodCount testGame
--     gameLoop initialState

main :: IO ()
main = do
    qTable <- loadQTable "data/models/qlearning_v1.txt"

    runGui
        [
            GuiAgent
                {
                    guiAgentWormId = 1,
                    guiAgentName = "Q-learning v1",
                    guiAgentController =
                        AI (qLearningAgent qTable),
                    guiAgentColor =
                        makeColorI 50 140 255 255,
                    guiAgentQTable =
                        Just qTable
                },

            GuiAgent
                {
                    guiAgentWormId = 2,
                    guiAgentName = "SafeGreedyFood",
                    guiAgentController =
                        AI safeGreedyFoodAgent,
                    guiAgentColor =
                        makeColorI 255 145 40 255,
                    guiAgentQTable =
                        Nothing
                }
        ]
        testGame
