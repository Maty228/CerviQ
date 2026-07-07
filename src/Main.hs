module Main where

import Config
import Game
import Maps
import Render
import Controller
import TestData
import Types

import Control.Concurrent (threadDelay)
import Control.Monad (when)


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
main :: IO ()
main = do
    initialState <- maintainFoodCount maxFoodCount testGame
    gameLoop initialState