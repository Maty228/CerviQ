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
import System.Random (randomRIO)
import System.Posix.Internals (statGetType)


-- -----------------------------------------------------------------------------
-- Food spawning
-- -----------------------------------------------------------------------------

-- | Returns a random valid position for spawning food.
--
-- Returns Nothing if no free position exists.
randomFoodPosition :: GameState -> IO (Maybe Position)
randomFoodPosition state = do
    let freePositions = freeFoodPositions state
    case freePositions of
        [] -> return Nothing
        _ -> do 
            index <- randomRIO (0, length freePositions - 1)
            return (Just (freePositions !! index))


-- | Ensures that at least the given number of food items
-- are present on the map.
maintainFoodCount :: Int -> GameState -> IO GameState
maintainFoodCount maxFood state
    | length (foodPositions (gameMap state)) >= maxFood = return state
    | otherwise = do
        maybePosition <- randomFoodPosition state
        case maybePosition of
            Nothing -> return state
            Just position -> maintainFoodCount maxFood (spawnFoodAt position state)


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