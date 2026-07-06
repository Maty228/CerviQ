module Main where

import Types
import Game
import Render
import TestData
import Maps
import Config

import System.Random (randomRIO)






randomFoodPosition :: GameState -> IO (Maybe Position)
randomFoodPosition state = do
    let freePositions = freeFoodPositions state
    case freePositions of
        [] -> return Nothing
        _ -> do 
            index <- randomRIO (0, length freePositions - 1)
            return (Just (freePositions !! index))


ensureFoodCount :: Int -> GameState -> IO GameState
ensureFoodCount maxFood state
    | length (foodPositions (gameMap state)) >= maxFood = return state
    | otherwise = do
        maybePosition <- randomFoodPosition state
        case maybePosition of
            Nothing -> return state
            Just position -> ensureFoodCount maxFood (spawnFoodAt position state)



gameLoop :: GameState -> IO ()
gameLoop state = do
    printGameAscii state

    case filter wormAlive (gameWorms state) of
        [] ->
            putStrLn "Game over!"

        playerWorm : _ -> do
            putStrLn "Action: w = north, s = south, a = west, d = east, Enter = straight"
            input <- getLine
            let action = playerActionFromInput playerWorm input
                newState = stepGame [(playerWormId, action)] state
            stateWithFood <- ensureFoodCount maxFoodCount newState
            gameLoop stateWithFood


main :: IO ()
main = do
    initialState <- ensureFoodCount maxFoodCount testGame
    gameLoop initialState