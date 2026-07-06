module Main where

import Types
import Game
import Render
import TestData


playerWormId :: Int
playerWormId = 1


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
            gameLoop newState


main :: IO ()
main = gameLoop testGame