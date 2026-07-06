module Render where

import Types
import Maps
import Game


renderGameAscii :: GameState -> String
renderGameAscii state =
    renderStatus state ++ "\n" ++ renderGrid state

renderGrid :: GameState -> String
renderGrid state =
    unlines
        [ [ charAt (x, y) | x <- [0 .. mapWidth (gameMap state) - 1] ]
        | y <- [0 .. mapHeight (gameMap state) - 1]
        ]
  where
    visibleWorms = filter wormAlive (gameWorms state)
    wormHeads = map wormHead visibleWorms
    wormPositions = occupiedPositions visibleWorms

    charAt pos
        | positionOccupied pos wormHeads = '@'
        | positionOccupied pos wormPositions = 'o'
        | otherwise = renderTile (tileAt (gameMap state) pos)

renderStatus :: GameState -> String
renderStatus state =
    "Tick: " ++ show (gameTick state)
        ++ " | Worms: "
        ++ unwords (map renderWormStatus (gameWorms state))

renderWormStatus :: Worm -> String
renderWormStatus worm =
    "[#" ++ show (wormId worm)
        ++ " len=" ++ show (length (wormBody worm))
        ++ " food=" ++ show (foodEaten (wormStats worm))
        ++ " age=" ++ show (age (wormStats worm))
        ++ " alive=" ++ show (wormAlive worm)
        ++ "]"


printGameAscii :: GameState -> IO ()
printGameAscii = putStrLn . renderGameAscii

renderTile :: Maybe Tile -> Char
renderTile Nothing = ' '
renderTile (Just Empty) = '.'
renderTile (Just Wall) = '#'
renderTile (Just Food) = 'F'
renderTile (Just Poison) = 'P'