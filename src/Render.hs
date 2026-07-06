module Render where

import Types
import Maps
import Game


renderGameAscii :: GameState -> String
renderGameAscii state =
    unlines
        [ [ charAt (x, y) | x <- [0 .. mapWidth (gameMap state) - 1] ]
        | y <- [0 .. mapHeight (gameMap state) - 1]
        ]
  where
    visibleWorms :: [Worm]
    visibleWorms = filter wormAlive (gameWorms state)

    wormHeads :: [Position]
    wormHeads = map wormHead visibleWorms

    wormPositions :: [Position]
    wormPositions = occupiedPositions visibleWorms

    charAt :: Position -> Char
    charAt pos
        | positionOccupied pos wormHeads = '@'
        | positionOccupied pos wormPositions = 'o'
        | otherwise = renderTile (tileAt (gameMap state) pos)


printGameAscii :: GameState -> IO ()
printGameAscii = putStrLn . renderGameAscii

renderTile :: Maybe Tile -> Char
renderTile Nothing = ' '
renderTile (Just Empty) = '.'
renderTile (Just Wall) = '#'
renderTile (Just Food) = 'F'
renderTile (Just Poison) = 'P'