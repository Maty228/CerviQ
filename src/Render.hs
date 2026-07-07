module Render where

import Types
import Maps
import Movement
import Collision

-- -----------------------------------------------------------------------------
-- ASCII rendering
-- -----------------------------------------------------------------------------


-- | Renders the complete game state as an ASCII string.
renderGameAscii :: GameState -> String
renderGameAscii state =
    renderStatus state ++ "\n" ++ renderGrid state

-- | Renders only the game grid.
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

-- | Renders the current game status and worm statistics.
renderStatus :: GameState -> String
renderStatus state =
    "Tick: " ++ show (gameTick state)
        ++ " | Worms: "
        ++ unwords (map renderWormStatus (gameWorms state))

-- | Renders a short summary of a single worm.
renderWormStatus :: Worm -> String
renderWormStatus worm =
    "[#" ++ show (wormId worm)
        ++ " len=" ++ show (length (wormBody worm))
        ++ " food=" ++ show (foodEaten (wormStats worm))
        ++ " age=" ++ show (age (wormStats worm))
        ++ " alive=" ++ show (wormAlive worm)
        ++ "]"


-- -----------------------------------------------------------------------------
-- Output
-- -----------------------------------------------------------------------------

-- | Prints the ASCII representation of the game state.
printGameAscii :: GameState -> IO ()
printGameAscii = putStrLn . renderGameAscii

-- | Converts a map tile into its ASCII representation.
renderTile :: Maybe Tile -> Char
renderTile Nothing = ' '
renderTile (Just Empty) = '.'
renderTile (Just Wall) = '#'
renderTile (Just Food) = 'F'
renderTile (Just Poison) = 'P'