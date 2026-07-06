module TestData where

import Types
import Maps


-- | Simple hardcoded map used for development and testing.
testMap :: GameMap
testMap = fromAsciiMap
    [ "##########"
    , "#........#"
    , "#....F...#"
    , "#........#"
    , "##########"]


testWorm :: Worm
testWorm = Worm {
    wormId = 1,
    wormBody = [(3,1), (2,1), (1,1)],
    wormDirection = East,
    wormAlive = True
}

testGame :: GameState
testGame = 
    GameState
        {
            gameMap = testMap,
            gameWorms = [testWorm],
            gameTick = 0
        }