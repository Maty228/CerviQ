module TestData where

import Types
import Maps


initialWormStats :: WormStats
initialWormStats =
    WormStats
        { foodEaten = 0
        , kills = 0
        , age = 0
        }

-- | Simple hardcoded map used for development and testing.
testMap :: GameMap
testMap =
    fromAsciiMap
        [ "####################"
        , "#..................#"
        , "#....F.............#"
        , "#..................#"
        , "#..................#"
        , "#.........F........#"
        , "#..................#"
        , "#..................#"
        , "#.............F....#"
        , "####################"
        ]


testWorm :: Worm
testWorm =
    Worm
        { wormId = 1
        , wormBody = [(3, 1), (2, 1), (1, 1)]
        , wormDirection = East
        , wormAlive = True
        , wormStats = initialWormStats
        }


testAiWorm :: Worm
testAiWorm = 
    Worm
        { wormId = 2
        , wormBody = [(16, 7), (17, 7), (18, 7)]
        , wormDirection = West
        , wormAlive = True
        , wormStats = initialWormStats
        }


testGame :: GameState
testGame = 
    GameState
        {
            gameMap = testMap,
            gameWorms = [testWorm, testAiWorm],
            gameTick = 0
        }