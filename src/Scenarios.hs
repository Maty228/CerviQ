{-|
Module      : Scenarios
Description : Built-in deterministic game environments used by CerviQ.

This module contains the manually designed scenarios available to gameplay,
training, evaluation, and multi-worm visualization.

Every scenario keeps its original two default worms so existing training and
evaluation behaviour remains unchanged. Additional predefined starting
positions are available to interactive multi-worm modes. The number of starts
is chosen according to the size and structure of each map rather than forcing
every environment to support the maximum application roster.

Arena supports six worms, Cave eight, Corridors six, and Large Cave the full
nine-worm interface limit. All starts are deterministic and validated together
with the scenario before use.
-}

module Scenarios where

import Maps
import Scenario
import Types


-- -----------------------------------------------------------------------------
-- Arena
-- -----------------------------------------------------------------------------

-- | Original open arena used during development and Q-learning training.
arenaMap :: GameMap
arenaMap =
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


-- | First default worm of the arena scenario.
arenaWormOne :: Worm
arenaWormOne =
    freshWorm 1 [(3, 1), (2, 1), (1, 1)] East


-- | Second default worm of the arena scenario.
arenaWormTwo :: Worm
arenaWormTwo =
    freshWorm 2 [(16, 7), (17, 7), (18, 7)] West


-- | Optional third worm of the arena scenario.
arenaWormThree :: Worm
arenaWormThree =
    freshWorm 3 [(18, 3), (18, 2), (18, 1)] South


-- | Optional fourth worm of the arena scenario.
arenaWormFour :: Worm
arenaWormFour =
    freshWorm 4 [(1, 6), (1, 7), (1, 8)] North


-- | Optional fifth worm of the arena scenario.
arenaWormFive :: Worm
arenaWormFive =
    freshWorm 5 [(8, 8), (7, 8), (6, 8)] East


-- | Optional sixth worm of the arena scenario.
arenaWormSix :: Worm
arenaWormSix =
    freshWorm 6 [(11, 1), (12, 1), (13, 1)] West


-- | Open baseline scenario supporting up to six predefined worm starts.
arenaScenario :: Scenario
arenaScenario =
    Scenario
        { scenarioName = "Arena"
        , scenarioDescription = "Open baseline arena used during Q-learning development."
        , scenarioMap = arenaMap
        , scenarioWorms = [arenaWormOne, arenaWormTwo]
        , scenarioExtraWorms =
            [ arenaWormThree
            , arenaWormFour
            , arenaWormFive
            , arenaWormSix
            ]
        , scenarioBaseFoodCount = 2
        , scenarioMaximumFoodCount = 4
        }


-- -----------------------------------------------------------------------------
-- Cave
-- -----------------------------------------------------------------------------

-- | Larger cave-like map containing chambers and internal obstacles.
caveMap :: GameMap
caveMap =
    fromAsciiMap
        [ "##############################"
        , "#............................#"
        , "#...#####..........#####.....#"
        , "#...#..................#.....#"
        , "#...#....F.....###......#....#"
        , "#...###........###....###....#"
        , "#........##..........##......#"
        , "#........##..........##......#"
        , "#...###.................###..#"
        , "#...#......#####..........#..#"
        , "#...#..F..................#..#"
        , "#...#####..........#####.....#"
        , "#............................#"
        , "#...............F............#"
        , "##############################"
        ]


-- | First default worm of the cave scenario.
caveWormOne :: Worm
caveWormOne =
    freshWorm 1 [(3, 1), (2, 1), (1, 1)] East


-- | Second default worm of the cave scenario.
caveWormTwo :: Worm
caveWormTwo =
    freshWorm 2 [(26, 13), (27, 13), (28, 13)] West


-- | Optional third worm of the cave scenario.
caveWormThree :: Worm
caveWormThree =
    freshWorm 3 [(28, 3), (28, 2), (28, 1)] South


-- | Optional fourth worm of the cave scenario.
caveWormFour :: Worm
caveWormFour =
    freshWorm 4 [(1, 11), (1, 12), (1, 13)] North


-- | Optional fifth worm of the cave scenario.
caveWormFive :: Worm
caveWormFive =
    freshWorm 5 [(14, 1), (13, 1), (12, 1)] East


-- | Optional sixth worm of the cave scenario.
caveWormSix :: Worm
caveWormSix =
    freshWorm 6 [(13, 13), (14, 13), (15, 13)] West


-- | Optional seventh worm of the cave scenario.
caveWormSeven :: Worm
caveWormSeven =
    freshWorm 7 [(1, 7), (1, 6), (1, 5)] South


-- | Optional eighth worm of the cave scenario.
caveWormEight :: Worm
caveWormEight =
    freshWorm 8 [(28, 7), (28, 8), (28, 9)] North


-- | Chamber-based scenario supporting up to eight predefined worm starts.
caveScenario :: Scenario
caveScenario =
    Scenario
        { scenarioName = "Cave"
        , scenarioDescription = "Larger cave with chambers, obstacles and alternative routes."
        , scenarioMap = caveMap
        , scenarioWorms = [caveWormOne, caveWormTwo]
        , scenarioExtraWorms =
            [ caveWormThree
            , caveWormFour
            , caveWormFive
            , caveWormSix
            , caveWormSeven
            , caveWormEight
            ]
        , scenarioBaseFoodCount = 2
        , scenarioMaximumFoodCount = 5
        }


-- -----------------------------------------------------------------------------
-- Corridors
-- -----------------------------------------------------------------------------

-- | Restrictive map containing narrow passages and chokepoints.
corridorMap :: GameMap
corridorMap =
    fromAsciiMap
        [ "##############################"
        , "#............................#"
        , "#.##########....##########...#"
        , "#..........#....#............#"
        , "#..F.......#....#............#"
        , "#..........#....#............#"
        , "#.######.###....###.######...#"
        , "#.............F..............#"
        , "#...######.###..###.######...#"
        , "#............#..#............#"
        , "#............#..#......F.....#"
        , "#............#..#............#"
        , "#...##########..##########...#"
        , "#............................#"
        , "##############################"
        ]


-- | First default worm of the corridor scenario.
corridorWormOne :: Worm
corridorWormOne =
    freshWorm 1 [(3, 1), (2, 1), (1, 1)] East


-- | Second default worm of the corridor scenario.
corridorWormTwo :: Worm
corridorWormTwo =
    freshWorm 2 [(26, 13), (27, 13), (28, 13)] West


-- | Optional third worm of the corridor scenario.
corridorWormThree :: Worm
corridorWormThree =
    freshWorm 3 [(28, 3), (28, 2), (28, 1)] South


-- | Optional fourth worm of the corridor scenario.
corridorWormFour :: Worm
corridorWormFour =
    freshWorm 4 [(1, 11), (1, 12), (1, 13)] North


-- | Optional fifth worm of the corridor scenario.
corridorWormFive :: Worm
corridorWormFive =
    freshWorm 5 [(14, 1), (13, 1), (12, 1)] East


-- | Optional sixth worm of the corridor scenario.
corridorWormSix :: Worm
corridorWormSix =
    freshWorm 6 [(15, 13), (16, 13), (17, 13)] West


-- | Chokepoint-heavy scenario supporting up to six predefined worm starts.
corridorScenario :: Scenario
corridorScenario =
    Scenario
        { scenarioName = "Corridors"
        , scenarioDescription = "Chokepoint-heavy scenario stressing self-trapping and escape decisions."
        , scenarioMap = corridorMap
        , scenarioWorms = [corridorWormOne, corridorWormTwo]
        , scenarioExtraWorms =
            [ corridorWormThree
            , corridorWormFour
            , corridorWormFive
            , corridorWormSix
            ]
        , scenarioBaseFoodCount = 2
        , scenarioMaximumFoodCount = 4
        }


-- -----------------------------------------------------------------------------
-- Large Cave
-- -----------------------------------------------------------------------------

-- | Width of the large viewport-testing map.
largeCaveWidth :: Int
largeCaveWidth = 60


-- | Height of the large viewport-testing map.
largeCaveHeight :: Int
largeCaveHeight = 36


-- | Fixed food positions of the large cave.
largeCaveFoodPositions :: [Position]
largeCaveFoodPositions =
    [(8, 28), (27, 5), (50, 8), (34, 30), (50, 25)]


-- | Returns whether a position belongs to one of the large cave's internal walls.
largeCaveInternalWall :: Position -> Bool
largeCaveInternalWall (x, y) =
    verticalLeft || verticalRight || horizontalUpper || horizontalLower || centralWall
  where
    verticalLeft =
        x == 15 && ((y >= 4 && y <= 14) || (y >= 21 && y <= 31))

    verticalRight =
        x == 43 && ((y >= 4 && y <= 15) || (y >= 22 && y <= 31))

    horizontalUpper =
        y == 10 && x >= 21 && x <= 37 && x /= 29

    horizontalLower =
        y == 26 && x >= 22 && x <= 38 && x /= 31

    centralWall =
        x == 30 && y >= 13 && y <= 23 && y /= 18


-- | Converts one large-cave position into its ASCII map representation.
largeCaveCell :: Position -> Char
largeCaveCell pos@(x, y)
    | x == 0 || y == 0 || x == largeCaveWidth - 1 || y == largeCaveHeight - 1 = '#'
    | largeCaveInternalWall pos = '#'
    | pos `elem` largeCaveFoodPositions = 'F'
    | otherwise = '.'


-- | Large deterministic map used for viewport-based gameplay and visualization.
largeCaveMap :: GameMap
largeCaveMap =
    fromAsciiMap
        [ [largeCaveCell (x, y) | x <- [0 .. largeCaveWidth - 1]]
        | y <- [0 .. largeCaveHeight - 1]
        ]


-- | First default worm of the large cave scenario.
largeCaveWormOne :: Worm
largeCaveWormOne =
    freshWorm 1 [(4, 4), (3, 4), (2, 4)] East


-- | Second default worm of the large cave scenario.
largeCaveWormTwo :: Worm
largeCaveWormTwo =
    freshWorm 2 [(55, 31), (56, 31), (57, 31)] West


-- | Optional third worm of the large cave scenario.
largeCaveWormThree :: Worm
largeCaveWormThree =
    freshWorm 3 [(55, 4), (55, 3), (55, 2)] South


-- | Optional fourth worm of the large cave scenario.
largeCaveWormFour :: Worm
largeCaveWormFour =
    freshWorm 4 [(4, 31), (4, 32), (4, 33)] North


-- | Optional fifth worm of the large cave scenario.
largeCaveWormFive :: Worm
largeCaveWormFive =
    freshWorm 5 [(28, 4), (27, 4), (26, 4)] East


-- | Optional sixth worm of the large cave scenario.
largeCaveWormSix :: Worm
largeCaveWormSix =
    freshWorm 6 [(32, 31), (33, 31), (34, 31)] West


-- | Optional seventh worm of the large cave scenario.
largeCaveWormSeven :: Worm
largeCaveWormSeven =
    freshWorm 7 [(4, 18), (4, 17), (4, 16)] South


-- | Optional eighth worm of the large cave scenario.
largeCaveWormEight :: Worm
largeCaveWormEight =
    freshWorm 8 [(55, 18), (55, 19), (55, 20)] North


-- | Optional ninth worm of the large cave scenario.
--
-- This worm starts beside the opening through the central wall.
largeCaveWormNine :: Worm
largeCaveWormNine =
    freshWorm 9 [(29, 18), (28, 18), (27, 18)] East


-- | Large scenario providing all nine predefined starts supported by the UI.
largeCaveScenario :: Scenario
largeCaveScenario =
    Scenario
        { scenarioName = "Large Cave"
        , scenarioDescription = "Large multi-chamber map intended for viewport-based gameplay."
        , scenarioMap = largeCaveMap
        , scenarioWorms = [largeCaveWormOne, largeCaveWormTwo]
        , scenarioExtraWorms =
            [ largeCaveWormThree
            , largeCaveWormFour
            , largeCaveWormFive
            , largeCaveWormSix
            , largeCaveWormSeven
            , largeCaveWormEight
            , largeCaveWormNine
            ]
        , scenarioBaseFoodCount = 3
        , scenarioMaximumFoodCount = 7
        }


-- -----------------------------------------------------------------------------
-- Scenario collection
-- -----------------------------------------------------------------------------

-- | All built-in scenarios currently available in CerviQ.
allScenarios :: [Scenario]
allScenarios =
    [ arenaScenario
    , caveScenario
    , corridorScenario
    , largeCaveScenario
    ]


-- | Finds a built-in scenario by its exact name.
scenarioByName :: String -> Maybe Scenario
scenarioByName targetName =
    case filter ((== targetName) . scenarioName) allScenarios of
        scenario : _ -> Just scenario
        [] -> Nothing