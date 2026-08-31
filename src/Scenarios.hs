{-|
Module      : Scenarios
Description : Built-in deterministic game environments used by CerviQ.

This module contains the manually designed scenarios available to gameplay,
training, and evaluation. Arena, Cave, and Corridors use direct ASCII layouts.
Large Cave is constructed deterministically from geometric rules to provide a
larger environment for testing the viewport and long-range navigation.
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


-- | First worm of the arena scenario.
arenaWormOne :: Worm
arenaWormOne =
    freshWorm 1 [(3, 1), (2, 1), (1, 1)] East


-- | Second worm of the arena scenario.
arenaWormTwo :: Worm
arenaWormTwo =
    freshWorm 2 [(16, 7), (17, 7), (18, 7)] West


-- | Open baseline scenario used throughout development and evaluation.
arenaScenario :: Scenario
arenaScenario =
    Scenario
        { scenarioName = "Arena"
        , scenarioDescription = "Open baseline arena used during Q-learning development."
        , scenarioMap = arenaMap
        , scenarioWorms = [arenaWormOne, arenaWormTwo]
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


-- | First worm of the cave scenario.
caveWormOne :: Worm
caveWormOne =
    freshWorm 1 [(3, 1), (2, 1), (1, 1)] East


-- | Second worm of the cave scenario.
caveWormTwo :: Worm
caveWormTwo =
    freshWorm 2 [(26, 13), (27, 13), (28, 13)] West


-- | Scenario testing navigation through chambers, obstacles, and alternative routes.
caveScenario :: Scenario
caveScenario =
    Scenario
        { scenarioName = "Cave"
        , scenarioDescription = "Larger cave with chambers, obstacles and alternative routes."
        , scenarioMap = caveMap
        , scenarioWorms = [caveWormOne, caveWormTwo]
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


-- | First worm of the corridor scenario.
corridorWormOne :: Worm
corridorWormOne =
    freshWorm 1 [(3, 1), (2, 1), (1, 1)] East


-- | Second worm of the corridor scenario.
corridorWormTwo :: Worm
corridorWormTwo =
    freshWorm 2 [(26, 13), (27, 13), (28, 13)] West


-- | Scenario stressing narrow-space navigation and escape decisions.
corridorScenario :: Scenario
corridorScenario =
    Scenario
        { scenarioName = "Corridors"
        , scenarioDescription = "Chokepoint-heavy scenario stressing self-trapping and escape decisions."
        , scenarioMap = corridorMap
        , scenarioWorms = [corridorWormOne, corridorWormTwo]
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


-- | First worm of the large cave scenario.
largeCaveWormOne :: Worm
largeCaveWormOne =
    freshWorm 1 [(4, 4), (3, 4), (2, 4)] East


-- | Second worm of the large cave scenario.
largeCaveWormTwo :: Worm
largeCaveWormTwo =
    freshWorm 2 [(55, 31), (56, 31), (57, 31)] West


-- | Large multi-chamber scenario designed to exercise the camera system.
largeCaveScenario :: Scenario
largeCaveScenario =
    Scenario
        { scenarioName = "Large Cave"
        , scenarioDescription = "Large multi-chamber map intended for viewport-based gameplay."
        , scenarioMap = largeCaveMap
        , scenarioWorms = [largeCaveWormOne, largeCaveWormTwo]
        }


-- -----------------------------------------------------------------------------
-- Scenario collection
-- -----------------------------------------------------------------------------

-- | All built-in scenarios currently available in CerviQ.
allScenarios :: [Scenario]
allScenarios =
    [arenaScenario, caveScenario, corridorScenario, largeCaveScenario]


-- | Finds a built-in scenario by its exact name.
scenarioByName :: String -> Maybe Scenario
scenarioByName targetName =
    case filter ((== targetName) . scenarioName) allScenarios of
        scenario : _ -> Just scenario
        [] -> Nothing