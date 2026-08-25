module Scenarios where

import Maps
import Scenario
import Types


-- -----------------------------------------------------------------------------
-- Arena scenario
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


-- | First worm of the original arena scenario.
arenaWormOne :: Worm
arenaWormOne =
    freshWorm
        1
        [(3, 1), (2, 1), (1, 1)]
        East


-- | Second worm of the original arena scenario.
arenaWormTwo :: Worm
arenaWormTwo =
    freshWorm
        2
        [(16, 7), (17, 7), (18, 7)]
        West


-- | Original open scenario used for training and baseline evaluation.
arenaScenario :: Scenario
arenaScenario =
    Scenario
        { scenarioName = "Arena"
        , scenarioDescription = "Open baseline arena used during Q-learning development."
        , scenarioMap = arenaMap
        , scenarioWorms = [arenaWormOne, arenaWormTwo]
        }


-- -----------------------------------------------------------------------------
-- Cave scenario
-- -----------------------------------------------------------------------------

-- | Larger cave-like map containing chambers and obstacles.
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
    freshWorm
        1
        [(3, 1), (2, 1), (1, 1)]
        East


-- | Second worm of the cave scenario.
caveWormTwo :: Worm
caveWormTwo =
    freshWorm
        2
        [(26, 13), (27, 13), (28, 13)]
        West


-- | Scenario testing general navigation through open areas and obstacles.
caveScenario :: Scenario
caveScenario =
    Scenario
        { scenarioName = "Cave"
        , scenarioDescription = "Larger cave with chambers, obstacles and alternative routes."
        , scenarioMap = caveMap
        , scenarioWorms = [caveWormOne, caveWormTwo]
        }


-- -----------------------------------------------------------------------------
-- Corridor scenario
-- -----------------------------------------------------------------------------

-- | Restrictive map containing narrow passages and chokepoints
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
    freshWorm
        1
        [(3, 1), (2, 1), (1, 1)]
        East


-- | Second worm of the corridor scenario.
corridorWormTwo :: Worm
corridorWormTwo =
    freshWorm
        2
        [(26, 13), (27, 13), (28, 13)]
        West


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
-- Scenario collection
-- -----------------------------------------------------------------------------

-- | All manually defined scenarios currently available in CerviQ.
allScenarios :: [Scenario]
allScenarios =
    [ arenaScenario
    , caveScenario
    , corridorScenario
    ]


-- | Finds a manually defined scenario by its exact name.
scenarioByName :: String -> Maybe Scenario
scenarioByName targetName =
    case filter ((== targetName) . scenarioName) allScenarios of
        scenario : _ -> Just scenario
        [] -> Nothing