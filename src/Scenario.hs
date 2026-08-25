module Scenario where

import Data.List (nub)

import Maps ( isInsideMap, isEmptyTile )
import Types
import Game (initialHeadHistory)


-- -----------------------------------------------------------------------------
-- Scenario definition
-- -----------------------------------------------------------------------------

-- | Complete static configuration of one game scenario.
--
-- Controllers are intentionally not part of a scenario. The same scenario can
-- therefore be used for human play, heuristic agents, Q-learning agents and
-- evaluation.
data Scenario = Scenario
    {
        scenarioName :: String,
        scenarioDescription :: String,
        scenarioMap :: GameMap,
        scenarioWorms :: [Worm]
    }
    deriving (Show, Eq)


-- | Default statistics of a newly spawned worm.
emptyWormStats :: WormStats
emptyWormStats =
    WormStats
        { foodEaten = 0
        , kills = 0
        , age = 0
        }


-- | Creates a living worm with empty statistics.
freshWorm :: Int -> [Position] -> Direction -> Worm
freshWorm targetId body direction =
    Worm
        { wormId = targetId
        , wormBody = body
        , wormDirection = direction
        , wormAlive = True
        , wormStats = emptyWormStats
        }


-- -----------------------------------------------------------------------------
-- Scenario validation
-- -----------------------------------------------------------------------------

-- | Returns validation errors found in a scenario.
--
-- A valid scenario must contain at least one living worm, use unique worm IDs,
-- place every worm body entirely inside empty map tiles and avoid overlapping
-- worm bodies.
validateScenario :: Scenario -> [String]
validateScenario scenario =
    concat
        [ ["Scenario must contain at least one worm." | null worms]
        , ["All worms must start alive." | any (not . wormAlive) worms]
        , ["Every worm must have a non-empty body." | any (null . wormBody) worms]
        , ["Worm IDs must be unique." | length wormIds /= length (nub wormIds)]
        , ["Worm bodies must not overlap." | length bodyPositions /= length (nub bodyPositions)]
        , ["All worm body positions must lie inside the map." | not (null outsidePositions)]
        , ["All worm body positions must start on empty tiles." | not (null blockedPositions)]
        ]
  where
    gamemap = scenarioMap scenario
    worms = scenarioWorms scenario
    wormIds = map wormId worms
    bodyPositions = concatMap wormBody worms

    outsidePositions =
        [ position
        | position <- bodyPositions
        , not (isInsideMap gamemap position)
        ]

    blockedPositions =
        [ position
        | position <- bodyPositions
        , isInsideMap gamemap position
        , not (isEmptyTile gamemap position)
        ]


-- | Builds the initial game state of a scenario.
--
-- Invalid manually defined scenarios fail immediately with their validation
-- errors instead of producing difficult-to-debug game behaviour later.
scenarioInitialState :: Scenario -> GameState
scenarioInitialState scenario =
    case validateScenario scenario of
        [] ->
            GameState
                {
                    gameMap = scenarioMap scenario,
                    gameWorms = scenarioWorms scenario,
                    gameTick = 0,
                    gameHeadHistory = initialHeadHistory (scenarioWorms scenario)
                }

        errors ->
            error
                ( "Invalid scenario \""
                    ++ scenarioName scenario
                    ++ "\":\n"
                    ++ unlines (map ("  - " ++) errors)
                )