{-|
Module      : Scenario
Description : Definition, construction, and validation of game scenarios.

A scenario combines a game map with the worms that should initially inhabit it.
Controllers are deliberately not included, allowing the same environment to be
used for human play, AI visualization, training, and evaluation. Before a
scenario becomes a 'GameState', its initial configuration is validated.
-}

module Scenario where

import Data.List (nub)

import Game (initialHeadHistory)
import Maps (isEmptyTile, isInsideMap)
import Types


-- -----------------------------------------------------------------------------
-- Scenario definition
-- -----------------------------------------------------------------------------

-- | Complete static configuration of one game scenario.
data Scenario = Scenario
    {
        -- | Human-readable scenario name.
        scenarioName :: String,

        -- | Short explanation of the environment and its purpose.
        scenarioDescription :: String,

        -- | Map used by the scenario.
        scenarioMap :: GameMap,

        -- | Worms placed on the map at the beginning of the game.
        scenarioWorms :: [Worm]
    }
    deriving (Show, Eq)


-- -----------------------------------------------------------------------------
-- Worm construction
-- -----------------------------------------------------------------------------

-- | Default statistics assigned to a newly created worm.
emptyWormStats :: WormStats
emptyWormStats =
    WormStats
        { foodEaten = 0
        , kills = 0
        , age = 0
        }


-- | Creates a living worm with empty statistics.
--
-- The supplied body is expected to follow the project invariant that its first
-- position represents the worm's head.
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

-- | Returns all validation errors found in a scenario.
--
-- A valid scenario contains at least one living worm, uses unique worm IDs,
-- has non-empty and non-overlapping bodies, and places every body segment on an
-- empty position inside the map.
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
    gameMap' = scenarioMap scenario
    worms = scenarioWorms scenario
    wormIds = map wormId worms
    bodyPositions = concatMap wormBody worms

    outsidePositions =
        [ pos | pos <- bodyPositions, not (isInsideMap gameMap' pos) ]

    blockedPositions =
        [ pos
        | pos <- bodyPositions
        , isInsideMap gameMap' pos
        , not (isEmptyTile gameMap' pos)
        ]


-- -----------------------------------------------------------------------------
-- Initial game state
-- -----------------------------------------------------------------------------

-- | Builds the initial game state represented by a scenario.
--
-- Invalid manually defined scenarios fail immediately instead of allowing an
-- inconsistent state to reach the game engine.
scenarioInitialState :: Scenario -> GameState
scenarioInitialState scenario =
    case validateScenario scenario of
        [] ->
            GameState
                { gameMap = scenarioMap scenario
                , gameWorms = scenarioWorms scenario
                , gameTick = 0
                , gameHeadHistory = initialHeadHistory (scenarioWorms scenario)
                }
        errors ->
            error
                ( "Invalid scenario \"" ++ scenarioName scenario ++ "\":\n"
                    ++ unlines (map ("  - " ++) errors)
                )