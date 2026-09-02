{-|
Module      : Scenario
Description : Definition, construction, and validation of game scenarios.

A scenario combines a game map with default and optional worm starting
positions together with scenario-specific interactive food settings.
Controllers are deliberately not included, allowing the same environment to be
reused for human play, AI visualization, training, and evaluation.

The default worms preserve the original scenario setup used by training and
evaluation. Additional starts and adaptive food targets are used by interactive
multi-worm sessions without changing those default experiment configurations.

Before a scenario becomes a 'GameState', its selected worm configuration and
static scenario settings are validated.
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

        -- | Default worms placed on the map.
        --
        -- Training, evaluation, and compatibility constructors use this list,
        -- preserving the original two-worm experiment environments.
        scenarioWorms :: [Worm],

        -- | Additional predefined worm starts available to interactive modes.
        --
        -- These worms are not included by 'scenarioInitialState'. Watch Agents
        -- and Play vs Agents request them through
        -- 'scenarioInitialStateForWormCount'.
        scenarioExtraWorms :: [Worm],

        -- | Minimum food target used by interactive games on this scenario.
        --
        -- Training and evaluation may deliberately use their own fixed food
        -- configuration instead.
        scenarioBaseFoodCount :: Int,

        -- | Upper bound on the interactive food target as more worms are added.
        scenarioMaximumFoodCount :: Int
    }
    deriving (Show, Eq)


-- | Returns all predefined worm starts available in a scenario.
scenarioAvailableWorms :: Scenario -> [Worm]
scenarioAvailableWorms scenario =
    scenarioWorms scenario ++ scenarioExtraWorms scenario


-- | Returns the maximum number of predefined worms available in a scenario.
scenarioMaximumWormCount :: Scenario -> Int
scenarioMaximumWormCount =
    length . scenarioAvailableWorms

-- | Computes the maintained food target for an interactive game.
--
-- The scenario starts from its base food count and gains approximately one
-- additional food item for every two extra configured worms, capped by the
-- scenario-specific maximum.
scenarioFoodCountForWorms :: Int -> Scenario -> Int
scenarioFoodCountForWorms wormCount scenario =
    min
        (scenarioMaximumFoodCount scenario)
        (scenarioBaseFoodCount scenario + additionalFood)
  where
    additionalFood =
        max 0 (wormCount - 1) `div` 2


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

-- | Returns validation errors for one map and selected worm configuration.
--
-- A valid configuration contains at least one living worm, uses unique worm
-- IDs, has non-empty and non-overlapping bodies, and places every body segment
-- on an empty position inside the map.
validateWormConfiguration :: GameMap -> [Worm] -> [String]
validateWormConfiguration gameMap' worms =
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
    wormIds = map wormId worms
    bodyPositions = concatMap wormBody worms

    outsidePositions =
        [ pos
        | pos <- bodyPositions
        , not (isInsideMap gameMap' pos)
        ]

    blockedPositions =
        [ pos
        | pos <- bodyPositions
        , isInsideMap gameMap' pos
        , not (isEmptyTile gameMap' pos)
        ]

-- | Returns all validation errors found in a complete scenario definition.
--
-- Default and optional worm starts are validated together. Interactive food
-- configuration must also use a non-negative base and a maximum no smaller
-- than that base.
validateScenario :: Scenario -> [String]
validateScenario scenario =
    foodConfigurationErrors
        ++ validateWormConfiguration
            (scenarioMap scenario)
            (scenarioAvailableWorms scenario)
  where
    baseFood =
        scenarioBaseFoodCount scenario

    maximumFood =
        scenarioMaximumFoodCount scenario

    foodConfigurationErrors =
        [ "Scenario base food count must not be negative."
        | baseFood < 0
        ]
            ++ [ "Scenario maximum food count must be at least its base food count."
               | maximumFood < baseFood
               ]


-- -----------------------------------------------------------------------------
-- Initial game states
-- -----------------------------------------------------------------------------

-- | Builds a game state from a selected subset of one scenario's predefined worms.
--
-- Invalid predefined configurations fail immediately instead of allowing an
-- inconsistent state to reach the game engine.
initialStateWithWorms :: Scenario -> [Worm] -> GameState
initialStateWithWorms scenario worms =
    case validateWormConfiguration (scenarioMap scenario) worms of
        [] ->
            GameState
                { gameMap = scenarioMap scenario
                , gameWorms = worms
                , gameTick = 0
                , gameHeadHistory = initialHeadHistory worms
                }

        errors ->
            error
                ( "Invalid scenario \""
                    ++ scenarioName scenario
                    ++ "\":\n"
                    ++ unlines (map ("  - " ++) errors)
                )


-- | Builds the default initial game state represented by a scenario.
--
-- Only 'scenarioWorms' are used here. This deliberately preserves the original
-- two-worm behaviour used by training, evaluation, and compatibility callers.
scenarioInitialState :: Scenario -> GameState
scenarioInitialState scenario =
    case validateScenario scenario of
        [] -> initialStateWithWorms scenario (scenarioWorms scenario)

        errors ->
            error
                ( "Invalid scenario \""
                    ++ scenarioName scenario
                    ++ "\":\n"
                    ++ unlines (map ("  - " ++) errors)
                )


-- | Builds an initial state containing the requested number of predefined worms.
--
-- Worms are taken in scenario order: the default worms first, followed by the
-- additional multi-worm starts. 'Nothing' is returned when the requested count
-- is non-positive or exceeds the number of available predefined starts.
scenarioInitialStateForWormCount :: Int -> Scenario -> Maybe GameState
scenarioInitialStateForWormCount wormCount scenario
    | wormCount <= 0 = Nothing
    | wormCount > scenarioMaximumWormCount scenario = Nothing
    | not (null validationErrors) =
        error
            ( "Invalid scenario \""
                ++ scenarioName scenario
                ++ "\":\n"
                ++ unlines (map ("  - " ++) validationErrors)
            )
    | otherwise =
        Just $
            initialStateWithWorms
                scenario
                (take wormCount (scenarioAvailableWorms scenario))
  where
    validationErrors =
        validateScenario scenario
