{-|
Module      : Types
Description : Core data types shared across the CerviQ project.

This module defines the fundamental vocabulary of the game: map positions,
directions, relative actions, map tiles, worms, complete game state, and the
controller interface used by human and AI-controlled worms. Most other modules
depend on these definitions.
-}
module Types where

import Data.Map (Map)


-- -----------------------------------------------------------------------------
-- Basic types
-- -----------------------------------------------------------------------------

-- | Zero-based map coordinate represented as @(x, y)@.
--
-- The X coordinate increases from left to right and the Y coordinate increases
-- from top to bottom.
type Position = (Int, Int)


-- | Absolute cardinal direction in which a worm can face.
data Direction
    = North
    | East
    | South
    | West
    deriving (Show, Read, Eq, Ord, Enum, Bounded)


-- | Relative movement decision performed by a worm during one game tick.
--
-- Actions are relative to the worm's current 'Direction'. For example,
-- 'TurnLeft' means a different absolute direction depending on which way the
-- worm currently faces.
data Action
    = TurnLeft
    | GoStraight
    | TurnRight
    deriving (Show, Read, Eq, Ord, Enum, Bounded)


-- -----------------------------------------------------------------------------
-- Map
-- -----------------------------------------------------------------------------

-- | Type of one cell in the game map.
data Tile
    = Wall
    | Empty
    | Food
    | Poison
    deriving (Show, Eq)


-- | Complete rectangular game map.
data GameMap = GameMap
    {
        -- | Width of the map in cells.
        mapWidth :: Int,

        -- | Height of the map in cells.
        mapHeight :: Int,

        -- | Tiles indexed by their map positions.
        mapTiles :: Map Position Tile
    }
    deriving (Show, Eq)


-- -----------------------------------------------------------------------------
-- Worm
-- -----------------------------------------------------------------------------

-- | Statistics accumulated by one worm during a game.
data WormStats = WormStats
    {
        -- | Number of food items eaten by the worm.
        foodEaten :: Int,

        -- | Number of opponent deaths credited to the worm.
        kills :: Int,

        -- | Number of game ticks survived by the worm.
        age :: Int
    }
    deriving (Show, Eq)


-- | Complete state of one worm.
--
-- For every living worm, 'wormBody' is non-empty. Its first position is always
-- the head and the remaining positions follow the body towards the tail.
data Worm = Worm
    {
        -- | Unique identifier of the worm within the game.
        wormId :: Int,

        -- | Ordered body positions with the head first.
        wormBody :: [Position],

        -- | Current absolute movement direction.
        wormDirection :: Direction,

        -- | Whether the worm is still alive.
        wormAlive :: Bool,

        -- | Statistics accumulated by the worm.
        wormStats :: WormStats
    }
    deriving (Show, Eq)


-- -----------------------------------------------------------------------------
-- Game state
-- -----------------------------------------------------------------------------

-- | Complete state required to simulate one game.
data GameState = GameState
    {
        -- | Map on which the game is being played.
        gameMap :: GameMap,

        -- | All worms participating in the game.
        gameWorms :: [Worm],

        -- | Number of game ticks that have already been simulated.
        gameTick :: Int,

        -- | Recent head positions indexed by worm ID.
        --
        -- The history can be used by agents and diagnostics to detect repeated
        -- movement patterns such as loops.
        gameHeadHistory :: Map Int [Position]
    }
    deriving (Show, Eq)


-- -----------------------------------------------------------------------------
-- Controllers and agents
-- -----------------------------------------------------------------------------

-- | Function used by an AI-controlled worm to select its next action.
--
-- The agent receives the complete current game state together with the worm it
-- controls. 'IO' allows agents to use effects such as random action selection.
type Agent = GameState -> Worm -> IO Action


-- | Source of decisions for one worm.
data Controller
    = Human
    | AI Agent