module Types where

import Data.Map (Map)


-- -----------------------------------------------------------------------------
-- Basic types
-- -----------------------------------------------------------------------------

type Position = (Int, Int)


data Direction
    = North
    | East
    | South
    | West
    deriving (Show, Read, Eq, Ord, Enum, Bounded)


data Action
    = TurnLeft
    | GoStraight
    | TurnRight
    deriving (Show, Read, Eq, Ord, Enum, Bounded)


-- -----------------------------------------------------------------------------
-- Map
-- -----------------------------------------------------------------------------

data Tile
    = Wall
    | Empty
    | Food
    | Poison
    deriving (Show, Eq)


data GameMap = GameMap
    {
        mapWidth :: Int,
        mapHeight :: Int,
        mapTiles :: Map Position Tile
    }
    deriving (Show, Eq)


-- -----------------------------------------------------------------------------
-- Worm
-- -----------------------------------------------------------------------------

data WormStats = WormStats
    {
        foodEaten :: Int,
        kills :: Int,
        age :: Int
    }
    deriving (Show, Eq)


-- Worm body is never empty for an alive
-- The first position in wormBody is always the head
data Worm = Worm
    {
        wormId :: Int,
        wormBody :: [Position],
        wormDirection :: Direction,
        wormAlive :: Bool,
        wormStats :: WormStats
    }
    deriving (Show, Eq)


-- -----------------------------------------------------------------------------
-- Agent
-- -----------------------------------------------------------------------------

type Agent = GameState -> Worm -> IO Action

data Controller
    = Human
    | AI Agent

-- -----------------------------------------------------------------------------
-- Game state
-- -----------------------------------------------------------------------------

data GameState = GameState
    {
        gameMap :: GameMap,
        gameWorms :: [Worm],
        gameTick :: Int
    }
    deriving (Show, Eq)