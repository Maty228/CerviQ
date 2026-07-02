module Types where

import Data.Map (Map)

type Position = (Int, Int)

data Direction
    = North
    | East
    | South
    | West
    deriving (Show, Eq, Ord, Enum, Bounded)


data Action
    = TurnLeft
    | GoStraight
    | TurnRight
    deriving (Show, Eq, Ord, Enum, Bounded)

data Tile
    = Wall
    | Empty
    | Food
    | Poison
    deriving (Show, Eq)

data Worm = Worm
    {
        wormId :: Int,
        wormBody :: [Position],
        wormDirection :: Direction,
        wormAlive :: Bool
    }
    deriving (Show, Eq)


type Grid = Map Position Tile

data GameState = GameState
    {
        gameGrid :: Grid,
        gameWorms :: [Worm],
        gameTick :: Int
    }
    deriving (Show, Eq)