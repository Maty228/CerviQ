module Types where


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


-- Worm body is never empty for an alive
-- The first position in wormBody is always the head
data Worm = Worm
    {
        wormId :: Int,
        wormBody :: [Position],
        wormDirection :: Direction,
        wormAlive :: Bool
    }
    deriving (Show, Eq)



data GameState = GameState
    {
        gameWorms :: [Worm],
        gameTick :: Int
    }
    deriving (Show, Eq)