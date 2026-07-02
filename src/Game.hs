module Game where

import Types
import Maps

turnLeft :: Direction -> Direction
turnLeft North = West
turnLeft East = North
turnLeft South = East
turnLeft West = South


turnRight :: Direction -> Direction
turnRight North = East
turnRight East = South
turnRight South = West
turnRight West = North


moveForward :: Position -> Direction -> Position
moveForward (x,y) North = (x, y-1)
moveForward (x,y) East = (x+1, y)
moveForward (x,y) South = (x, y+1)
moveForward (x,y) West = (x-1, y)



{-
Using Image coordination system:
0,0 1,0 2,0 3,0 4,0
0,1 1,1 2,1 3,1 4,1
0,2 1,2 2,2 3,2 4,2
0,3 1,3 2,3 3,3 4,3
0,4 1,4 2,4 3,4 4,4
-}

-- Returns the head position of a worm
-- An alive worm always has a non-empty body and the first position is its head
wormHead :: Worm -> Position
wormHead worm = 
    case wormBody worm of
        headPosition : _ -> headPosition
        [] -> error "wormHead: wormBody is empty"


applyAction :: Direction -> Action -> Direction
applyAction dir TurnLeft = turnLeft dir
applyAction dir TurnRight = turnRight dir
applyAction dir GoStraight = dir


nextHeadPosition :: Worm -> Position
nextHeadPosition worm = moveForward (wormHead worm) (wormDirection worm)



-- Build a new worm body after moving to a new head position
-- If the worm grows, the tail is kept, otherwise the last body segment is removed
advanceBody :: Bool -> Position -> [Position] -> [Position]
advanceBody grows newHead oldBody
    | grows = newHead : oldBody
    | otherwise = newHead : init oldBody

moveWorm :: Bool -> Worm -> Worm
moveWorm grows worm =
    worm {wormBody = advanceBody grows (nextHeadPosition worm) (wormBody worm)}


moveWormAfterAction :: Bool -> Action -> Worm -> Worm
moveWormAfterAction grows action worm =
    let newDirection = applyAction (wormDirection worm) action
        turnedWorm = worm {wormDirection = newDirection}
    in moveWorm grows turnedWorm



isBlocked :: GameMap -> Position -> Bool
isBlocked map pos =
    case tileAt map pos of
        Nothing -> True
        Just tile -> tile == Wall 


wouldHitWall :: GameMap -> Worm -> Bool
wouldHitWall map worm =
    isBlocked map (nextHeadPosition worm)