module Movement where

import Types


-- -----------------------------------------------------------------------------
-- Direction helpers
-- -----------------------------------------------------------------------------

-- | Rotates the given direction 90 degrees to the left.
turnLeft :: Direction -> Direction
turnLeft North = West
turnLeft East = North
turnLeft South = East
turnLeft West = South


-- | Rotates the given direction 90 degrees to the right.
turnRight :: Direction -> Direction
turnRight North = East
turnRight East = South
turnRight South = West
turnRight West = North


-- | Moves one tile forward in the given direction.
moveForward :: Position -> Direction -> Position
moveForward (x,y) North = (x, y-1)
moveForward (x,y) East = (x+1, y)
moveForward (x,y) South = (x, y+1)
moveForward (x,y) West = (x-1, y)


-- | Converts a desired direction into a relative action.
--
-- Returns Nothing when the desired direction would require
-- an illegal 180-degree turn.
directionToAction :: Direction -> Direction -> Maybe Action
directionToAction current desired
    | desired == current = Just GoStraight
    | desired == turnLeft current = Just TurnLeft
    | desired == turnRight current = Just TurnRight
    | otherwise = Nothing


-- -----------------------------------------------------------------------------
-- Worm movement
-- -----------------------------------------------------------------------------

-- | Returns the head position of a living worm.
--
-- A living worm is guaranteed to have a non-empty body.
wormHead :: Worm -> Position
wormHead worm = 
    case wormBody worm of
        headPosition : _ -> headPosition
        [] -> error "wormHead: wormBody is empty"


-- | Applies the given action to the current direction.
applyAction :: Direction -> Action -> Direction
applyAction dir TurnLeft = turnLeft dir
applyAction dir TurnRight = turnRight dir
applyAction dir GoStraight = dir


-- | Computes the next head position based on the worm's current direction.
nextHeadPosition :: Worm -> Position
nextHeadPosition worm = moveForward (wormHead worm) (wormDirection worm)

-- | Removes the last body segment while preserving the order
-- of the remaining segments.
removeTail :: [Position] -> [Position]
removeTail [_] = []
removeTail (segment : rest) = segment : removeTail rest
removeTail [] = []


-- | Builds the new worm body after moving to a new head position.
--
-- If the worm grows, the tail is preserved.
-- Otherwise, the last body segment is removed.
advanceBody :: Bool -> Position -> [Position] -> [Position]
advanceBody grows newHead oldBody =
    case oldBody of
        [] -> [newHead]
        _
            | grows -> newHead : oldBody
            | otherwise -> newHead : removeTail oldBody


-- | Moves the worm one tile forward.
--
-- The worm grows if the first argument is True.
moveWorm :: Bool -> Worm -> Worm
moveWorm grows worm =
    worm {wormBody = advanceBody grows (nextHeadPosition worm) (wormBody worm)}


-- | Applies an action and then moves the worm.
moveWormAfterAction :: Bool -> Action -> Worm -> Worm
moveWormAfterAction grows action worm =
    let newDirection = applyAction (wormDirection worm) action
        turnedWorm = worm {wormDirection = newDirection}
    in moveWorm grows turnedWorm