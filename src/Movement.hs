{-|
Module      : Movement
Description : Pure helpers for worm direction, position, and body movement.

This module implements local worm movement only. It converts relative actions
into absolute directions, computes new head positions, and updates worm bodies
including growth. Collision detection, deaths, food handling, and simultaneous
game-tick resolution are handled by higher-level modules.
-}
module Movement where

import Types


-- -----------------------------------------------------------------------------
-- Direction and action helpers
-- -----------------------------------------------------------------------------

-- | Rotates an absolute direction 90 degrees to the left.
turnLeft :: Direction -> Direction
turnLeft North = West
turnLeft East = North
turnLeft South = East
turnLeft West = South


-- | Rotates an absolute direction 90 degrees to the right.
turnRight :: Direction -> Direction
turnRight North = East
turnRight East = South
turnRight South = West
turnRight West = North


-- | Applies a relative action to an absolute direction.
applyAction :: Direction -> Action -> Direction
applyAction direction TurnLeft = turnLeft direction
applyAction direction GoStraight = direction
applyAction direction TurnRight = turnRight direction


-- | Converts a requested absolute direction into a relative worm action.
--
-- Returns 'Nothing' when reaching the requested direction would require an
-- illegal 180-degree turn.
directionToAction :: Direction -> Direction -> Maybe Action
directionToAction currentDirection desiredDirection
    | desiredDirection == currentDirection =
        Just GoStraight

    | desiredDirection == turnLeft currentDirection =
        Just TurnLeft

    | desiredDirection == turnRight currentDirection =
        Just TurnRight

    | otherwise =
        Nothing


-- -----------------------------------------------------------------------------
-- Position helpers
-- -----------------------------------------------------------------------------

-- | Moves a position by one cell in the given absolute direction.
moveForward :: Position -> Direction -> Position
moveForward (x, y) North = (x, y - 1)
moveForward (x, y) East = (x + 1, y)
moveForward (x, y) South = (x, y + 1)
moveForward (x, y) West = (x - 1, y)


-- | Returns the first body position of a worm, which represents its head.
--
-- Game logic maintains the invariant that every living worm has a non-empty
-- body. Calling this function for an empty body therefore indicates an invalid
-- game state.
wormHead :: Worm -> Position
wormHead worm =
    case wormBody worm of
        headPosition : _ -> headPosition
        [] -> error "wormHead: wormBody is empty"


-- | Returns the position directly ahead of a worm in its current direction.
nextHeadPosition :: Worm -> Position
nextHeadPosition worm =
    moveForward
        (wormHead worm)
        (wormDirection worm)


-- | Returns the head position the worm would reach after performing an action.
--
-- The worm itself is not modified.
headAfterAction :: Worm -> Action -> Position
headAfterAction worm action =
    moveForward
        (wormHead worm)
        (applyAction (wormDirection worm) action)


-- -----------------------------------------------------------------------------
-- Body movement
-- -----------------------------------------------------------------------------

-- | Removes the final segment from a body while preserving the order of all
-- remaining segments.
removeTail :: [Position] -> [Position]
removeTail [] = []
removeTail [_] = []
removeTail (segment : rest) =
    segment : removeTail rest


-- | Builds a worm body after moving to a new head position.
--
-- A growing worm keeps its previous tail, increasing its length by one.
-- Otherwise the old tail is removed so that the body keeps the same length.
advanceBody :: Bool -> Position -> [Position] -> [Position]
advanceBody grows newHead oldBody =
    case oldBody of
        []  -> [newHead]
        _
            | grows -> newHead : oldBody
            | otherwise -> newHead : removeTail oldBody


-- | Moves a worm one cell forward without changing its current direction.
--
-- When @grows@ is 'True', the old tail remains in the body. Statistics and
-- other game-wide effects are handled outside this module.
moveWorm :: Bool -> Worm -> Worm
moveWorm grows worm =
    worm
        {
            wormBody =
                advanceBody
                    grows
                    (nextHeadPosition worm)
                    (wormBody worm)
        }


-- | Applies a relative action and then moves the worm one cell forward.
moveWormAfterAction :: Bool -> Action -> Worm -> Worm
moveWormAfterAction grows action worm =
    moveWorm grows turnedWorm
  where
    turnedWorm =
        worm
            {
                wormDirection =
                    applyAction
                        (wormDirection worm)
                        action
            }