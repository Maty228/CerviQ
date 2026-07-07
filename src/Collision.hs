module Collision where


-- -----------------------------------------------------------------------------
-- Map collisions
-- -----------------------------------------------------------------------------

-- | Returns True if the given position is blocked by a wall or lies outside the map.
isBlocked :: GameMap -> Position -> Bool
isBlocked currentMap pos =
    case tileAt currentMap pos of
        Nothing -> True
        Just tile -> tile == Wall 


-- | Returns True if the worm would hit a wall by moving forward.
wouldHitWall :: GameMap -> Worm -> Bool
wouldHitWall currentMap worm =
    isBlocked currentMap (nextHeadPosition worm)


-- | Returns True if the worm would hit a wall after performing the given action.
wouldHitWallAfterAction :: GameMap -> Action -> Worm -> Bool
wouldHitWallAfterAction currentMap action worm =
    let newDirection = applyAction (wormDirection worm) action
        turnedWorm = worm {wormDirection = newDirection}
    in wouldHitWall currentMap turnedWorm


-- -----------------------------------------------------------------------------
-- Worm collisions
-- -----------------------------------------------------------------------------

-- | Returns all positions currently occupied by the given worms.
occupiedPositions :: [Worm] -> [Position]
occupiedPositions worms =
    concatMap wormBody worms


-- | Simulates the next state of all worms after executing their actions.
futureWorms :: [(Bool, Action, Worm)] -> [Worm]
futureWorms moves =
    map (\(grows, action, worm) -> moveWormAfterAction grows action worm) moves


-- | Returns all positions occupied after the simulated movement.
futureOccupiedPositions :: [(Bool, Action, Worm)] -> [Position]
futureOccupiedPositions moves =
    occupiedPositions (futureWorms moves)


-- | Counts how many times the given position occurs in the list.
countPosition :: Position -> [Position] -> Int
countPosition pos positions =
    length (filter (== pos) positions)


-- | Returns True if the worm's head collides with another occupied position.
headCollision :: [Position] -> Worm -> Bool
headCollision positions worm =
    countPosition (wormHead worm) positions > 1


-- | Returns True if the given position is occupied.
positionOccupied :: Position -> [Position] -> Bool
positionOccupied pos positions =
    pos `elem` positions


-- | Checks whether the worm collides with another worm.
wormCollidesWithBodies :: [Worm] -> Worm -> Bool
wormCollidesWithBodies worms worm =
    headCollision (occupiedPositions worms) worm


-- | Checks whether the worm collides with the map.
wormCollidesWithMap :: GameMap -> Worm -> Bool
wormCollidesWithMap currentMap worm =
    isBlocked currentMap (wormHead worm)


-- | Checks whether the worm collides with either the map or another worm.
wormCollides :: GameMap -> [Worm] -> Worm -> Bool
wormCollides currentMap worms worm =
    wormCollidesWithMap currentMap worm ||
    wormCollidesWithBodies worms worm


-- -----------------------------------------------------------------------------
-- Turn simulation
-- -----------------------------------------------------------------------------

-- | Simulates one game turn and separates surviving and colliding worms.
simulateTurn :: GameMap -> [(Bool, Action, Worm)] -> ([Worm], [Worm])
simulateTurn currentMap moves =
    let movedWorms = futureWorms moves
        collidingWorms = filter (wormCollides currentMap movedWorms) movedWorms
        survivingWorms = filter (not . wormCollides currentMap movedWorms) movedWorms
    in (survivingWorms, collidingWorms)
