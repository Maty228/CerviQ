{-|
Module      : Collision
Description : Collision detection and simultaneous worm-movement resolution.

This module determines whether worm positions are blocked by the map or collide
with worm bodies. For a complete game turn, all worms are first moved according
to their actions and collisions are then evaluated on the resulting shared
state. This ensures that worm movement is resolved simultaneously.
-}

module Collision where

import Maps
import Movement
import Types

import Data.List (find)
import Data.Maybe (mapMaybe)


-- -----------------------------------------------------------------------------
-- Death reasons
-- -----------------------------------------------------------------------------

-- | Reason why a worm died during one game tick.
data DeathReason
    = HitWall
    | HitPoison
    | HitOwnBody
    | HitOtherBody Int
    | HeadToHead [Int]
    deriving (Show, Eq)


-- -----------------------------------------------------------------------------
-- Map collisions
-- -----------------------------------------------------------------------------

-- | Returns whether a position is a wall or lies outside the map.
--
-- Positions outside the map are treated as blocked so movement code does not
-- need a separate boundary check.
isBlocked :: GameMap -> Position -> Bool
isBlocked currentMap pos =
    case tileAt currentMap pos of
        Nothing -> True
        Just tile -> tile == Wall


-- | Returns whether a worm would hit a blocked map position by moving forward.
wouldHitWall :: GameMap -> Worm -> Bool
wouldHitWall currentMap worm =
    isBlocked currentMap (nextHeadPosition worm)


-- | Returns whether a worm would hit a blocked map position after an action.
wouldHitWallAfterAction :: GameMap -> Action -> Worm -> Bool
wouldHitWallAfterAction currentMap action worm =
    isBlocked currentMap (headAfterAction worm action)


-- -----------------------------------------------------------------------------
-- Occupied positions
-- -----------------------------------------------------------------------------

-- | Returns all positions occupied by the given worms.
occupiedPositions :: [Worm] -> [Position]
occupiedPositions worms =
    concatMap wormBody worms


-- | Returns whether a position is occupied by any worm segment.
positionOccupied :: Position -> [Position] -> Bool
positionOccupied pos positions =
    pos `elem` positions


-- | Counts how many times a position occurs in a collection of positions.
countPosition :: Position -> [Position] -> Int
countPosition pos positions =
    length (filter (== pos) positions)


-- | Returns the worm body without its head.
bodyWithoutHead :: Worm -> [Position]
bodyWithoutHead worm =
    case wormBody worm of
        [] -> []
        _ : body -> body


-- -----------------------------------------------------------------------------
-- Collision helpers
-- -----------------------------------------------------------------------------

-- | Returns whether a worm's head shares a position with another worm segment.
headCollision :: [Position] -> Worm -> Bool
headCollision positions worm =
    countPosition (wormHead worm) positions > 1


-- | Returns whether a worm collides with any worm body.
wormCollidesWithBodies :: [Worm] -> Worm -> Bool
wormCollidesWithBodies worms worm =
    headCollision (occupiedPositions worms) worm


-- | Returns whether a worm collides with a blocked map position.
wormCollidesWithMap :: GameMap -> Worm -> Bool
wormCollidesWithMap currentMap worm =
    isBlocked currentMap (wormHead worm)


-- | Returns whether a worm has any fatal collision in the current state.
wormCollides :: GameMap -> [Worm] -> Worm -> Bool
wormCollides currentMap worms worm =
    case deathReason currentMap worms worm of
        Just _ -> True
        Nothing -> False


-- | Returns the first worm whose body, excluding its head, contains a position.
bodyOwnerAt :: Position -> [Worm] -> Maybe Worm
bodyOwnerAt pos worms =
    find (\worm -> pos `elem` bodyWithoutHead worm) worms


-- | Returns IDs of other worms whose heads occupy the same position.
headToHeadIds :: [Worm] -> Worm -> [Int]
headToHeadIds worms worm =
    [ wormId other
    | other <- worms
    , wormId other /= wormId worm
    , wormHead other == wormHead worm
    ]


-- -----------------------------------------------------------------------------
-- Death resolution
-- -----------------------------------------------------------------------------

-- | Determines whether a moved worm died and returns the corresponding reason.
--
-- Collision types are checked in a fixed order: blocked map positions, poison,
-- the worm's own body, another head, and finally another worm's body.
deathReason :: GameMap -> [Worm] -> Worm -> Maybe (Worm, DeathReason)
deathReason currentMap worms worm
    | isBlocked currentMap headPos = Just (worm, HitWall)
    | isPoison currentMap headPos = Just (worm, HitPoison)
    | headPos `elem` bodyWithoutHead worm = Just (worm, HitOwnBody)
    | not (null headHits) = Just (worm, HeadToHead (wormId worm : headHits))
    | otherwise =
        case bodyOwnerAt headPos otherWorms of
            Just killer -> Just (worm, HitOtherBody (wormId killer))
            Nothing -> Nothing
  where
    headPos = wormHead worm
    otherWorms = filter (\other -> wormId other /= wormId worm) worms
    headHits = headToHeadIds worms worm


-- -----------------------------------------------------------------------------
-- Simultaneous turn simulation
-- -----------------------------------------------------------------------------

-- | Moves all worms according to their prepared actions.
--
-- The Boolean value specifies whether the corresponding worm grows this tick.
futureWorms :: [(Bool, Action, Worm)] -> [Worm]
futureWorms moves =
    map (\(grows, action, worm) -> moveWormAfterAction grows action worm) moves


-- | Returns all positions occupied after the prepared movements are applied.
futureOccupiedPositions :: [(Bool, Action, Worm)] -> [Position]
futureOccupiedPositions moves =
    occupiedPositions (futureWorms moves)


-- | Simulates simultaneous worm movement and separates survivors from deaths.
--
-- Every worm is moved before any collision is resolved. Therefore all
-- collision checks observe the same resulting state rather than depending on
-- the order in which worms appear in the list.
simulateTurn :: GameMap -> [(Bool, Action, Worm)] -> ([Worm], [(Worm, DeathReason)])
simulateTurn currentMap moves =
    (survivingWorms, deaths)
  where
    movedWorms = futureWorms moves
    deaths = mapMaybe (deathReason currentMap movedWorms) movedWorms
    deadIds = map (wormId . fst) deaths
    survivingWorms = filter (\worm -> wormId worm `notElem` deadIds) movedWorms