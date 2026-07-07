module Collision where

import Types
import Maps
import Movement

import Data.List (find)
import Data.Maybe (mapMaybe)


-- | Describes why a worm died during a turn.
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


-- | Checks whether the worm collides with the map or another worm.
wormCollides :: GameMap -> [Worm] -> Worm -> Bool
wormCollides currentMap worms worm =
    case deathReason currentMap worms worm of
        Just _ -> True
        Nothing -> False

-- | Returns the worm body without its head.
bodyWithoutHead :: Worm -> [Position]
bodyWithoutHead worm =
    case wormBody worm of
        [] -> []
        _ : body -> body


-- -----------------------------------------------------------------------------
-- Turn simulation
-- -----------------------------------------------------------------------------

-- | Returns the first worm whose body contains the given position.
bodyOwnerAt :: Position -> [Worm] -> Maybe Worm
bodyOwnerAt pos worms =
    find (\worm -> pos `elem` bodyWithoutHead worm) worms


-- | Returns IDs of other worms whose heads are on the same position.
headToHeadIds :: [Worm] -> Worm -> [Int]
headToHeadIds worms worm =
    [ wormId other
    | other <- worms
    , wormId other /= wormId worm
    , wormHead other == wormHead worm
    ]


-- | Determines whether the given moved worm died and why.
deathReason :: GameMap -> [Worm] -> Worm -> Maybe (Worm, DeathReason)
deathReason currentMap worms worm
    | isBlocked currentMap headPos =
        Just (worm, HitWall)

    | isPoison currentMap headPos =
        Just (worm, HitPoison)

    | headPos `elem` bodyWithoutHead worm =
        Just (worm, HitOwnBody)

    | not (null headHits) =
        Just (worm, HeadToHead (wormId worm : headHits))

    | otherwise =
        case bodyOwnerAt headPos otherWorms of
            Just killer ->
                Just (worm, HitOtherBody (wormId killer))

            Nothing ->
                Nothing
  where
    headPos = wormHead worm

    otherWorms =
        filter (\other -> wormId other /= wormId worm) worms

    headHits =
        headToHeadIds worms worm


-- | Simulates one game turn and separates surviving worms from deaths.
simulateTurn :: GameMap -> [(Bool, Action, Worm)] -> ([Worm], [(Worm, DeathReason)])
simulateTurn currentMap moves =
    let movedWorms = futureWorms moves
        deaths = mapMaybe (deathReason currentMap movedWorms) movedWorms
        deadIds = map (wormId . fst) deaths
        survivingWorms =
            filter
                (\worm -> wormId worm `notElem` deadIds)
                movedWorms
    in (survivingWorms, deaths)
