{-|
Module      : Maps
Description : Queries, manipulation, and construction of CerviQ game maps.

This module provides the basic operations for working with 'GameMap'. Maps use
a sparse representation: explicitly stored entries contain walls, food, poison,
or other non-empty tiles, while missing in-bounds positions are interpreted as
'Empty'. The module also supports constructing deterministic maps from a simple
ASCII representation.
-}

module Maps where

import qualified Data.Map as Map

import Types


-- -----------------------------------------------------------------------------
-- Map bounds and tile queries
-- -----------------------------------------------------------------------------

-- | Returns whether a position lies inside the rectangular map bounds.
isInsideMap :: GameMap -> Position -> Bool
isInsideMap gameMap' (x, y) =
    x >= 0 && y >= 0 && x < mapWidth gameMap' && y < mapHeight gameMap'


-- | Returns the tile at a map position.
--
-- 'Nothing' means the position lies outside the map. Missing positions inside
-- the sparse tile map are interpreted as 'Empty'.
tileAt :: GameMap -> Position -> Maybe Tile
tileAt gameMap' pos
    | not (isInsideMap gameMap' pos) = Nothing
    | otherwise = Just (Map.findWithDefault Empty pos (mapTiles gameMap'))


-- | Returns whether a position contains a wall.
isWall :: GameMap -> Position -> Bool
isWall gameMap' pos =
    tileAt gameMap' pos == Just Wall


-- | Returns whether a position contains food.
isFood :: GameMap -> Position -> Bool
isFood gameMap' pos =
    tileAt gameMap' pos == Just Food


-- | Returns whether a position contains poison.
isPoison :: GameMap -> Position -> Bool
isPoison gameMap' pos =
    tileAt gameMap' pos == Just Poison


-- | Returns whether a position is an empty map tile.
isEmptyTile :: GameMap -> Position -> Bool
isEmptyTile gameMap' pos =
    tileAt gameMap' pos == Just Empty


-- | Returns every valid position on the map.
allMapPositions :: GameMap -> [Position]
allMapPositions gameMap' =
    [ (x, y)
    | y <- [0 .. mapHeight gameMap' - 1]
    , x <- [0 .. mapWidth gameMap' - 1]
    ]


-- -----------------------------------------------------------------------------
-- Tile manipulation
-- -----------------------------------------------------------------------------

-- | Sets a tile at the specified position.
setTile :: Position -> Tile -> GameMap -> GameMap
setTile pos tile gameMap' =
    gameMap' {mapTiles = Map.insert pos tile (mapTiles gameMap')}


-- | Removes an explicitly stored tile from a position.
--
-- Because maps are sparse, an in-bounds position without an explicit tile is
-- interpreted as 'Empty'.
removeTile :: Position -> GameMap -> GameMap
removeTile pos gameMap' =
    gameMap' {mapTiles = Map.delete pos (mapTiles gameMap')}


-- | Places food at the specified position.
placeFood :: Position -> GameMap -> GameMap
placeFood pos =
    setTile pos Food


-- | Removes food from a position if food is currently present there.
removeFood :: Position -> GameMap -> GameMap
removeFood pos gameMap'
    | isFood gameMap' pos = removeTile pos gameMap'
    | otherwise = gameMap'


-- -----------------------------------------------------------------------------
-- Tile collections
-- -----------------------------------------------------------------------------

-- | Returns all positions containing a particular explicitly stored tile.
positionsOfTile :: Tile -> GameMap -> [Position]
positionsOfTile tile gameMap' =
    [ pos
    | (pos, currentTile) <- Map.toList (mapTiles gameMap')
    , currentTile == tile
    ]


-- | Returns all food positions on the map.
foodPositions :: GameMap -> [Position]
foodPositions =
    positionsOfTile Food


-- | Returns all wall positions on the map.
wallPositions :: GameMap -> [Position]
wallPositions =
    positionsOfTile Wall


-- | Returns all poison positions on the map.
poisonPositions :: GameMap -> [Position]
poisonPositions =
    positionsOfTile Poison


-- -----------------------------------------------------------------------------
-- ASCII map parsing
-- -----------------------------------------------------------------------------

-- | Converts one ASCII map character into an explicitly stored tile.
--
-- @#@ represents a wall, @F@ food, @P@ poison, and @.@ an empty position.
-- Empty positions return 'Nothing' because they do not need to be stored in
-- the sparse map representation.
tileFromChar :: Char -> Maybe Tile
tileFromChar '#' = Just Wall
tileFromChar 'F' = Just Food
tileFromChar 'P' = Just Poison
tileFromChar '.' = Nothing
tileFromChar c = error ("Unknown map character: " ++ [c])


-- | Parses one row of an ASCII map into explicitly stored tiles.
parseRow :: Int -> String -> [(Position, Tile)]
parseRow y row =
    concatMap parseCell (zip [0 ..] row)
  where
    parseCell (x, char) =
        case tileFromChar char of
            Just tile -> [((x, y), tile)]
            Nothing -> []


-- | Builds a rectangular game map from its ASCII representation.
--
-- The outer list represents rows from top to bottom. Every row must be
-- non-empty and have the same width.
fromAsciiMap :: [String] -> GameMap
fromAsciiMap [] =
    error "Cannot create a game map from an empty ASCII map."

fromAsciiMap stringMap@(firstRow : _)
    | null firstRow =
        error "Cannot create a game map with empty rows."
    | any ((/= width) . length) stringMap =
        error "Cannot create a game map whose rows have different widths."
    | otherwise =
        GameMap
            { mapWidth = width
            , mapHeight = length stringMap
            , mapTiles = Map.fromList tiles
            }
  where
    width = length firstRow
    tiles = concatMap (uncurry parseRow) (zip [0 ..] stringMap)