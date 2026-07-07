module Maps where

import qualified Data.Map as Map
import Types

-- -----------------------------------------------------------------------------
-- Tile queries
-- -----------------------------------------------------------------------------

-- | Returns True if the position lies inside the map.
isInsideMap :: GameMap -> Position -> Bool
isInsideMap gamemap (x,y) =
    x >= 0 &&
    y >= 0 &&
    x < mapWidth gamemap &&
    y < mapHeight gamemap


-- | Returns the tile at the given position.
--
-- * Nothing means the position lies outside the map.
-- * Just Empty means the position is inside the map but has no explicit tile.
-- * Just tile means the position contains the corresponding tile.
tileAt :: GameMap -> Position -> Maybe Tile
tileAt gamemap pos
    | not (isInsideMap gamemap pos) = Nothing
    | otherwise = Just (Map.findWithDefault Empty pos (mapTiles gamemap))


-- | Returns True if the position contains a wall.
isWall :: GameMap -> Position -> Bool
isWall gamemap pos = tileAt gamemap pos == Just Wall

-- | Returns True if the position contains food.
isFood :: GameMap -> Position -> Bool
isFood gamemap pos = tileAt gamemap pos == Just Food

-- | Returns True if the position contains poison.
isPoison :: GameMap -> Position -> Bool
isPoison gamemap pos = tileAt gamemap pos == Just Poison

-- | Returns True if the position is an empty map tile.
isEmptyTile :: GameMap -> Position -> Bool
isEmptyTile gamemap pos = tileAt gamemap pos == Just Empty


-- -----------------------------------------------------------------------------
-- Map manipulation
-- -----------------------------------------------------------------------------

-- | Returns every valid position on the map.
allMapPositions :: GameMap -> [Position]
allMapPositions gamemap =
    [ (x, y) | y <- [0 .. mapHeight gamemap -1]
    , x <- [0 .. mapWidth gamemap -1]]


-- | Sets the given tile at the specified position.
setTile :: Position -> Tile -> GameMap -> GameMap
setTile pos tile gamemap =
    gamemap {mapTiles = Map.insert pos tile (mapTiles gamemap)}

-- | Removes the explicit tile from the given position.
removeTile :: Position -> GameMap -> GameMap
removeTile pos gamemap =
    gamemap {mapTiles = Map.delete pos (mapTiles gamemap)}


-- | Places food at the given position.
placeFood :: Position -> GameMap -> GameMap
placeFood pos = setTile pos Food

-- | Removes food from the given position if present.
removeFood :: Position -> GameMap -> GameMap
removeFood pos gamemap
    | isFood gamemap pos = removeTile pos gamemap
    | otherwise = gamemap

-- -----------------------------------------------------------------------------
-- Map queries
-- -----------------------------------------------------------------------------

-- | Returns all positions containing the specified tile.
positionsOfTile :: Tile -> GameMap -> [Position]
positionsOfTile tile gamemap =
    [ pos
    | (pos, currentTile) <- Map.toList (mapTiles gamemap)
    , currentTile == tile
    ]

foodPositions :: GameMap -> [Position]
foodPositions = positionsOfTile Food

wallPositions :: GameMap -> [Position]
wallPositions = positionsOfTile Wall

poisonPositions :: GameMap -> [Position]
poisonPositions = positionsOfTile Poison


-- -----------------------------------------------------------------------------
-- ASCII map parsing
-- -----------------------------------------------------------------------------

-- | Converts an ASCII character into a map tile.
tileFromChar :: Char -> Maybe Tile
tileFromChar '#' = Just Wall
tileFromChar 'F' = Just Food
tileFromChar '.' = Nothing
tileFromChar 'P' = Just Poison
tileFromChar c =
    error ("Unknown map character: " ++ [c])


-- | Parses a single ASCII map row.
parseRow :: Int -> String -> [(Position, Tile)]
parseRow y row = concatMap parseCell (zip [0..] row)
    where
        parseCell :: (Int, Char) -> [(Position, Tile)]
        parseCell (x, char) =
            case tileFromChar char of
                Just tile -> [((x, y), tile)]
                Nothing -> []

-- | Builds a game map from its ASCII representation.
fromAsciiMap :: [String] -> GameMap
fromAsciiMap stringMap =
    let
        width = length (head stringMap)
        height = length stringMap
        rows = zip [0..] stringMap
        tiles = concatMap (uncurry parseRow) rows
    in
        GameMap
            { mapWidth = width
            , mapHeight = height
            , mapTiles = Map.fromList tiles
            }
