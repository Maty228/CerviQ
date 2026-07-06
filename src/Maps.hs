module Maps where

import qualified Data.Map as Map
import Types


isInsideMap :: GameMap -> Position -> Bool
isInsideMap gameMap (x,y) =
    x >= 0 &&
    y >= 0 &&
    x < mapWidth gameMap &&
    y < mapHeight gameMap



-- Returns the position on the GameMap
-- Nothing is a position outside the map
-- Just Empty is a position inside the Map and not specified in mapTiles (blank space)
-- Just `Tile` is a position inside the Map of the type `Tile`
tileAt :: GameMap -> Position -> Maybe Tile
tileAt gameMap pos
    | not (isInsideMap gameMap pos) = Nothing
    | otherwise = Just (Map.findWithDefault Empty pos (mapTiles gameMap))


isWall :: GameMap -> Position -> Bool
isWall gameMap pos = tileAt gameMap pos == Just Wall

isFood :: GameMap -> Position -> Bool
isFood gameMap pos = tileAt gameMap pos == Just Food


-- # -> Wall
-- F -> Food
-- P -> Poison
-- . -> Empty
tileFromChar :: Char -> Maybe Tile
tileFromChar '#' = Just Wall
tileFromChar 'F' = Just Food
tileFromChar '.' = Nothing
tileFromChar 'P' = Just Poison
tileFromChar c =
    error ("Unknown map character: " ++ [c])



parseRow :: Int -> String -> [(Position, Tile)]
parseRow y row = concatMap parseCell (zip [0..] row)
    where
        parseCell :: (Int, Char) -> [(Position, Tile)]
        parseCell (x, char) =
            case tileFromChar char of
                Just tile -> [((x, y), tile)]
                Nothing -> []

fromAsciiMap :: [String] -> GameMap
fromAsciiMap stringMap =
    let
        width = length (head stringMap)
        height = length stringMap
        rows = zip [0..] stringMap
        tiles = concatMap (uncurry parseRow) rows
    in
        GameMap {
            mapWidth = width,
            mapHeight = height,
            mapTiles = Map.fromList tiles
        }


