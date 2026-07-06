module Maps where

import qualified Data.Map as Map
import Types


isInsideMap :: GameMap -> Position -> Bool
isInsideMap gamemap (x,y) =
    x >= 0 &&
    y >= 0 &&
    x < mapWidth gamemap &&
    y < mapHeight gamemap



-- Returns the position on the GameMap
-- Nothing is a position outside the map
-- Just Empty is a position inside the Map and not specified in mapTiles (blank space)
-- Just `Tile` is a position inside the Map of the type `Tile`
tileAt :: GameMap -> Position -> Maybe Tile
tileAt gamemap pos
    | not (isInsideMap gamemap pos) = Nothing
    | otherwise = Just (Map.findWithDefault Empty pos (mapTiles gamemap))


isWall :: GameMap -> Position -> Bool
isWall gamemap pos = tileAt gamemap pos == Just Wall

isFood :: GameMap -> Position -> Bool
isFood gamemap pos = tileAt gamemap pos == Just Food

isPoison :: GameMap -> Position -> Bool
isPoison gamemap pos = tileAt gamemap pos == Just Poison

isEmptyTile :: GameMap -> Position -> Bool
isEmptyTile gamemap pos = tileAt gamemap pos == Just Empty


allMapPositions :: GameMap -> [Position]
allMapPositions gamemap =
    [ (x, y) | y <- [0 .. mapHeight gamemap -1]
    , x <- [0 .. mapWidth gamemap -1]]


setTile :: Position -> Tile -> GameMap -> GameMap
setTile pos tile gamemap =
    gamemap {mapTiles = Map.insert pos tile (mapTiles gamemap)}

removeTile :: Position -> GameMap -> GameMap
removeTile pos gamemap =
    gamemap {mapTiles = Map.delete pos (mapTiles gamemap)}


placeFood :: Position -> GameMap -> GameMap
placeFood pos = setTile pos Food

removeFood :: Position -> GameMap -> GameMap
removeFood pos gamemap
    | isFood gamemap pos = removeTile pos gamemap
    | otherwise = gamemap

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





