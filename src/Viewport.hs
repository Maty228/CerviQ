{-|
Module      : Viewport
Description : Camera calculations for displaying large CerviQ maps.

This module defines the rectangular camera used when a map is too large to show
comfortably in full. It computes a viewport around a focused worm, converts map
coordinates into viewport-relative Gloss coordinates, and provides geometry for
off-screen food and opponent indicators.

The viewport affects rendering only; the game state and AI agents always retain
access to the complete map.
-}

module Viewport where

import Types


-- -----------------------------------------------------------------------------
-- Viewport configuration
-- -----------------------------------------------------------------------------

-- | Number of map cells visible horizontally in camera mode.
viewportWidthCells :: Int
viewportWidthCells = 30


-- | Number of map cells visible vertically in camera mode.
viewportHeightCells :: Int
viewportHeightCells = 18


-- | Maximum pixel width reserved for the visible map.
viewportMaxPixelWidth :: Float
viewportMaxPixelWidth = 800


-- | Maximum pixel height reserved for the visible map.
viewportMaxPixelHeight :: Float
viewportMaxPixelHeight = 640


-- | Maximum rendered size of one map cell.
viewportMaxCellSize :: Float
viewportMaxCellSize = 40


-- | Distance between an opponent indicator and the viewport edge.
opponentIndicatorInset :: Float
opponentIndicatorInset = 14


-- | Distance between the nearest-food indicator and the viewport edge.
--
-- Food is drawn slightly farther inward to reduce overlap with opponent
-- indicators.
foodIndicatorInset :: Float
foodIndicatorInset = 36


-- -----------------------------------------------------------------------------
-- Viewport definition
-- -----------------------------------------------------------------------------

-- | Rectangular part of a game map currently visible on screen.
data Viewport = Viewport
    {
        -- | Leftmost visible map coordinate.
        viewportMinX :: Int,

        -- | Topmost visible map coordinate.
        viewportMinY :: Int,

        -- | Number of visible horizontal cells.
        viewportWidth :: Int,

        -- | Number of visible vertical cells.
        viewportHeight :: Int,

        -- | Rendered size of one visible cell in pixels.
        viewportCellSize :: Float
    }
    deriving (Show, Eq)


-- | Returns whether a map is larger than the configured camera area.
usesViewport :: GameMap -> Bool
usesViewport gameMap' =
    mapWidth gameMap' > viewportWidthCells
        || mapHeight gameMap' > viewportHeightCells


-- | Restricts a value to an inclusive integer interval.
clampInt :: Int -> Int -> Int -> Int
clampInt minimumValue maximumValue value =
    max minimumValue (min maximumValue value)


-- | Computes a cell size that keeps the visible map inside the drawing area.
cellSizeForViewport :: Int -> Int -> Float
cellSizeForViewport width height =
    min viewportMaxCellSize $
        min
            (viewportMaxPixelWidth / fromIntegral width)
            (viewportMaxPixelHeight / fromIntegral height)


-- | Creates a viewport centered as closely as possible on a map position.
--
-- Near map boundaries the viewport itself is clamped instead of displaying
-- coordinates outside the map, so the focused position will no longer remain
-- exactly in the center.
viewportAround :: GameMap -> Position -> Viewport
viewportAround gameMap' (focusX, focusY) =
    Viewport
        { viewportMinX = minimumX
        , viewportMinY = minimumY
        , viewportWidth = visibleWidth
        , viewportHeight = visibleHeight
        , viewportCellSize = cellSizeForViewport visibleWidth visibleHeight
        }
  where
    visibleWidth = min viewportWidthCells (mapWidth gameMap')
    visibleHeight = min viewportHeightCells (mapHeight gameMap')

    maximumX = max 0 (mapWidth gameMap' - visibleWidth)
    maximumY = max 0 (mapHeight gameMap' - visibleHeight)

    minimumX = clampInt 0 maximumX (focusX - visibleWidth `div` 2)
    minimumY = clampInt 0 maximumY (focusY - visibleHeight `div` 2)


-- -----------------------------------------------------------------------------
-- Visible positions and coordinate conversion
-- -----------------------------------------------------------------------------

-- | Returns whether a map position lies inside a viewport.
positionInViewport :: Viewport -> Position -> Bool
positionInViewport viewport (x, y) =
    x >= viewportMinX viewport
        && y >= viewportMinY viewport
        && x < viewportMinX viewport + viewportWidth viewport
        && y < viewportMinY viewport + viewportHeight viewport


-- | Returns every map position currently visible inside a viewport.
viewportPositions :: Viewport -> [Position]
viewportPositions viewport =
    [ (x, y)
    | y <- [viewportMinY viewport .. viewportMinY viewport + viewportHeight viewport - 1]
    , x <- [viewportMinX viewport .. viewportMinX viewport + viewportWidth viewport - 1]
    ]


-- | Converts an absolute map position into viewport-relative Gloss coordinates.
--
-- Map Y coordinates increase downwards, while Gloss Y coordinates increase
-- upwards, so the vertical axis is inverted during conversion.
viewportPositionToScreen :: Viewport -> Position -> (Float, Float)
viewportPositionToScreen viewport (x, y) =
    (screenX, screenY)
  where
    cellSize = viewportCellSize viewport
    localX = x - viewportMinX viewport
    localY = y - viewportMinY viewport

    pixelWidth = fromIntegral (viewportWidth viewport) * cellSize
    pixelHeight = fromIntegral (viewportHeight viewport) * cellSize

    screenX = fromIntegral localX * cellSize - pixelWidth / 2 + cellSize / 2
    screenY = pixelHeight / 2 - fromIntegral localY * cellSize - cellSize / 2


-- -----------------------------------------------------------------------------
-- Off-screen indicators
-- -----------------------------------------------------------------------------

-- | Returns squared Euclidean distance between two map positions.
--
-- Squared distance is sufficient when only relative distances are compared.
squaredDistance :: Position -> Position -> Int
squaredDistance (x1, y1) (x2, y2) =
    dx * dx + dy * dy
  where
    dx = x2 - x1
    dy = y2 - y1


-- | Returns the geometrically nearest position to an origin.
nearestPosition :: Position -> [Position] -> Maybe Position
nearestPosition _ [] =
    Nothing

nearestPosition origin (firstPosition : remainingPositions) =
    Just (foldl chooseCloser firstPosition remainingPositions)
  where
    chooseCloser currentBest candidate
        | squaredDistance origin candidate < squaredDistance origin currentBest = candidate
        | otherwise = currentBest


-- | Computes edge placement and angle for an indicator pointing at an
-- off-screen target.
--
-- The returned tuple contains @(screenX, screenY, angleDegrees)@. 'Nothing' is
-- returned when the target is already visible.
offscreenIndicatorPlacement :: Float -> Viewport -> Position -> Position -> Maybe (Float, Float, Float)
offscreenIndicatorPlacement inset viewport focusPosition targetPosition
    | positionInViewport viewport targetPosition = Nothing
    | deltaX == 0 && deltaY == 0 = Nothing
    | otherwise =
        Just
            ( fromIntegral deltaX * scaleFactor
            , fromIntegral deltaY * scaleFactor
            , angleDegrees
            )
  where
    (focusX, focusY) = focusPosition
    (targetX, targetY) = targetPosition

    deltaX = targetX - focusX

    -- Map Y increases downwards, but Gloss Y increases upwards.
    deltaY = focusY - targetY

    cellSize = viewportCellSize viewport
    halfWidth = fromIntegral (viewportWidth viewport) * cellSize / 2 - inset
    halfHeight = fromIntegral (viewportHeight viewport) * cellSize / 2 - inset

    scaleFactor
        | deltaX == 0 = halfHeight / abs (fromIntegral deltaY)
        | deltaY == 0 = halfWidth / abs (fromIntegral deltaX)
        | otherwise =
            min
                (halfWidth / abs (fromIntegral deltaX))
                (halfHeight / abs (fromIntegral deltaY))

    angleDegrees =
        atan2 (fromIntegral deltaY) (fromIntegral deltaX) * 180 / pi