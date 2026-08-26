module Viewport where

import Types


-- -----------------------------------------------------------------------------
-- Viewport configuration
-- -----------------------------------------------------------------------------

-- | Number of map cells visible horizontally in camera mode.
--
-- Change this value to make the viewport wider or narrower.
viewportWidthCells :: Int
viewportWidthCells = 30


-- | Number of map cells visible vertically in camera mode.
--
-- Change this value to make the viewport taller or shorter.
viewportHeightCells :: Int
viewportHeightCells = 18


-- | Maximum pixel width reserved for the visible map.
viewportMaxPixelWidth :: Float
viewportMaxPixelWidth = 800


-- | Maximum pixel height reserved for the visible map.
viewportMaxPixelHeight :: Float
viewportMaxPixelHeight = 640


-- | Maximum size of one map cell in viewport mode.
viewportMaxCellSize :: Float
viewportMaxCellSize = 40

-- | Distance in pixels between an opponent indicator and the viewport edge.
opponentIndicatorInset :: Float
opponentIndicatorInset = 14


-- | Distance in pixels between the nearest-food indicator and the viewport
-- edge. Food is drawn slightly farther inward so overlapping indicators remain
-- easier to distinguish.
foodIndicatorInset :: Float
foodIndicatorInset = 36

-- -----------------------------------------------------------------------------
-- Viewport
-- -----------------------------------------------------------------------------

-- | Rectangular part of a game map currently visible on screen.
data Viewport = Viewport
    {
        viewportMinX :: Int,
        viewportMinY :: Int,
        viewportWidth :: Int,
        viewportHeight :: Int,
        viewportCellSize :: Float
    }
    deriving (Show, Eq)


-- | Returns True if the map is larger than the configured viewport.
usesViewport :: GameMap -> Bool
usesViewport gameMap' =
    mapWidth gameMap' > viewportWidthCells
        || mapHeight gameMap' > viewportHeightCells


-- | Restricts an integer value to the given inclusive interval.
clampInt :: Int -> Int -> Int -> Int
clampInt minimumValue maximumValue value =
    max minimumValue (min maximumValue value)


-- | Computes a cell size that keeps the configured viewport inside the board
-- drawing area.
cellSizeForViewport :: Int -> Int -> Float
cellSizeForViewport width height =
    min
        viewportMaxCellSize
        ( min
            (viewportMaxPixelWidth / fromIntegral width)
            (viewportMaxPixelHeight / fromIntegral height)
        )


-- | Creates a viewport centered as closely as possible around the given map
-- position while remaining fully inside the map.
viewportAround :: GameMap -> Position -> Viewport
viewportAround gameMap' (focusX, focusY) =
    Viewport
        {
            viewportMinX = minimumX,
            viewportMinY = minimumY,
            viewportWidth = visibleWidth,
            viewportHeight = visibleHeight,
            viewportCellSize =
                cellSizeForViewport
                    visibleWidth
                    visibleHeight
        }
  where
    visibleWidth =
        min viewportWidthCells (mapWidth gameMap')

    visibleHeight =
        min viewportHeightCells (mapHeight gameMap')

    maximumX =
        max 0 (mapWidth gameMap' - visibleWidth)

    maximumY =
        max 0 (mapHeight gameMap' - visibleHeight)

    minimumX =
        clampInt
            0
            maximumX
            (focusX - visibleWidth `div` 2)

    minimumY =
        clampInt
            0
            maximumY
            (focusY - visibleHeight `div` 2)


-- | Returns True if a map position lies inside the visible viewport.
positionInViewport :: Viewport -> Position -> Bool
positionInViewport viewport (x, y) =
    x >= viewportMinX viewport
        && y >= viewportMinY viewport
        && x < viewportMinX viewport + viewportWidth viewport
        && y < viewportMinY viewport + viewportHeight viewport


-- | Returns all map positions currently visible in the viewport.
viewportPositions :: Viewport -> [Position]
viewportPositions viewport =
    [
        (x, y)
        | y <- [viewportMinY viewport .. viewportMinY viewport + viewportHeight viewport - 1],
          x <- [viewportMinX viewport .. viewportMinX viewport + viewportWidth viewport - 1]
    ]


-- | Converts an absolute map position into Gloss coordinates relative to the
-- visible viewport.
viewportPositionToScreen :: Viewport -> Position -> (Float, Float)
viewportPositionToScreen viewport (x, y) =
    (screenX, screenY)
  where
    cellSize =
        viewportCellSize viewport

    localX =
        x - viewportMinX viewport

    localY =
        y - viewportMinY viewport

    pixelWidth =
        fromIntegral (viewportWidth viewport) * cellSize

    pixelHeight =
        fromIntegral (viewportHeight viewport) * cellSize

    screenX =
        fromIntegral localX * cellSize
            - pixelWidth / 2
            + cellSize / 2

    screenY =
        pixelHeight / 2
            - fromIntegral localY * cellSize
            - cellSize / 2


-- -----------------------------------------------------------------------------
-- Off-screen indicators
-- -----------------------------------------------------------------------------

-- | Returns the squared geometric distance between two map positions.
--
-- Squared distance is sufficient for comparing positions and avoids an
-- unnecessary square-root calculation.
squaredDistance :: Position -> Position -> Int
squaredDistance (x1, y1) (x2, y2) =
    dx * dx + dy * dy
  where
    dx = x2 - x1
    dy = y2 - y1


-- | Returns the position nearest to the given origin.
nearestPosition :: Position -> [Position] -> Maybe Position
nearestPosition _ [] =
    Nothing

nearestPosition origin (firstPosition : remainingPositions) =
    Just
        (foldl chooseCloser firstPosition remainingPositions)
  where
    chooseCloser currentBest candidate
        | squaredDistance origin candidate < squaredDistance origin currentBest =
            candidate

        | otherwise =
            currentBest


-- | Computes the screen position and rotation angle of an indicator pointing
-- from the focused map position towards an off-screen target.
--
-- Nothing is returned when the target is already visible.
offscreenIndicatorPlacement :: Float -> Viewport -> Position -> Position -> Maybe (Float, Float, Float)
offscreenIndicatorPlacement inset viewport focusPosition targetPosition
    | positionInViewport viewport targetPosition =
        Nothing

    | deltaX == 0 && deltaY == 0 =
        Nothing

    | otherwise =
        Just
            (
                fromIntegral deltaX * scaleFactor,
                fromIntegral deltaY * scaleFactor,
                angle
            )
  where
    (focusX, focusY) =
        focusPosition

    (targetX, targetY) =
        targetPosition

    deltaX =
        targetX - focusX

    -- Map Y increases downwards while Gloss Y increases upwards.
    deltaY =
        focusY - targetY

    cellSize =
        viewportCellSize viewport

    halfWidth =
        fromIntegral (viewportWidth viewport) * cellSize / 2 - inset

    halfHeight =
        fromIntegral (viewportHeight viewport) * cellSize / 2 - inset

    horizontalScale
        | deltaX == 0 = 1 / 0
        | otherwise =
            halfWidth / abs (fromIntegral deltaX)

    verticalScale
        | deltaY == 0 = 1 / 0
        | otherwise =
            halfHeight / abs (fromIntegral deltaY)

    scaleFactor =
        min horizontalScale verticalScale

    angle =
        atan2
            (fromIntegral deltaY)
            (fromIntegral deltaX)
            * 180
            / pi