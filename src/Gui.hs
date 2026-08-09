module Gui where

import Maps ( tileAt, allMapPositions )
import Movement ()
import Types
import Config ( maxFoodCount )
import Controller ( collectActions )
import Game ( maintainFoodCount, stepGame )

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

-- -----------------------------------------------------------------------------
-- GUI configuration
-- -----------------------------------------------------------------------------

-- | Size of one map cell in pixels.
cellSize :: Float
cellSize = 40

-- | Width of the initial GUI window.
windowWidth :: Int
windowWidth = 1100


-- | Height of the initial GUI window.
windowHeight :: Int
windowHeight = 700


-- | Gloss window used by the game visualizer.
guiDisplay :: Display
guiDisplay =
    InWindow
        "CerviQ"
        (windowWidth, windowHeight)
        (100, 100)


-- -----------------------------------------------------------------------------
-- GUI state
-- -----------------------------------------------------------------------------

-- | One stored moment of the game simulation.
--
-- The actions are the actions that produced this snapshot from the previous one.
data GuiSnapshot = GuiSnapshot
    {
        snapshotGameState :: GameState,
        snapshotActions :: [(Int, Action)]
    }

-- | Complete state of the interactive GUI debugger.
data GuiWorld = GuiWorld
    {
        guiCurrent :: GuiSnapshot,
        guiHistory :: [GuiSnapshot],
        guiFuture :: [GuiSnapshot],
        guiInitialState :: GameState,
        guiControllers :: [(Int, Controller)],
        guiPaused :: Bool,
        guiSpeed :: Float,
        guiAccumulator :: Float
    }

-- | Creates the initial GUI debugger state.
initialGuiWorld :: [(Int, Controller)] -> GameState -> GuiWorld
initialGuiWorld controllers initialState =
    GuiWorld 
        {
            guiCurrent =
                GuiSnapshot 
                    {
                        snapshotGameState = initialState,
                        snapshotActions = []
                    },
            guiHistory = [],
            guiFuture = [],
            guiInitialState = initialState,
            guiControllers = controllers,
            guiPaused = True,
            guiSpeed = 3,
            guiAccumulator = 0       
        }

-- | Returns True if no worm is still alive.
guiGameFinished :: GameState -> Bool
guiGameFinished state = 
    not (any wormAlive (gameWorms state))

-- | Advances the GUI simulation by one game tick.
--
-- If a future snapshot already exists because the user previously stepped backwards,
-- that stored snapshot is replayed instead of recomputing the tick.
stepGuiForward :: GuiWorld -> IO GuiWorld
stepGuiForward world =
    case guiFuture world of
        nextSnapshot : remainingFuture ->
            pure
                world
                    {
                        guiCurrent = nextSnapshot,
                        guiHistory = guiCurrent world : guiHistory world,
                        guiFuture = remainingFuture,
                        guiAccumulator = 0
                    }
        [] -> computeNextSnapshot world

-- | Computes a new game snapshot using the configured controllers.
computeNextSnapshot :: GuiWorld -> IO GuiWorld
computeNextSnapshot world
    | guiGameFinished currentState =
        pure
            world
                {
                    guiPaused = True,
                    guiAccumulator = 0
                }
    | otherwise = do
        actions <- collectActions (guiControllers world) currentState
        let steppedState = stepGame actions currentState
        nextState <- maintainFoodCount maxFoodCount steppedState
        let nextSnapshot = GuiSnapshot
                {
                    snapshotGameState = nextState,
                    snapshotActions = actions
                }
        pure
            world
                {
                    guiCurrent = nextSnapshot,
                    guiHistory = guiCurrent world : guiHistory world,
                    guiFuture = [],
                    guiAccumulator = 0
                }
  where
    currentState = snapshotGameState (guiCurrent world)

-- | Moves the debugger one stored snapshot backwards.
stepGuiBackward :: GuiWorld -> GuiWorld
stepGuiBackward world =
    case guiHistory world of
        previousSnapshot : remainingHistory ->
            world
                {
                    guiCurrent = previousSnapshot,
                    guiHistory = remainingHistory,
                    guiFuture = guiCurrent world : guiFuture world,
                    guiPaused = True,
                    guiAccumulator = 0
                }
        [] -> world

-- | Restarts the GUI simulation from its original game state.
restartGui :: GuiWorld -> GuiWorld
restartGui world =
    world
        {
            guiCurrent = GuiSnapshot
                    {
                        snapshotGameState = guiInitialState world,
                        snapshotActions = []
                    },
            guiHistory = [],
            guiFuture = [],
            guiPaused = True,
            guiAccumulator = 0
        }

-- | Handles keyboard input for the GUI debugger.
handleGuiEvent :: Event -> GuiWorld -> IO GuiWorld
handleGuiEvent (EventKey (SpecialKey KeySpace) Down _ _) world =
    pure
        world
            {
                guiPaused = not (guiPaused world),
                guiAccumulator = 0
            }

handleGuiEvent (EventKey (SpecialKey KeyRight) Down _ _) world =
    stepGuiForward
        world
            {
                guiPaused = True,
                guiAccumulator = 0
            }

handleGuiEvent (EventKey (SpecialKey KeyLeft) Down _ _) world =
    pure
        ( stepGuiBackward world
        )

handleGuiEvent (EventKey (Char 'r') Down _ _) world =
    pure (restartGui world)

handleGuiEvent (EventKey (Char 'R') Down _ _) world =
    pure (restartGui world)

handleGuiEvent (EventKey (Char '+') Down _ _) world =
    pure
        world
            {
                guiSpeed = min 20 (guiSpeed world * 1.5)
            }

handleGuiEvent (EventKey (Char '=') Down _ _) world =
    pure
        world
            {
                guiSpeed = min 20 (guiSpeed world * 1.5)
            }

handleGuiEvent (EventKey (Char '-') Down _ _) world =
    pure
        world
            {
                guiSpeed = max 0.25 (guiSpeed world / 1.5)
            }

handleGuiEvent _ world =
    pure world


-- | Updates the automatic game simulation according to elapsed real time.
updateGuiWorld :: Float -> GuiWorld -> IO GuiWorld
updateGuiWorld deltaTime world
    | guiPaused world = pure world
    | guiGameFinished currentState =
        pure
            world
                {
                    guiPaused = True,
                    guiAccumulator = 0
                }
    | accumulatedTime >= tickInterval =
        stepGuiForward
            world
                {
                    guiAccumulator = accumulatedTime - tickInterval
                }
    | otherwise = 
        pure
            world
                {
                    guiAccumulator = accumulatedTime
                }
  where
    currentState = snapshotGameState (guiCurrent world)
    accumulatedTime = guiAccumulator world + deltaTime
    tickInterval = 1 / guiSpeed world

-- | Draws the currently selected snapshot of the GUI debugger.
drawGuiWorld :: GuiWorld -> IO Picture
drawGuiWorld world =
    pure
        (drawGameState (snapshotGameState (guiCurrent world)))


-- | Starts the interactive Gloss game debugger.
--
-- The supplied controllers should currently be AI controllers,
-- terminal controllers wait for console input.
runGui :: [(Int, Controller)] -> GameState -> IO ()
runGui controllers initialState =
    playIO
        guiDisplay
        black
        60
        (initialGuiWorld controllers initialState)
        drawGuiWorld
        handleGuiEvent
        updateGuiWorld

-- -----------------------------------------------------------------------------
-- Coordinate conversion
-- -----------------------------------------------------------------------------

-- | Converts a game-map position to Gloss coordinates.
mapPositionToScreen :: GameMap -> Position -> (Float, Float)
mapPositionToScreen gameMap' (x, y) =
    (screenX, screenY)
  where
    mapPixelWidth = fromIntegral (mapWidth gameMap') * cellSize
    mapPixelHeight = fromIntegral (mapHeight gameMap') * cellSize
    
    screenX = fromIntegral x * cellSize - mapPixelWidth/2 + cellSize/2
    screenY = mapPixelHeight/2 - fromIntegral y * cellSize - cellSize/2


-- | Draws one square map cell.
drawCell :: Color -> Picture
drawCell cellColor =
    color cellColor $
        rectangleSolid
            (cellSize - 2)
            (cellSize - 2)

-- | Returns the display color of a map tile.
tileColor :: Tile -> Color
tileColor Wall = greyN 0.25
tileColor Empty = greyN 0.92
tileColor Food = green
tileColor Poison = magenta

-- | Draws one map tile at its map position.
drawTile :: GameMap -> Position -> Picture
drawTile gameMap' position =
    case tileAt gameMap' position of
        Nothing -> Blank
        Just tile -> 
            let (screenX, screenY) = mapPositionToScreen gameMap' position
            in
                translate screenX screenY $
                    drawCell (tileColor tile)

-- | Draws all cells of the game map.
drawMap :: GameMap -> Picture
drawMap gameMap' =
    pictures
        [ drawTile gameMap' position | position <- allMapPositions gameMap']


-- | Draws one segment of a worm.
drawWormSegment :: GameMap -> Color -> Position -> Picture
drawWormSegment gameMap' segmentColor position =
    let (screenX, screenY) = mapPositionToScreen gameMap' position
    in
        translate screenX screenY $
            color segmentColor $
                circleSolid (cellSize * 0.35)

-- | Draws one living worm.
drawWorm :: GameMap -> Worm -> Picture
drawWorm gameMap' worm
    | not (wormAlive worm) = Blank
    | otherwise = 
        case wormBody worm of
            [] -> Blank
            headPosition : bodyPositions ->
                pictures (
                    drawWormSegment gameMap' orange headPosition 
                    : map (drawWormSegment gameMap' azure) bodyPositions)


-- | Draws the complete current game state.
drawGameState :: GameState -> Picture
drawGameState state =
    pictures
    [drawMap currentMap, pictures [drawWorm currentMap worm | worm <- gameWorms state]]
  where
    currentMap = gameMap state


-- | Opens a Gloss window displaying a static game state.
runStaticGui :: GameState -> IO ()
runStaticGui initialState =
    display
        guiDisplay black (drawGameState initialState)