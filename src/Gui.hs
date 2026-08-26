module Gui where

import Maps ( tileAt, allMapPositions, foodPositions )
import Movement ()
import Types
import Config ( maxFoodCount )
import Controller ( collectActions )
import Game ( maintainFoodCount, stepGame )
import Viewport

import qualified QLearning.Core as Core
import qualified QLearning.Debug as Debug

import Text.Printf (printf)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

-- -----------------------------------------------------------------------------
-- GUI configuration
-- -----------------------------------------------------------------------------

-- | Maximum size of one map cell in pixels.
maxCellSize :: Float
maxCellSize = 40


-- | Maximum width available for the game board.
boardMaxWidth :: Float
boardMaxWidth = 820


-- | Maximum height available for the game board.
boardMaxHeight :: Float
boardMaxHeight = 680


-- | Computes a map cell size that fits the complete map into the game area.
cellSizeForMap :: GameMap -> Float
cellSizeForMap gameMap' =
    min
        maxCellSize
        ( min
            (boardMaxWidth / fromIntegral (mapWidth gameMap'))
            (boardMaxHeight / fromIntegral (mapHeight gameMap'))
        )

-- | Width of the initial GUI window.
windowWidth :: Int
windowWidth = 1400


-- | Height of the initial GUI window.
windowHeight :: Int
windowHeight = 760

-- | Horizontal offset of the game board inside the debugger window.
boardOffsetX :: Float
boardOffsetX = -250


-- | Left edge of the debug panel.
panelLeftX :: Float
panelLeftX = 210


-- | Top position of the debug panel.
panelTopY :: Float
panelTopY = 330

-- | Vertical position of the simulator status.
statusY :: Float
statusY = 330


-- | Vertical position of the agent legend.
legendTopY :: Float
legendTopY = 275


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
        snapshotActions :: [(Int, Action)],
        snapshotRewards :: [(Int, Double)]

    }

-- | Complete state of the interactive GUI debugger.
data GuiWorld = GuiWorld
    {
        guiCurrent :: GuiSnapshot,
        guiHistory :: [GuiSnapshot],
        guiFuture :: [GuiSnapshot],
        guiInitialState :: GameState,
        guiAgents :: [GuiAgent],
        guiSelectedWormId :: Int,
        guiPaused :: Bool,
        guiSpeed :: Float,
        guiAccumulator :: Float
    }

-- | Description of one agent displayed by the GUI debugger.
data GuiAgent = GuiAgent
    {
        guiAgentWormId :: Int,
        guiAgentName :: String,
        guiAgentController :: Controller,
        guiAgentColor :: Color,
        guiAgentRewardFunction :: Maybe Core.RewardFunction,
        guiAgentDebugProvider :: Maybe Debug.AgentDebugProvider
    }

-- | Creates the initial GUI debugger state.
initialGuiWorld :: [GuiAgent] -> GameState -> GuiWorld
initialGuiWorld agents initialState =
    GuiWorld 
        {
            guiCurrent =
                GuiSnapshot 
                    {
                        snapshotGameState = initialState,
                        snapshotActions = [],
                        snapshotRewards = []
                    },
            guiHistory = [],
            guiFuture = [],
            guiInitialState = initialState,
            guiAgents = agents,
            guiSelectedWormId =
                case agents of
                    agent : _ -> guiAgentWormId agent
                    [] -> 1,
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
        actions <- collectActions (guiControllers (guiAgents world)) currentState
        let steppedState = stepGame actions currentState
        nextState <- maintainFoodCount maxFoodCount steppedState
        let rewards =
                [
                    (guiAgentWormId agent, reward) | agent <- guiAgents world,
                    Just reward <- [rewardForAgent agent currentState nextState]
                ]
        let nextSnapshot = GuiSnapshot
                {
                    snapshotGameState = nextState,
                    snapshotActions = actions,
                    snapshotRewards = rewards
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
                        snapshotActions = [],
                        snapshotRewards = []
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

handleGuiEvent (EventKey (Char key) Down _ _) world
    | key >= '1' && key <= '9' =
        let selectedId = fromEnum key - fromEnum '0'

            exists =
                any
                    ((== selectedId) . guiAgentWormId)
                    (guiAgents world)
        in
            pure
                ( if exists
                    then world {guiSelectedWormId = selectedId}
                    else world
                )

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

-- | Draws the complete interactive debugger.
drawGuiWorld :: GuiWorld -> IO Picture
drawGuiWorld world =
    pure
        ( pictures
            [
                translate boardOffsetX 0 $
                    drawGameStateForGui world,

                drawGuiStatus world,
                drawAgentLegend world,
                drawSelectedWormStats world,
                drawAgentDebug world
            ]
        )


-- | Starts the interactive Gloss game debugger.
runGui
    :: [GuiAgent]
    -> GameState
    -> IO ()
runGui agents initialState =
    playIO
        guiDisplay
        black
        60
        (initialGuiWorld agents initialState)
        drawGuiWorld
        handleGuiEvent
        updateGuiWorld

-- | Computes the configured reward for one GUI agent across a game transition.
rewardForAgent :: GuiAgent -> GameState -> GameState -> Maybe Double
rewardForAgent agent beforeState afterState = do
    rewardFunction <- guiAgentRewardFunction agent
    beforeWorm <- Core.controlledWorm (guiAgentWormId agent) beforeState
    afterWorm <- Core.controlledWorm (guiAgentWormId agent) afterState
    pure ( rewardFunction beforeState beforeWorm afterState afterWorm )


-- -----------------------------------------------------------------------------
-- Agent helpers
-- -----------------------------------------------------------------------------

-- | Converts GUI agent descriptions to the controller list used by the game.
guiControllers :: [GuiAgent] -> [(Int, Controller)]
guiControllers agents =
    [(guiAgentWormId agent, guiAgentController agent) | agent <- agents]

-- | Finds GUI metadata for the given worm.
findGuiAgent :: Int -> [GuiAgent] -> Maybe GuiAgent
findGuiAgent targetId agents =
    case filter ((== targetId) . guiAgentWormId) agents of
        agent : _ -> Just agent
        [] -> Nothing

-- | Returns the display color assigned to a worm.
wormColor :: [GuiAgent] -> Worm -> Color
wormColor agents worm =
    case findGuiAgent (wormId worm) agents of
        Just agent -> guiAgentColor agent
        Nothing -> white



-- -----------------------------------------------------------------------------
-- Coordinate conversion
-- -----------------------------------------------------------------------------

-- | Converts a game-map position to Gloss coordinates.
mapPositionToScreen :: GameMap -> Position -> (Float, Float)
mapPositionToScreen gameMap' (x, y) =
    (screenX, screenY)
  where
    cellSize = cellSizeForMap gameMap'

    mapPixelWidth = fromIntegral (mapWidth gameMap') * cellSize
    mapPixelHeight = fromIntegral (mapHeight gameMap') * cellSize
    
    screenX = fromIntegral x * cellSize - mapPixelWidth / 2 + cellSize / 2
    screenY = mapPixelHeight / 2 - fromIntegral y * cellSize - cellSize / 2

-- | Draws one square map cell.
drawCell :: Float -> Color -> Picture
drawCell cellSize cellColor =
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
                    drawCell (cellSizeForMap gameMap') (tileColor tile)

-- | Draws all cells of the game map.
drawMap :: GameMap -> Picture
drawMap gameMap' =
    pictures
        [ drawTile gameMap' position | position <- allMapPositions gameMap']

-- | Draws one map tile using viewport-relative coordinates.
drawTileViewport :: Viewport -> GameMap -> Position -> Picture
drawTileViewport viewport gameMap' position =
    case tileAt gameMap' position of
        Nothing ->
            Blank

        Just tile ->
            let
                (screenX, screenY) =
                    viewportPositionToScreen viewport position

            in
                translate screenX screenY $
                    drawCell
                        (viewportCellSize viewport)
                        (tileColor tile)


-- | Draws only the currently visible cells of a game map.
drawMapViewport :: Viewport -> GameMap -> Picture
drawMapViewport viewport gameMap' =
    pictures
        [
            drawTileViewport viewport gameMap' position
            | position <- viewportPositions viewport
        ]

-- | Draws one segment of a worm.
drawWormSegment :: GameMap -> Color -> Position -> Picture
drawWormSegment gameMap' segmentColor position =
    let
        (screenX, screenY) = mapPositionToScreen gameMap' position
        cellSize = cellSizeForMap gameMap'
    in
        translate screenX screenY $
            color segmentColor $
                circleSolid (cellSize * 0.35)

-- | Draws one living worm using its assigned GUI color.
drawColoredWorm :: GameMap -> Color -> Worm -> Picture
drawColoredWorm gameMap' wormColor' worm
    | not (wormAlive worm) = Blank
    | otherwise =
        case wormBody worm of
            [] -> Blank

            headPosition : bodyPositions ->
                let cellSize = cellSizeForMap gameMap'
                in
                    pictures
                        ( drawColoredWormSegment gameMap' wormColor' (cellSize * 0.38) headPosition
                            : map
                                (drawColoredWormSegment gameMap' wormColor' (cellSize * 0.29))
                                bodyPositions
                        )

-- | Draws one worm segment.
drawColoredWormSegment :: GameMap -> Color -> Float -> Position -> Picture
drawColoredWormSegment gameMap' segmentColor radius position =
    let (screenX, screenY) =
            mapPositionToScreen gameMap' position
    in
        translate screenX screenY $
            color segmentColor $
                circleSolid radius

-- | Draws one worm segment using viewport-relative coordinates.
drawColoredWormSegmentViewport :: Viewport -> Color -> Float -> Position -> Picture
drawColoredWormSegmentViewport viewport segmentColor radius position =
    let
        (screenX, screenY) =
            viewportPositionToScreen viewport position

    in
        translate screenX screenY $
            color segmentColor $
                circleSolid radius


-- | Draws the visible part of one living worm inside the viewport.
drawColoredWormViewport :: Viewport -> Color -> Worm -> Picture
drawColoredWormViewport viewport wormColor' worm
    | not (wormAlive worm) =
        Blank

    | otherwise =
        case wormBody worm of
            [] ->
                Blank

            headPosition : bodyPositions ->
                let
                    cellSize =
                        viewportCellSize viewport

                    visibleBody =
                        filter
                            (positionInViewport viewport)
                            bodyPositions

                    headPicture =
                        if positionInViewport viewport headPosition
                            then
                                [
                                    drawColoredWormSegmentViewport
                                        viewport
                                        wormColor'
                                        (cellSize * 0.38)
                                        headPosition
                                ]
                            else
                                []

                in
                    pictures
                        ( headPicture
                            ++ map
                                ( drawColoredWormSegmentViewport
                                    viewport
                                    wormColor'
                                    (cellSize * 0.29)
                                )
                                visibleBody
                        )


-- -----------------------------------------------------------------------------
-- Viewport indicators
-- -----------------------------------------------------------------------------

-- | Draws an arrow at the viewport edge pointing towards an off-screen map
-- position.
drawOffscreenIndicator :: Float -> Viewport -> Position -> Position -> Color -> String -> Picture
drawOffscreenIndicator inset viewport focusPosition targetPosition indicatorColor label =
    case
        offscreenIndicatorPlacement
            inset
            viewport
            focusPosition
            targetPosition
    of
        Nothing ->
            Blank

        Just (indicatorX, indicatorY, angle) ->
            pictures
                [
                    translate indicatorX indicatorY $
                        rotate (-angle) $
                            color indicatorColor $
                                polygon
                                    [
                                        (13, 0),
                                        (-9, 8),
                                        (-9, -8)
                                    ],

                    drawGuiText
                        labelX
                        labelY
                        0.085
                        indicatorColor
                        label
                ]
          where
            angleRadians =
                angle * pi / 180

            labelX =
                indicatorX
                    - cos angleRadians * 28
                    - 5

            labelY =
                indicatorY
                    - sin angleRadians * 28
                    - 5


-- | Draws an indicator pointing towards the geometrically nearest food when
-- that food lies outside the viewport.
drawNearestFoodIndicator :: Viewport -> GameMap -> Position -> Picture
drawNearestFoodIndicator viewport gameMap' focusPosition =
    case nearestPosition focusPosition (foodPositions gameMap') of
        Nothing ->
            Blank

        Just nearestFood ->
            drawOffscreenIndicator
                foodIndicatorInset
                viewport
                focusPosition
                nearestFood
                green
                "F"


-- | Draws indicators for living worms outside the viewport together with the
-- nearest off-screen food relative to the selected worm.
drawGuiViewportIndicators :: GuiWorld -> Viewport -> GameState -> Position -> Picture
drawGuiViewportIndicators world viewport state focusPosition =
    pictures
        (
            drawNearestFoodIndicator
                viewport
                (gameMap state)
                focusPosition

            : [
                drawOffscreenIndicator
                    opponentIndicatorInset
                    viewport
                    focusPosition
                    headPosition
                    (wormColor (guiAgents world) worm)
                    (show (wormId worm))
                | worm <- gameWorms state,
                  wormAlive worm,
                  wormId worm /= guiSelectedWormId world,
                  headPosition : _ <- [wormBody worm]
              ]
        )


-- | Returns the head position of the selected GUI worm when available.
selectedWormPosition :: GuiWorld -> GameState -> Maybe Position
selectedWormPosition world state =
    case
        filter
            ((== guiSelectedWormId world) . wormId)
            (gameWorms state)
    of
        worm : _ ->
            case wormBody worm of
                headPosition : _ -> Just headPosition
                [] -> Nothing

        [] ->
            Nothing


-- | Returns the center position of a game map.
mapCenterPosition :: GameMap -> Position
mapCenterPosition gameMap' =
    (
        mapWidth gameMap' `div` 2,
        mapHeight gameMap' `div` 2
    )

-- | Draws the complete game state with agent-specific worm colors.
--
-- Small maps are shown completely. Large maps use a viewport centered on the
-- currently selected worm.
drawGameStateForGui :: GuiWorld -> Picture
drawGameStateForGui world
    | usesViewport currentMap =
        pictures
            [
                drawMapViewport viewport currentMap,

                pictures
                    [
                        drawColoredWormViewport
                            viewport
                            (wormColor (guiAgents world) worm)
                            worm
                        | worm <- gameWorms currentState
                    ],

                drawGuiViewportIndicators
                    world
                    viewport
                    currentState
                    focusPosition
            ]

    | otherwise =
        pictures
            [
                drawMap currentMap,
                pictures
                    [
                        drawColoredWorm
                            currentMap
                            (wormColor (guiAgents world) worm)
                            worm
                        | worm <- gameWorms currentState
                    ]
            ]
  where
    currentState =
        snapshotGameState (guiCurrent world)

    currentMap =
        gameMap currentState

    focusPosition =
        case selectedWormPosition world currentState of
            Just position -> position
            Nothing -> mapCenterPosition currentMap

    viewport =
        viewportAround currentMap focusPosition


-- | Draws one scaled line of text.
drawGuiText :: Float -> Float -> Float -> Color -> String -> Picture
drawGuiText x y textSize textColor contents =
    translate x y $
        scale textSize textSize $
            color textColor $
                text contents

-- | Draws the agent color legend.
drawAgentLegend :: GuiWorld -> Picture
drawAgentLegend world = pictures ( header : zipWith drawAgent [0 ..] (guiAgents world) )
  where
    header = drawGuiText panelLeftX legendTopY 0.16 white "Agents"

    drawAgent :: Int -> GuiAgent -> Picture
    drawAgent index agent =
        let y = legendTopY - 35 - fromIntegral index * 28

            selected = guiSelectedWormId world == guiAgentWormId agent

            label =
                (if selected then "> " else "  ")
                    ++ "Worm "
                    ++ show (guiAgentWormId agent)
                    ++ " - "
                    ++ guiAgentName agent
        in
            pictures
                [
                    translate (panelLeftX + 8) (y + 7) $
                        color (guiAgentColor agent) $
                            circleSolid 7,

                    drawGuiText (panelLeftX + 25) y 0.11 white label
                ]

-- | Returns the last action performed by the given worm.
snapshotActionFor :: Int -> GuiSnapshot -> Maybe Action
snapshotActionFor wormId' snapshot =
    lookup wormId' (snapshotActions snapshot)


-- | Returns the last reward obtained by the given worm.
snapshotRewardFor :: Int -> GuiSnapshot -> Maybe Double
snapshotRewardFor wormId' snapshot =
    lookup wormId' (snapshotRewards snapshot)


-- | Draws basic statistics for the currently selected worm.
drawSelectedWormStats :: GuiWorld -> Picture
drawSelectedWormStats world =
    case Core.controlledWorm selectedId currentState of
        Nothing ->
            drawGuiText panelLeftX 140 0.12 red "Selected worm not found"

        Just worm ->
            pictures
                [
                    drawGuiText panelLeftX 160 0.15 white title,
                    drawGuiText panelLeftX 130 0.10 white statusLine,
                    drawGuiText panelLeftX 108 0.10 white lengthLine,
                    drawGuiText panelLeftX 86 0.10 white ageLine,
                    drawGuiText panelLeftX 64 0.10 white foodLine,
                    drawGuiText panelLeftX 42 0.10 white killsLine,
                    drawGuiText panelLeftX 20 0.10 white actionLine,
                    drawGuiText panelLeftX (-2) 0.10 white rewardLine
                ]
          where
            stats = wormStats worm

            title = "Selected: Worm " ++ show selectedId

            statusLine = "Status: " ++ if wormAlive worm then "alive" else "dead"

            lengthLine = "Length: " ++ show (length (wormBody worm))

            ageLine = "Age: " ++ show (age stats)

            foodLine = "Food: " ++ show (foodEaten stats)

            killsLine = "Kills: " ++ show (kills stats)

            actionLine =
                "Last action: "
                    ++ maybe
                        "-"
                        show
                        (snapshotActionFor selectedId snapshot)

            rewardLine =
                "Last reward: "
                    ++ maybe
                        "-"
                        (printf "%.2f")
                        (snapshotRewardFor selectedId snapshot)

  where
    selectedId =
        guiSelectedWormId world

    snapshot =
        guiCurrent world

    currentState =
        snapshotGameState snapshot


-- | Draws version-independent debug information for the selected agent.
drawAgentDebug :: GuiWorld -> Picture
drawAgentDebug world =
    case
        (
            findGuiAgent selectedId (guiAgents world),
            Core.controlledWorm selectedId currentState
        )
    of
        (Just agent, Just worm) ->
            case guiAgentDebugProvider agent of
                Nothing ->
                    drawGuiText
                        panelLeftX
                        (-55)
                        0.11
                        (greyN 0.7)
                        "No Q-learning debug data."

                Just debugProvider
                    | not (wormAlive worm) ->
                        drawGuiText
                            panelLeftX
                            (-55)
                            0.11
                            red
                            "Worm is dead."

                    | otherwise ->
                        drawDebugInfo
                            (debugProvider currentState worm)

        _ ->
            Blank
  where
    selectedId =
        guiSelectedWormId world

    currentState =
        snapshotGameState (guiCurrent world)


-- | Draws generic state information and Q-values.
drawDebugInfo :: Debug.AgentDebugInfo -> Picture
drawDebugInfo debugInfo =
    pictures
        (
            [ drawGuiText panelLeftX (-45) 0.14 white "RL state"]
            ++ statePictures
            ++
            [
                drawGuiText panelLeftX qHeaderY 0.14 white "Q-values",
                drawGuiText panelLeftX (qHeaderY - 27) 0.10 white ("Left:     " ++ printf "%.3f" (qValueOf TurnLeft)),
                drawGuiText panelLeftX (qHeaderY - 49) 0.10 white ("Straight: " ++ printf "%.3f" (qValueOf GoStraight)),
                drawGuiText panelLeftX (qHeaderY - 71) 0.10 white ("Right:    " ++ printf "%.3f" (qValueOf TurnRight)),
                drawGuiText panelLeftX (qHeaderY - 97) 0.10 white ("Q best: " ++ show (Debug.debugBestActions debugInfo))            ]
        )
  where
    stateLines = Debug.debugStateLines debugInfo

    statePictures = zipWith drawStateLine [0 ..] stateLines

    drawStateLine :: Int -> (String, String) -> Picture
    drawStateLine index (label, value) = drawGuiText panelLeftX (-72 - fromIntegral index * 22) 0.09 white (label ++ ": " ++ value)

    qHeaderY = -72 - fromIntegral (length stateLines) * 22 - 13

    qValueOf action =
        case lookup action (Debug.debugQValues debugInfo) of
            Just value -> value

            Nothing -> 0

-- | Draws current simulator status and controls.
drawGuiStatus :: GuiWorld -> Picture
drawGuiStatus world =
    pictures
        [
            drawGuiText panelLeftX 300 0.11 white ("Tick: " ++ show (gameTick currentState)),
            drawGuiText (panelLeftX + 120) statusY 0.11 statusColor statusText,
            drawGuiText (panelLeftX + 235) 300 0.11 white ("Speed: " ++ printf "%.2fx" (guiSpeed world)),
            drawGuiText panelLeftX (-335) 0.085 (greyN 0.7) "SPACE play/pause | arrows step | R restart | +/- speed | 1-9 select/focus"
        ]
  where
    currentState = snapshotGameState (guiCurrent world)

    statusText =
        if guiPaused world
            then "PAUSED"
            else "PLAYING"

    statusColor =
        if guiPaused world
            then yellow
            else green