{-|
Module      : Gui
Description : Watch Agents simulation, graphical board rendering, and debug HUD.

This module implements the interactive Watch Agents mode. It maintains a
history of simulation snapshots, obtains actions from configured controllers,
supports pausing and stepping through the simulation, and displays both general
game information and version-independent Q-learning diagnostics.

The module also contains graphical helpers shared with the application menus
and 'PlayGui', including board rendering, viewport indicators, panels, text,
and common HUD components.
-}
module Gui where

import Maps ( tileAt, allMapPositions, foodPositions )
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
        guiHudMode :: WatchHudMode,
        guiPaused :: Bool,
        guiSpeed :: Float,
        guiAccumulator :: Float
    }

-- | Right-side Watch HUD page.
data WatchHudMode
    = WatchOverviewMode
    | WatchDebugMode
    deriving (Show, Eq)

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
            guiHudMode = WatchOverviewMode,
            guiPaused = True,
            guiSpeed = 3,
            guiAccumulator = 0       
        }

-- -----------------------------------------------------------------------------
-- Simulation history
-- -----------------------------------------------------------------------------

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

-- | Returns the last action performed by the given worm.
snapshotActionFor :: Int -> GuiSnapshot -> Maybe Action
snapshotActionFor wormId' snapshot =
    lookup wormId' (snapshotActions snapshot)

-- | Returns the last reward obtained by the given worm.
snapshotRewardFor :: Int -> GuiSnapshot -> Maybe Double
snapshotRewardFor wormId' snapshot =
    lookup wormId' (snapshotRewards snapshot)

-- -----------------------------------------------------------------------------
-- Input handling
-- -----------------------------------------------------------------------------

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

handleGuiEvent (EventKey (Char 'd') Down _ _) world =
    pure world { guiHudMode = toggleWatchHudMode (guiHudMode world) }

handleGuiEvent (EventKey (Char 'D') Down _ _) world =
    pure world { guiHudMode = toggleWatchHudMode (guiHudMode world) }

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

-- | Switches between the compact Watch overview and detailed RL diagnostics.
toggleWatchHudMode :: WatchHudMode -> WatchHudMode
toggleWatchHudMode WatchOverviewMode = WatchDebugMode
toggleWatchHudMode WatchDebugMode = WatchOverviewMode

-- -----------------------------------------------------------------------------
-- Automatic updating
-- -----------------------------------------------------------------------------

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

-- -----------------------------------------------------------------------------
-- Agent helpers
-- -----------------------------------------------------------------------------

-- | Computes the configured reward for one GUI agent across a game transition.
rewardForAgent :: GuiAgent -> GameState -> GameState -> Maybe Double
rewardForAgent agent beforeState afterState = do
    rewardFunction <- guiAgentRewardFunction agent
    beforeWorm <- Core.controlledWorm (guiAgentWormId agent) beforeState
    afterWorm <- Core.controlledWorm (guiAgentWormId agent) afterState
    pure (rewardFunction beforeState beforeWorm afterState afterWorm)

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
-- Board rendering
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

-- | Draws the complete game state with agent-specific worm colors.
--
-- Small maps are shown completely. Large maps use a viewport centered on the
-- currently selected worm.
drawGameStateForGui :: GuiWorld -> Picture
drawGameStateForGui world
    | usesViewport currentMap =
        pictures
            [ drawMapViewport viewport currentMap,
              pictures
                [ drawColoredWormViewport viewport (wormColor (guiAgents world) worm) worm
                  | worm <- gameWorms currentState
                ],
              drawGuiViewportIndicators world viewport currentState focusPosition
            ]
    | otherwise =
        pictures
            [ drawMap currentMap,
              pictures
                [ drawColoredWorm currentMap (wormColor (guiAgents world) worm) worm
                  | worm <- gameWorms currentState
                ]
            ]
  where
    currentState = snapshotGameState (guiCurrent world)
    currentMap = gameMap currentState
    focusPosition =
        case selectedWormPosition world currentState of
            Just position -> position
            Nothing -> mapCenterPosition currentMap
    viewport = viewportAround currentMap focusPosition


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

-- -----------------------------------------------------------------------------
-- Shared UI drawing helpers
-- -----------------------------------------------------------------------------

-- | Draws one scaled line of text.
drawGuiText :: Float -> Float -> Float -> Color -> String -> Picture
drawGuiText x y textSize textColor contents =
    translate x y $
        scale textSize textSize $
            color textColor $
                text contents

-- | Joins short UI labels without depending on extra library helpers.
joinWith :: String -> [String] -> String
joinWith _ [] = ""
joinWith _ [value] = value
joinWith separator (value : values) =
    value ++ separator ++ joinWith separator values


-- | Shortens long labels so setup rows and HUD lines stay inside their panel.
shortenText :: Int -> String -> String
shortenText maxLength contents
    | length contents <= maxLength = contents
    | maxLength <= 3 = take maxLength contents
    | otherwise = take (maxLength - 3) contents ++ "..."


-- | Main panel fill used by menus and HUDs.
uiPanelFill :: Color
uiPanelFill = makeColorI 18 22 27 245


-- | Slightly lighter fill used for selected rows.
uiSelectedFill :: Color
uiSelectedFill = makeColorI 42 48 58 255


-- | Subtle border color for panels and rows.
uiBorderColor :: Color
uiBorderColor = makeColorI 88 96 108 255


-- | Warm accent used for focus and selected controls.
uiAccentColor :: Color
uiAccentColor = makeColorI 255 205 80 255


-- | Secondary text color.
uiMutedColor :: Color
uiMutedColor = greyN 0.72


-- | Draws a rectangle from its top-left corner.
drawBox :: Float -> Float -> Float -> Float -> Color -> Picture
drawBox left top width height boxColor =
    translate (left + width / 2) (top - height / 2) $
        color boxColor $
            rectangleSolid width height


-- | Draws a filled panel with a thin outline.
drawPanel :: Float -> Float -> Float -> Float -> Picture
drawPanel left top width height =
    pictures
        [
            drawBox left top width height uiPanelFill,
            translate (left + width / 2) (top - height / 2) $
                color uiBorderColor $
                    rectangleWire width height
        ]


-- | Draws a large screen or panel title with an optional subtitle.
drawTitle :: Float -> Float -> String -> String -> Picture
drawTitle left top title subtitle =
    pictures
        [
            drawGuiText left top 0.28 white title,
            drawGuiText left (top - 36) 0.10 uiMutedColor subtitle
        ]


-- | Draws a compact status badge.
drawStatusBadge :: Float -> Float -> String -> Color -> Picture
drawStatusBadge left top label badgeColor =
    pictures
        [
            drawBox left top 112 28 (makeColorI 32 36 42 255),
            translate (left + 56) (top - 14) $
                color badgeColor $
                    rectangleWire 112 28,
            drawGuiText (left + 15) (top - 20) 0.09 badgeColor label
        ]


-- | Draws one selectable menu command row.
drawMenuOptionRow :: Float -> Float -> Float -> Bool -> String -> Picture
drawMenuOptionRow left top width selected label =
    pictures
        [
            drawBox left top width 44 rowFill,
            translate (left + width / 2) (top - 22) $
                color borderColor $
                    rectangleWire width 44,
            drawBox left top 5 44 accentFill,
            drawGuiText (left + 22) (top - 29) 0.13 textColor label
        ]
  where
    rowFill =
        if selected then uiSelectedFill else makeColorI 24 28 34 255

    borderColor =
        if selected then uiAccentColor else uiBorderColor

    accentFill =
        if selected then uiAccentColor else rowFill

    textColor =
        if selected then white else greyN 0.86


-- | Draws one selectable configuration row with left/right value affordances.
drawSettingRow :: Float -> Float -> Float -> Bool -> String -> String -> Picture
drawSettingRow left top width selected label value =
    pictures
        [
            drawBox left top width 64 rowFill,
            translate (left + width / 2) (top - 32) $
                color borderColor $
                    rectangleWire width 64,
            drawBox left top 5 64 accentFill,
            drawGuiText (left + 22) (top - 23) 0.085 uiMutedColor label,
            drawGuiText (left + 22) (top - 50) 0.115 textColor ("< " ++ shortenText 44 value ++ " >")
        ]
  where
    rowFill =
        if selected then uiSelectedFill else makeColorI 24 28 34 255

    borderColor =
        if selected then uiAccentColor else uiBorderColor

    accentFill =
        if selected then uiAccentColor else rowFill

    textColor =
        if selected then white else greyN 0.88


-- | Draws one selectable action row such as Start or Back.
drawActionRow :: Float -> Float -> Float -> Bool -> String -> Picture
drawActionRow left top width selected label =
    drawMenuOptionRow left top width selected label


-- | Draws a small uppercase-style section header.
drawSectionHeader :: Float -> Float -> Color -> String -> Picture
drawSectionHeader left y headerColor label =
    drawGuiText left y 0.115 headerColor label


-- | Draws one label/value pair used in HUD panels.
drawStatLine :: Float -> Float -> String -> String -> Picture
drawStatLine left y label value =
    pictures
        [
            drawGuiText left y 0.09 uiMutedColor label,
            drawGuiText (left + 118) y 0.09 white value
        ]


-- | Draws a footer made from concise keyboard hints.
drawFooter :: Float -> Float -> [String] -> Picture
drawFooter left y hints =
    drawGuiText left y 0.085 uiMutedColor (joinWith "     " hints)


-- -----------------------------------------------------------------------------
-- Watch HUD
-- -----------------------------------------------------------------------------


-- | Draws the complete right-side Watch Agents HUD.
drawWatchHud :: GuiWorld -> Picture
drawWatchHud world =
    pictures
        [
            drawPanel 185 360 515 700,
            drawWatchHeader world,
            drawWatchModeTabs world,
            case guiHudMode world of
                WatchOverviewMode -> drawWatchOverview world
                WatchDebugMode -> drawWatchDebug world,
            drawFooter
                215
                (-320)
                [
                    "D view",
                    "SPACE pause",
                    "<-/-> step",
                    "R restart",
                    "+/- speed",
                    "ESC back"
                ]
        ]


-- | Draws the Watch title, status badge and simulation counters.
drawWatchHeader :: GuiWorld -> Picture
drawWatchHeader world =
    pictures
        [
            drawGuiText 215 318 0.18 white "Watch Agents",
            drawStatusBadge 560 332 statusText statusColor,
            drawStatLine 215 278 "Tick" (show (gameTick currentState)),
            drawStatLine 415 278 "Speed" (printf "%.2fx" (guiSpeed world))
        ]
  where
    currentState =
        snapshotGameState (guiCurrent world)

    statusText =
        if guiPaused world
            then "PAUSED"
            else "PLAYING"

    statusColor =
        if guiPaused world
            then uiAccentColor
            else green


-- | Draws the Overview / Debug segmented control.
drawWatchModeTabs :: GuiWorld -> Picture
drawWatchModeTabs world =
    pictures
        [
            drawTab 215 "OVERVIEW" (guiHudMode world == WatchOverviewMode),
            drawTab 335 "DEBUG" (guiHudMode world == WatchDebugMode)
        ]
  where
    drawTab left label selected =
        pictures
            [
                drawBox left 250 106 30 tabFill,
                translate (left + 53) 235 $
                    color tabBorder $
                        rectangleWire 106 30,
                drawGuiText (left + 15) 229 0.08 tabText label
            ]
      where
        tabFill =
            if selected then uiSelectedFill else makeColorI 24 28 34 255

        tabBorder =
            if selected then uiAccentColor else uiBorderColor

        tabText =
            if selected then white else uiMutedColor


-- | Draws the readable Watch overview page.
drawWatchOverview :: GuiWorld -> Picture
drawWatchOverview world =
    pictures
        [
            drawSectionHeader 215 190 white "Agents",
            drawWatchAgentRows world 160,
            drawSelectedWormOverview world
        ]


-- | Draws compact agent rows for the Watch overview page.
drawWatchAgentRows :: GuiWorld -> Float -> Picture
drawWatchAgentRows world startY =
    pictures (zipWith drawAgent [0 ..] (guiAgents world))
  where

    drawAgent index agent =
        let
            y =
                startY - fromIntegral index * 34

            selected =
                guiSelectedWormId world == guiAgentWormId agent

            rowColor =
                if selected then uiSelectedFill else makeColorI 24 28 34 255

            borderColor =
                if selected then uiAccentColor else uiBorderColor

            label =
                "Worm "
                    ++ show (guiAgentWormId agent)
                    ++ "  "
                    ++ shortenText 34 (guiAgentName agent)
        in
            pictures
                [
                    drawBox 215 (y + 14) 440 28 rowColor,
                    translate 435 y $
                        color borderColor $
                            rectangleWire 440 28,
                    translate 232 (y + 1) $
                        color (guiAgentColor agent) $
                            circleSolid 6,
                    drawGuiText 250 (y - 8) 0.085 white label
                ]


-- | Draws the selected worm's most important gameplay information.
drawSelectedWormOverview :: GuiWorld -> Picture
drawSelectedWormOverview world =
    case Core.controlledWorm selectedId currentState of
        Nothing ->
            drawGuiText 215 38 0.10 red "Selected worm not found"

        Just worm ->
            pictures
                [
                    drawSectionHeader 215 58 white ("Selected Worm " ++ show selectedId),
                    drawStatLine 215 26 "Status" (if wormAlive worm then "Alive" else "Dead"),
                    drawStatLine 215 2 "Length" (show (length (wormBody worm))),
                    drawStatLine 215 (-22) "Food" (show (foodEaten stats)),
                    drawStatLine 215 (-46) "Kills" (show (kills stats)),
                    drawStatLine 215 (-70) "Age" (show (age stats)),
                    drawStatLine 215 (-105) "Last action" (maybe "-" show (snapshotActionFor selectedId snapshot)),
                    drawStatLine 215 (-129) "Last reward" (maybe "-" (printf "%.2f") (snapshotRewardFor selectedId snapshot)),
                    drawGuiText 215 (-172) 0.085 uiMutedColor "Press 1-9 to focus a worm. D opens RL details."
                ]
          where
            stats =
                wormStats worm
  where
    selectedId =
        guiSelectedWormId world

    snapshot =
        guiCurrent world

    currentState =
        snapshotGameState snapshot


-- | Draws the detailed Watch diagnostic page.
drawWatchDebug :: GuiWorld -> Picture
drawWatchDebug world =
    pictures
        [
            drawSectionHeader 215 198 white ("Worm " ++ show (guiSelectedWormId world) ++ " - RL Debug"),
            drawGuiText 215 170 0.085 uiMutedColor "Overview is hidden here so the Q-learning state has room.",
            drawAgentDebug world
        ]



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
            [ drawGuiText panelLeftX 132 0.12 white "RL state"]
            ++ statePictures
            ++
            [
                drawGuiText panelLeftX qHeaderY 0.12 white "Q-values",
                drawGuiText panelLeftX (qHeaderY - 26) 0.09 white ("Left:     " ++ printf "%.3f" (qValueOf TurnLeft)),
                drawGuiText panelLeftX (qHeaderY - 48) 0.09 white ("Straight: " ++ printf "%.3f" (qValueOf GoStraight)),
                drawGuiText panelLeftX (qHeaderY - 70) 0.09 white ("Right:    " ++ printf "%.3f" (qValueOf TurnRight)),
                drawGuiText panelLeftX (qHeaderY - 96) 0.09 white ("Q best: " ++ shortenText 42 (show (Debug.debugBestActions debugInfo)))
            ]
        )
  where
    stateLines = Debug.debugStateLines debugInfo

    statePictures = zipWith drawStateLine [0 ..] stateLines

    drawStateLine :: Int -> (String, String) -> Picture
    drawStateLine index (label, value) =
        drawGuiText
            panelLeftX
            (104 - fromIntegral index * 20)
            0.082
            white
            (shortenText 48 (label ++ ": " ++ value))

    qHeaderY = 104 - fromIntegral (length stateLines) * 20 - 18

    qValueOf action =
        case lookup action (Debug.debugQValues debugInfo) of
            Just value -> value

            Nothing -> 0

-- -----------------------------------------------------------------------------
-- Top-level drawing
-- -----------------------------------------------------------------------------

-- | Draws the complete interactive debugger.
drawGuiWorld :: GuiWorld -> IO Picture
drawGuiWorld world =
    pure $ pictures [translate boardOffsetX 0 (drawGameStateForGui world), drawWatchHud world]
