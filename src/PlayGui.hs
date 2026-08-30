module PlayGui where

import Config (maxFoodCount)
import Controller (runController)
import Game (maintainFoodCount, stepGame)
import Gui
    ( boardOffsetX
    , drawBox
    , drawColoredWorm
    , drawColoredWormViewport
    , drawFooter
    , drawGuiText
    , drawMap
    , drawMapViewport
    , drawNearestFoodIndicator
    , drawOffscreenIndicator
    , drawPanel
    , drawSectionHeader
    , drawStatLine
    , drawStatusBadge
    , panelLeftX
    , shortenText
    , uiAccentColor
    , uiMutedColor
    )
import Viewport
import Types

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Text.Printf (printf)


-- -----------------------------------------------------------------------------
-- Play configuration
-- -----------------------------------------------------------------------------

-- | initial number of game ticks simulated per second.
initialPlaySpeed :: Float
initialPlaySpeed = 3

-- | Display color of the human-controlled worm.
humanColor :: Color
humanColor = makeColorI 50 140 255 255


-- | Display color of the AI-controlled opponent.
opponentColor :: Color
opponentColor = makeColorI 255 130 60 255

-- -----------------------------------------------------------------------------
-- Play state
-- -----------------------------------------------------------------------------

-- | Current result of a human-versus-agent game.
data PlayResult
    = PlayRunning
    | PlayerWon
    | PlayerLost
    | PlayDraw
    deriving (Show, Eq)


-- | Complete state of one human-versus-agent game.
data PlayWorld = PlayWorld
    {
        playGameState :: GameState,
        playInitialState :: GameState,
        playHumanWormId :: Int,
        playOpponentWormId :: Int,
        playOpponentName :: String,
        playOpponentController :: Controller,
        playPendingAction :: Action,
        playPaused :: Bool,
        playSpeed :: Float,
        playAccumulator :: Float,
        playResult :: PlayResult
    }

-- | Creates a new human-versus-agent game.
initialPlayWorld :: Int -> Int -> String -> Controller -> GameState -> PlayWorld
initialPlayWorld humanId opponentId opponentName opponentController initialState =
    PlayWorld
        {
            playGameState = initialState,
            playInitialState = initialState,
            playHumanWormId = humanId,
            playOpponentWormId = opponentId,
            playOpponentName = opponentName,
            playOpponentController = opponentController,
            playPendingAction = GoStraight,
            playPaused = True,
            playSpeed = initialPlaySpeed,
            playAccumulator = 0,
            playResult = resultForState humanId opponentId initialState
        }

-- -----------------------------------------------------------------------------
-- Game-state helpers
-- -----------------------------------------------------------------------------

-- | Finds a worm by its ID, including dead worms.
findWormById :: Int -> GameState -> Maybe Worm
findWormById targetId state =
    case filter ((== targetId) . wormId) (gameWorms state) of
        worm : _ -> Just worm
        [] -> Nothing


-- | Returns True if the given worm exists and is alive.
wormAliveById :: Int -> GameState -> Bool
wormAliveById targetId state =
    case findWormById targetId state of
        Just worm -> wormAlive worm
        Nothing -> False


-- | Determines the human-versus-agent result from a game state.
resultForState :: Int -> Int -> GameState -> PlayResult
resultForState humanId opponentId state =
    case
        (
            wormAliveById humanId state,
            wormAliveById opponentId state
        )
    of
        (True, True) -> PlayRunning
        (True, False) -> PlayerWon
        (False, True) -> PlayerLost
        (False, False) -> PlayDraw


-- | Returns a human-readable name of a relative movement action.
actionName :: Action -> String
actionName TurnLeft = "Turn left"
actionName GoStraight = "Straight"
actionName TurnRight = "Turn right"


-- | Returns a human-readable name of the current game result.
resultName :: PlayResult -> String
resultName PlayRunning = "Playing"
resultName PlayerWon = "You win!"
resultName PlayerLost = "You died"
resultName PlayDraw = "Draw"


-- -----------------------------------------------------------------------------
-- Game stepping
-- -----------------------------------------------------------------------------

-- | Advances the human-versus-agent game by one simultaneous game tick.
stepPlayWorld :: PlayWorld -> IO PlayWorld
stepPlayWorld world
    | playResult world /= PlayRunning =
        pure
            world
                {
                    playPaused = True,
                    playAccumulator = 0
                }

    | otherwise = do
        opponentAction <-
            runController
                currentState
                (
                    playOpponentWormId world,
                    playOpponentController world
                )

        let actions =
                [
                    (playHumanWormId world, playPendingAction world),
                    opponentAction
                ]

            steppedState =
                stepGame actions currentState

        nextState <-
            maintainFoodCount
                maxFoodCount
                steppedState

        let nextResult =
                resultForState
                    (playHumanWormId world)
                    (playOpponentWormId world)
                    nextState

        pure
            world
                {
                    playGameState = nextState,
                    playPendingAction = GoStraight,
                    playPaused =
                        if nextResult == PlayRunning
                            then playPaused world
                            else True,
                    playAccumulator = 0,
                    playResult = nextResult
                }
  where
    currentState =
        playGameState world


-- | Restarts the current human-versus-agent game.
restartPlay :: PlayWorld -> PlayWorld
restartPlay world =
    world
        {
            playGameState = playInitialState world,
            playPendingAction = GoStraight,
            playPaused = True,
            playAccumulator = 0,
            playResult =
                resultForState
                    (playHumanWormId world)
                    (playOpponentWormId world)
                    (playInitialState world)
        }


-- -----------------------------------------------------------------------------
-- Human input
-- -----------------------------------------------------------------------------

-- | Stores an action to be consumed during the next game tick.
queueHumanAction :: Action -> PlayWorld -> PlayWorld
queueHumanAction action world
    | playResult world /= PlayRunning = world
    | otherwise =
        world
            {
                playPendingAction = action
            }


-- | Converts a requested global direction into a relative worm action.
--
-- Reversing directly into the opposite direction is not allowed.
actionForDirection :: Direction -> Direction -> Maybe Action
actionForDirection North North = Just GoStraight
actionForDirection North East = Just TurnRight
actionForDirection North West = Just TurnLeft
actionForDirection North South = Nothing

actionForDirection East East = Just GoStraight
actionForDirection East South = Just TurnRight
actionForDirection East North = Just TurnLeft
actionForDirection East West = Nothing

actionForDirection South South = Just GoStraight
actionForDirection South West = Just TurnRight
actionForDirection South East = Just TurnLeft
actionForDirection South North = Nothing

actionForDirection West West = Just GoStraight
actionForDirection West North = Just TurnRight
actionForDirection West South = Just TurnLeft
actionForDirection West East = Nothing

-- | Queues a global direction for the human worm if the requested turn is
-- possible from its current direction.
queueHumanDirection :: Direction -> PlayWorld -> PlayWorld
queueHumanDirection requestedDirection world =
    case findWormById (playHumanWormId world) (playGameState world) of
        Nothing ->
            world

        Just worm ->
            case actionForDirection (wormDirection worm) requestedDirection of
                Nothing ->
                    world

                Just action ->
                    queueHumanAction action world

-- | Handles keyboard input during a human-versus-agent game.
handlePlayEvent :: Event -> PlayWorld -> IO PlayWorld
handlePlayEvent (EventKey (SpecialKey KeyUp) Down _ _) world =
    pure (queueHumanDirection North world)

handlePlayEvent (EventKey (SpecialKey KeyRight) Down _ _) world =
    pure (queueHumanDirection East world)

handlePlayEvent (EventKey (SpecialKey KeyDown) Down _ _) world =
    pure (queueHumanDirection South world)

handlePlayEvent (EventKey (SpecialKey KeyLeft) Down _ _) world =
    pure (queueHumanDirection West world)

handlePlayEvent (EventKey (SpecialKey KeySpace) Down _ _) world
    | playResult world /= PlayRunning =
        pure world

    | otherwise =
        pure
            world
                {
                    playPaused = not (playPaused world),
                    playAccumulator = 0
                }

handlePlayEvent (EventKey (Char 'r') Down _ _) world =
    pure (restartPlay world)

handlePlayEvent (EventKey (Char 'R') Down _ _) world =
    pure (restartPlay world)

handlePlayEvent (EventKey (Char '+') Down _ _) world =
    pure
        world
            {
                playSpeed = min 20 (playSpeed world * 1.5)
            }

handlePlayEvent (EventKey (Char '=') Down _ _) world =
    pure
        world
            {
                playSpeed = min 20 (playSpeed world * 1.5)
            }

handlePlayEvent (EventKey (Char '-') Down _ _) world =
    pure
        world
            {
                playSpeed = max 0.25 (playSpeed world / 1.5)
            }

handlePlayEvent _ world =
    pure world



-- -----------------------------------------------------------------------------
-- Automatic updating
-- -----------------------------------------------------------------------------

-- | Updates human-versus-agent gameplay according to elapsed real time.
updatePlayWorld :: Float -> PlayWorld -> IO PlayWorld
updatePlayWorld deltaTime world
    | playPaused world = pure world
    | playResult world /= PlayRunning = pure world
    | accumulatedTime >= tickInterval =
        stepPlayWorld
            world
                {
                    playAccumulator = accumulatedTime - tickInterval
                }

    | otherwise =
        pure
            world
                {
                    playAccumulator = accumulatedTime
                }
  where
    accumulatedTime =
        playAccumulator world + deltaTime

    tickInterval =
        1 / playSpeed world


-- | Returns the map position the play-mode camera should follow.
humanFocusPosition :: PlayWorld -> Position
humanFocusPosition world =
    case
        findWormById
            (playHumanWormId world)
            currentState
    of
        Just worm ->
            case wormBody worm of
                headPosition : _ -> headPosition
                [] -> fallbackPosition

        Nothing ->
            fallbackPosition
  where
    currentState =
        playGameState world

    currentMap =
        gameMap currentState

    fallbackPosition =
        (
            mapWidth currentMap `div` 2,
            mapHeight currentMap `div` 2
        )

-- -----------------------------------------------------------------------------
-- Drawing
-- -----------------------------------------------------------------------------

-- | Returns the display color assigned to a worm in play mode.
playWormColor :: PlayWorld -> Worm -> Color
playWormColor world worm
    | wormId worm == playHumanWormId world = humanColor
    | wormId worm == playOpponentWormId world = opponentColor
    | otherwise = white


-- | Draws the current game board for human-versus-agent gameplay.
--
-- Small maps are shown completely. Large maps use a viewport centered on the
-- human-controlled worm.
drawPlayGameState :: PlayWorld -> Picture
drawPlayGameState world
    | usesViewport currentMap =
        pictures
            [
                drawMapViewport viewport currentMap,

                pictures
                    [
                        drawColoredWormViewport
                            viewport
                            (playWormColor world worm)
                            worm
                        | worm <- gameWorms currentState
                    ],

                drawPlayViewportIndicators
                    world
                    viewport
                    currentState
                    (humanFocusPosition world)
            ]

    | otherwise =
        pictures
            [
                drawMap currentMap,
                pictures
                    [
                        drawColoredWorm
                            currentMap
                            (playWormColor world worm)
                            worm
                        | worm <- gameWorms currentState
                    ]
            ]
  where
    currentState =
        playGameState world

    currentMap =
        gameMap currentState

    viewport =
        viewportAround
            currentMap
            (humanFocusPosition world)


-- | Draws one worm's basic gameplay statistics.
drawPlayWormStats :: Float -> String -> Color -> Maybe Worm -> Picture
drawPlayWormStats y label labelColor maybeWorm =
    case maybeWorm of
        Nothing ->
            drawGuiText
                panelLeftX
                y
                0.11
                labelColor
                (label ++ ": unavailable")

        Just worm ->
            pictures
                [
                    drawSectionHeader panelLeftX y labelColor label,
                    drawStatLine panelLeftX (y - 32) "Length" (show (length (wormBody worm))),
                    drawStatLine panelLeftX (y - 56) "Food" (show (foodEaten (wormStats worm))),
                    drawStatLine panelLeftX (y - 80) "Kills" (show (kills (wormStats worm))),
                    drawStatLine panelLeftX (y - 104) "Age" (show (age (wormStats worm)))
                ]


-- | Draws gameplay status and controls.
drawPlayStatus :: PlayWorld -> Picture
drawPlayStatus world =
    pictures
        [
            drawPanel 185 360 515 700,
            drawGuiText panelLeftX 318 0.18 white "Play vs Agent",
            drawStatusBadge 560 332 statusText resultColor,
            drawStatLine panelLeftX 274 "Tick" (show (gameTick currentState)),
            drawStatLine (panelLeftX + 205) 274 "Speed" (printf "%.2fx" (playSpeed world)),

            drawPlayWormStats
                205
                "You"
                humanColor
                (findWormById (playHumanWormId world) currentState),

            drawPlayWormStats
                45
                ("Opponent - " ++ shortenText 30 (playOpponentName world))
                opponentColor
                (findWormById (playOpponentWormId world) currentState),

            drawGuiText panelLeftX (-145) 0.085 uiMutedColor "Arrow controls use global directions. Opposite direction is ignored.",
            drawFooter panelLeftX (-320) ["ARROWS move", "SPACE pause", "R restart", "+/- speed", "ESC back"]
        ]
  where
    currentState =
        playGameState world

    statusText =
        case playResult world of
            PlayRunning ->
                if playPaused world then "PAUSED" else "PLAYING"
            PlayerWon -> "YOU WIN"
            PlayerLost -> "GAME OVER"
            PlayDraw -> "DRAW"

    resultColor =
        case playResult world of
            PlayRunning ->
                if playPaused world then uiAccentColor else green
            PlayerWon -> green
            PlayerLost -> red
            PlayDraw -> yellow


-- | Draws the complete human-versus-agent game.
drawPlayWorld :: PlayWorld -> IO Picture
drawPlayWorld world =
    pure
        ( pictures
            [
                translate boardOffsetX 0 $
                    pictures
                        [
                            drawPlayGameState world,
                            drawPlayGameOverOverlay world
                        ],

                drawPlayStatus world
            ]
        )


-- | Draws a simple board-centered overlay when play mode has ended.
drawPlayGameOverOverlay :: PlayWorld -> Picture
drawPlayGameOverOverlay world
    | playResult world == PlayRunning =
        Blank

    | otherwise =
        pictures
            [
                drawBox (-225) 112 450 224 (makeColorI 5 7 10 220),
                translate 0 0 $
                    color uiBorder $
                        rectangleWire 450 224,
                drawGuiText titleX 58 0.23 titleColor title,
                drawGuiText (-96) 8 0.10 white ("Food eaten: " ++ maybe "-" (show . foodEaten . wormStats) humanWorm),
                drawGuiText (-96) (-18) 0.10 white ("Age: " ++ maybe "-" (show . age . wormStats) humanWorm),
                drawGuiText (-130) (-70) 0.095 uiMutedColor "R restart     ESC back"
            ]
  where
    currentState =
        playGameState world

    humanWorm =
        findWormById
            (playHumanWormId world)
            currentState

    title =
        case playResult world of
            PlayerWon -> "YOU WIN"
            PlayerLost -> "GAME OVER"
            PlayDraw -> "DRAW"
            PlayRunning -> ""

    titleColor =
        case playResult world of
            PlayerWon -> green
            PlayerLost -> red
            PlayDraw -> yellow
            PlayRunning -> white

    titleX =
        case playResult world of
            PlayerWon -> -96
            PlayerLost -> -128
            PlayDraw -> -55
            PlayRunning -> 0

    uiBorder =
        makeColorI 130 136 148 255


-- | Draws off-screen navigation indicators relative to the human-controlled
-- worm.
drawPlayViewportIndicators :: PlayWorld -> Viewport -> GameState -> Position -> Picture
drawPlayViewportIndicators world viewport state focusPosition =
    pictures
        [
            opponentIndicator,
            drawNearestFoodIndicator
                viewport
                (gameMap state)
                focusPosition
        ]
  where
    opponentIndicator =
        case
            findWormById
                (playOpponentWormId world)
                state
        of
            Just opponent ->
                case wormBody opponent of
                    headPosition : _
                        | wormAlive opponent ->
                            drawOffscreenIndicator
                                opponentIndicatorInset
                                viewport
                                focusPosition
                                headPosition
                                opponentColor
                                "AI"

                    _ ->
                        Blank

            Nothing ->
                Blank
