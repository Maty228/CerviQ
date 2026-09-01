{-|
Module      : PlayGui
Description : Human-versus-multiple-agents gameplay and graphical presentation.

This module implements the playable CerviQ mode. One worm is controlled by the
human while one or more opposing worms use the generic controller interface.
All human and AI actions are submitted together to the normal simultaneous game
engine on every tick.

Interactive sessions maintain a scenario-specific number of randomly positioned
food items. The food target is fixed for the lifetime of one game and replenished
whenever food is consumed.

The human wins after every configured opponent has died while the human remains
alive. The game is lost as soon as the human dies while at least one opponent
survives, and simultaneous elimination of all remaining worms is a draw.

Human arrow keys represent absolute map directions. They are converted to the
relative 'Action' representation used by the game engine through
'Movement.directionToAction'.

Large maps use the normal viewport centered on the human worm. Every living
off-screen opponent receives its own colored direction indicator.
-}

module PlayGui where

import Config (maxFoodCount)
import Controller (collectActions)
import Game (maintainFoodCountAwayFromWorms, stepGame)
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
    , uiBorderColor
    , uiMutedColor
    )
import Movement (directionToAction)
import Types
import Viewport

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Text.Printf (printf)


-- -----------------------------------------------------------------------------
-- Play configuration
-- -----------------------------------------------------------------------------

-- | Initial number of game ticks simulated per second.
initialPlaySpeed :: Float
initialPlaySpeed = 3


-- | Display color of the human-controlled worm.
humanColor :: Color
humanColor =
    makeColorI 50 140 255 255


-- -----------------------------------------------------------------------------
-- Play state
-- -----------------------------------------------------------------------------

-- | One AI opponent participating in a playable game.
data PlayOpponent = PlayOpponent
    { playOpponentWormId :: Int
    , playOpponentName :: String
    , playOpponentController :: Controller
    , playOpponentColor :: Color
    }


-- | Current result of a human-versus-agents game.
data PlayResult
    = PlayRunning
    | PlayerWon
    | PlayerLost
    | PlayDraw
    deriving (Show, Eq)


-- | Complete state of one human-versus-agents game.
data PlayWorld = PlayWorld
    { playGameState :: GameState
    , playInitialState :: GameState
    , playHumanWormId :: Int
    , playOpponents :: [PlayOpponent]
    , playFoodTarget :: Int
    , playPendingAction :: Action
    , playPaused :: Bool
    , playSpeed :: Float
    , playAccumulator :: Float
    , playResult :: PlayResult
    }


-- | Creates a playable game using the legacy/default fixed food target.
--
-- Application setup normally uses 'initialPlayWorldWithFoodTarget' with the
-- selected scenario's adaptive interactive target.
initialPlayWorld :: Int -> [PlayOpponent] -> GameState -> PlayWorld
initialPlayWorld =
    initialPlayWorldWithFoodTarget maxFoodCount


-- | Creates a human-versus-agents game with a specific maintained food target.
initialPlayWorldWithFoodTarget :: Int -> Int -> [PlayOpponent] -> GameState -> PlayWorld
initialPlayWorldWithFoodTarget foodTarget humanId opponents initialState =
    PlayWorld
        { playGameState = initialState
        , playInitialState = initialState
        , playHumanWormId = humanId
        , playOpponents = opponents
        , playFoodTarget = foodTarget
        , playPendingAction = GoStraight
        , playPaused = True
        , playSpeed = initialPlaySpeed
        , playAccumulator = 0
        , playResult =
            resultForState
                humanId
                (map playOpponentWormId opponents)
                initialState
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


-- | Returns all IDs of configured AI opponents.
playOpponentIds :: PlayWorld -> [Int]
playOpponentIds =
    map playOpponentWormId . playOpponents


-- | Returns all controller assignments used by AI opponents.
playOpponentControllers :: PlayWorld -> [(Int, Controller)]
playOpponentControllers world =
    [ (playOpponentWormId opponent, playOpponentController opponent)
    | opponent <- playOpponents world
    ]


-- | Finds the configured metadata of one AI opponent.
findPlayOpponent :: Int -> PlayWorld -> Maybe PlayOpponent
findPlayOpponent targetId world =
    case filter ((== targetId) . playOpponentWormId) (playOpponents world) of
        opponent : _ -> Just opponent
        [] -> Nothing


-- | Returns the number of configured opponents still alive.
livingOpponentCount :: PlayWorld -> GameState -> Int
livingOpponentCount world state =
    length
        [ opponentId
        | opponentId <- playOpponentIds world
        , wormAliveById opponentId state
        ]


-- | Determines a human-versus-agents result from the current game state.
resultForState :: Int -> [Int] -> GameState -> PlayResult
resultForState humanId opponentIds state =
    case (humanAlive, anyOpponentAlive) of
        (True, True) -> PlayRunning
        (True, False) -> PlayerWon
        (False, True) -> PlayerLost
        (False, False) -> PlayDraw
  where
    humanAlive =
        wormAliveById humanId state

    anyOpponentAlive =
        any (`wormAliveById` state) opponentIds


-- -----------------------------------------------------------------------------
-- Game stepping
-- -----------------------------------------------------------------------------

-- | Advances the human-versus-agents game by one simultaneous game tick.
stepPlayWorld :: PlayWorld -> IO PlayWorld
stepPlayWorld world
    | playResult world /= PlayRunning =
        pure
            world
                { playPaused = True
                , playAccumulator = 0
                }

    | otherwise = do
        opponentActions <-
            collectActions
                (playOpponentControllers world)
                currentState

        let actions =
                (playHumanWormId world, playPendingAction world)
                    : opponentActions

            steppedState =
                stepGame actions currentState

        nextState <- maintainFoodCountAwayFromWorms (playFoodTarget world) steppedState

        let nextResult =
                resultForState
                    (playHumanWormId world)
                    (playOpponentIds world)
                    nextState

        pure
            world
                { playGameState = nextState
                , playPendingAction = GoStraight
                , playPaused =
                    if nextResult == PlayRunning
                        then playPaused world
                        else True
                , playAccumulator = 0
                , playResult = nextResult
                }
  where
    currentState =
        playGameState world


-- | Restarts the current human-versus-agents game.
restartPlay :: PlayWorld -> PlayWorld
restartPlay world =
    world
        { playGameState = playInitialState world
        , playPendingAction = GoStraight
        , playPaused = True
        , playAccumulator = 0
        , playResult =
            resultForState
                (playHumanWormId world)
                (playOpponentIds world)
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
        world {playPendingAction = action}


-- | Queues a global direction for the human worm when the requested turn is legal.
queueHumanDirection :: Direction -> PlayWorld -> PlayWorld
queueHumanDirection requestedDirection world =
    case findWormById (playHumanWormId world) (playGameState world) of
        Nothing ->
            world

        Just worm ->
            case directionToAction (wormDirection worm) requestedDirection of
                Nothing -> world
                Just action -> queueHumanAction action world


-- | Handles keyboard input during a human-versus-agents game.
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
                { playPaused = not (playPaused world)
                , playAccumulator = 0
                }

handlePlayEvent (EventKey (Char 'r') Down _ _) world =
    pure (restartPlay world)

handlePlayEvent (EventKey (Char 'R') Down _ _) world =
    pure (restartPlay world)

handlePlayEvent (EventKey (Char '+') Down _ _) world =
    pure world {playSpeed = min 20 (playSpeed world * 1.5)}

handlePlayEvent (EventKey (Char '=') Down _ _) world =
    pure world {playSpeed = min 20 (playSpeed world * 1.5)}

handlePlayEvent (EventKey (Char '-') Down _ _) world =
    pure world {playSpeed = max 0.25 (playSpeed world / 1.5)}

handlePlayEvent _ world =
    pure world


-- -----------------------------------------------------------------------------
-- Automatic updating
-- -----------------------------------------------------------------------------

-- | Updates human-versus-agents gameplay according to elapsed real time.
updatePlayWorld :: Float -> PlayWorld -> IO PlayWorld
updatePlayWorld deltaTime world
    | playPaused world = pure world
    | playResult world /= PlayRunning = pure world
    | accumulatedTime >= tickInterval =
        stepPlayWorld
            world
                { playAccumulator = accumulatedTime - tickInterval
                }
    | otherwise =
        pure world {playAccumulator = accumulatedTime}
  where
    accumulatedTime =
        playAccumulator world + deltaTime

    tickInterval =
        1 / playSpeed world


-- -----------------------------------------------------------------------------
-- Camera helpers
-- -----------------------------------------------------------------------------

-- | Returns the map position the play-mode camera should follow.
humanFocusPosition :: PlayWorld -> Position
humanFocusPosition world =
    case findWormById (playHumanWormId world) currentState of
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
        ( mapWidth currentMap `div` 2
        , mapHeight currentMap `div` 2
        )


-- -----------------------------------------------------------------------------
-- Board drawing
-- -----------------------------------------------------------------------------

-- | Returns the display color assigned to a worm in play mode.
playWormColor :: PlayWorld -> Worm -> Color
playWormColor world worm
    | wormId worm == playHumanWormId world =
        humanColor

    | otherwise =
        case findPlayOpponent (wormId worm) world of
            Just opponent -> playOpponentColor opponent
            Nothing -> white


-- | Draws the current game board for human-versus-agents gameplay.
--
-- Small maps are shown completely. Large maps use a viewport centered on the
-- human-controlled worm.
drawPlayGameState :: PlayWorld -> Picture
drawPlayGameState world
    | usesViewport currentMap =
        pictures
            [ drawMapViewport viewport currentMap
            , pictures
                [ drawColoredWormViewport
                    viewport
                    (playWormColor world worm)
                    worm
                | worm <- gameWorms currentState
                ]
            , drawPlayViewportIndicators
                world
                viewport
                currentState
                (humanFocusPosition world)
            ]

    | otherwise =
        pictures
            [ drawMap currentMap
            , pictures
                [ drawColoredWorm
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
        viewportAround currentMap (humanFocusPosition world)


-- -----------------------------------------------------------------------------
-- Play HUD
-- -----------------------------------------------------------------------------

-- | Draws the human worm's basic gameplay statistics.
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
                [ drawSectionHeader panelLeftX y labelColor label
                , drawStatLine panelLeftX (y - 32) "Length" (show (length (wormBody worm)))
                , drawStatLine panelLeftX (y - 56) "Food" (show (foodEaten (wormStats worm)))
                , drawStatLine panelLeftX (y - 80) "Kills" (show (kills (wormStats worm)))
                , drawStatLine panelLeftX (y - 104) "Age" (show (age (wormStats worm)))
                ]


-- | Number of columns used for the compact Play opponent roster.
playOpponentColumnCount :: Int -> Int
playOpponentColumnCount opponentCount
    | opponentCount <= 4 = 1
    | otherwise = 2


-- | Number of visual rows required by the Play opponent roster.
playOpponentRowCount :: Int -> Int
playOpponentRowCount opponentCount
    | opponentCount <= 0 = 0
    | otherwise =
        (opponentCount + columnCount - 1) `div` columnCount
  where
    columnCount =
        playOpponentColumnCount opponentCount


-- | Draws the configured AI opponents in a compact adaptive roster.
drawPlayOpponentRoster :: PlayWorld -> Float -> Picture
drawPlayOpponentRoster world startY =
    pictures (zipWith drawOpponent [0 ..] opponents)
  where
    opponents =
        playOpponents world

    opponentCount =
        length opponents

    columnCount =
        playOpponentColumnCount opponentCount

    rowCount =
        playOpponentRowCount opponentCount

    rowSpacing =
        29

    rowWidth =
        if columnCount == 1 then 440 else 210

    columnSpacing =
        230

    maximumNameLength =
        if columnCount == 1 then 35 else 16

    currentState =
        playGameState world

    -- | Draws one opponent entry in the adaptive roster.
    drawOpponent :: Int -> PlayOpponent -> Picture
    drawOpponent index opponent =
        pictures
            [ drawBox left (centerY + 12) rowWidth 25 rowFill
            , translate (left + 15) centerY $
                color dotColor $
                    circleSolid 5
            , drawGuiText
                (left + 28)
                (centerY - 7)
                0.078
                textColor
                label
            ]
      where
        columnIndex =
            if columnCount == 1
                then 0
                else index `div` rowCount

        rowIndex =
            if columnCount == 1
                then index
                else index `mod` rowCount

        left =
            panelLeftX + fromIntegral columnIndex * columnSpacing

        centerY =
            startY - fromIntegral rowIndex * rowSpacing

        alive =
            wormAliveById
                (playOpponentWormId opponent)
                currentState

        rowFill =
            if alive
                then makeColorI 24 28 34 255
                else makeColorI 18 21 25 255

        dotColor =
            if alive
                then playOpponentColor opponent
                else greyN 0.35

        textColor =
            if alive
                then white
                else greyN 0.48

        label =
            "W"
                ++ show (playOpponentWormId opponent)
                ++ "  "
                ++ shortenText maximumNameLength (playOpponentName opponent)


-- | Draws gameplay status, human statistics, opponent roster, and controls.
drawPlayStatus :: PlayWorld -> Picture
drawPlayStatus world =
    pictures
        [ drawPanel 185 360 515 700
        , drawGuiText panelLeftX 318 0.18 white "Play vs Agents"
        , drawStatusBadge 560 332 statusText resultColor
        , drawStatLine panelLeftX 274 "Tick" (show (gameTick currentState))
        , drawStatLine (panelLeftX + 205) 274 "Speed" (printf "%.2fx" (playSpeed world))

        , drawPlayWormStats
            205
            "You"
            humanColor
            (findWormById (playHumanWormId world) currentState)

        , drawSectionHeader
            panelLeftX
            65
            white
            opponentsHeader

        , drawPlayOpponentRoster world 35

        , drawGuiText
            panelLeftX
            (-145)
            0.085
            uiMutedColor
            "Arrow controls use global directions. Opposite direction is ignored."

        , drawFooter
            panelLeftX
            (-320)
            ["ARROWS move", "SPACE pause", "R restart", "+/- speed", "ESC back"]
        ]
  where
    currentState =
        playGameState world

    totalOpponents =
        length (playOpponents world)

    aliveOpponents =
        livingOpponentCount world currentState

    opponentsHeader =
        "AI opponents ("
            ++ show aliveOpponents
            ++ " / "
            ++ show totalOpponents
            ++ " alive)"

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


-- | Draws the complete human-versus-agents game.
drawPlayWorld :: PlayWorld -> IO Picture
drawPlayWorld world =
    pure $
        pictures
            [ translate boardOffsetX 0 $
                pictures
                    [ drawPlayGameState world
                    , drawPlayGameOverOverlay world
                    ]
            , drawPlayStatus world
            ]


-- | Draws a board-centered result overlay after playable mode has ended.
drawPlayGameOverOverlay :: PlayWorld -> Picture
drawPlayGameOverOverlay world
    | playResult world == PlayRunning =
        Blank

    | otherwise =
        pictures
            [ drawBox (-225) 112 450 224 (makeColorI 5 7 10 220)
            , translate 0 0 $
                color uiBorder $
                    rectangleWire 450 224
            , drawGuiText titleX 58 0.23 titleColor title
            , drawGuiText
                (-96)
                8
                0.10
                white
                ("Food eaten: " ++ maybe "-" (show . foodEaten . wormStats) humanWorm)
            , drawGuiText
                (-96)
                (-18)
                0.10
                white
                ("Age: " ++ maybe "-" (show . age . wormStats) humanWorm)
            , drawGuiText (-130) (-70) 0.095 uiMutedColor "R restart     ESC back"
            ]
  where
    currentState =
        playGameState world

    humanWorm =
        findWormById (playHumanWormId world) currentState

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
        uiBorderColor


-- -----------------------------------------------------------------------------
-- Viewport indicators
-- -----------------------------------------------------------------------------

-- | Draws off-screen indicators for every living AI opponent and nearest food.
drawPlayViewportIndicators :: PlayWorld -> Viewport -> GameState -> Position -> Picture
drawPlayViewportIndicators world viewport state focusPosition =
    pictures
        ( opponentIndicators
            ++ [ drawNearestFoodIndicator
                    viewport
                    (gameMap state)
                    focusPosition
               ]
        )
  where
    opponentIndicators =
        [ drawOffscreenIndicator
            opponentIndicatorInset
            viewport
            focusPosition
            headPosition
            (playOpponentColor opponent)
            (show (playOpponentWormId opponent))
        | opponent <- playOpponents world
        , Just worm <- [findWormById (playOpponentWormId opponent) state]
        , wormAlive worm
        , headPosition : _ <- [wormBody worm]
        ]