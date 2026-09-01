{-|
Module      : App
Description : Top-level application state, dynamic setup menus, and mode transitions.

This module coordinates the complete Gloss application. It stores menu
configuration, constructs Watch Agents and Play vs Agents sessions, routes
input to the currently active mode, renders menu screens, and updates active
games.

Both interactive setup screens support variable-size multi-worm rosters.
Watch Agents configures between two and nine independently selectable AI
controllers. Play vs Agents reserves Worm 1 for the human and configures one or
more independently selectable AI opponents. In both cases the effective roster
limit is additionally restricted by the predefined starting positions of the
selected scenario.

The actual Watch simulation is implemented in "Gui", while human-controlled
gameplay is implemented in "PlayGui".
-}

module App where

import AgentRegistry
import Gui
import PlayGui
import Game (randomizeFoodCountAwayFromWorms)
import Scenario
import Scenarios
import Types

import Data.List (elemIndex, findIndex)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Exit (exitSuccess)


-- -----------------------------------------------------------------------------
-- Application configuration
-- -----------------------------------------------------------------------------

-- | Minimum number of agents allowed in Watch Agents mode.
minimumWatchAgentCount :: Int
minimumWatchAgentCount = 2


-- | Maximum number of agents supported by the Watch Agents interface.
--
-- The actual maximum for one game is also limited by the number of predefined
-- worm starts available in the selected scenario.
maximumWatchAgentCount :: Int
maximumWatchAgentCount = 9

-- | Minimum number of AI opponents required by Play vs Agents.
minimumPlayOpponentCount :: Int
minimumPlayOpponentCount = 1


-- | Maximum number of AI opponents supported by Play vs Agents.
--
-- Worm 1 is reserved for the human, leaving at most eight AI-controlled worms
-- within the application's nine-worm interface limit.
maximumPlayOpponentCount :: Int
maximumPlayOpponentCount =
    maximumWatchAgentCount - 1


-- | Stable display colors assigned to Watch agents by roster position.
--
-- The palette contains one color for every agent supported by the interface.
-- Green is deliberately avoided as a primary color because food is rendered
-- green on the game board.
watchAgentColors :: [Color]
watchAgentColors =
    [ makeColorI 50 140 255 255
    , makeColorI 255 130 60 255
    , makeColorI 190 90 255 255
    , makeColorI 245 205 65 255
    , makeColorI 55 195 215 255
    , makeColorI 255 95 170 255
    , makeColorI 235 75 75 255
    , makeColorI 100 170 210 255
    , makeColorI 205 150 95 255
    ]


-- -----------------------------------------------------------------------------
-- Application state
-- -----------------------------------------------------------------------------

-- | Menu screen currently displayed by the application.
data MenuScreen
    = MainMenuScreen
    | WatchSetupScreen
    | PlaySetupScreen
    deriving (Show, Eq)


-- | Selected field of the dynamic Watch Agents setup screen.
--
-- 'WatchAgentField' stores the zero-based roster position of the selected worm.
data WatchSetupField
    = WatchMapField
    | WatchAgentField Int
    | WatchAddAgentField
    | WatchRemoveAgentField
    | WatchStartField
    | WatchBackField
    deriving (Show, Eq)


-- | Selected field of the dynamic Play vs Agents setup screen.
--
-- 'PlayOpponentField' stores the zero-based index within the AI-opponent roster.
data PlaySetupField
    = PlayMapField
    | PlayOpponentField Int
    | PlayAddOpponentField
    | PlayRemoveOpponentField
    | PlayStartField
    | PlayBackField
    deriving (Show, Eq)


-- | Complete state of the application menus and their current selections.
data MenuWorld = MenuWorld
    { menuScreen :: MenuScreen
    , menuMainSelection :: Int
    , menuScenarioIndex :: Int

    -- | Selected AgentOption index for every active Watch worm.
    --
    -- The length of this list is the requested Watch roster size.
    , menuWatchAgentIndices :: [Int]

    , menuWatchField :: WatchSetupField
    -- | Selected AgentOption index for every configured AI opponent.
    , menuPlayOpponentIndices :: [Int]
    , menuPlayField :: PlaySetupField
    }


-- | Complete top-level state of the CerviQ application.
--
-- The stored 'MenuWorld' is retained while a game is running so returning with
-- Escape restores the previous setup selections.
data AppWorld
    = AppMenu MenuWorld
    | AppWatch MenuWorld GuiWorld
    | AppPlay MenuWorld PlayWorld


-- -----------------------------------------------------------------------------
-- Initial state
-- -----------------------------------------------------------------------------

-- | Returns the index of an agent with the given name, defaulting to zero.
agentIndexByName :: String -> [AgentOption] -> Int
agentIndexByName targetName options =
    case findIndex ((== targetName) . agentOptionName) options of
        Just index -> index
        Nothing -> 0


-- | Creates the initial application menu and its default selections.
initialMenuWorld :: [AgentOption] -> MenuWorld
initialMenuWorld agentOptions =
    MenuWorld
        { menuScreen = MainMenuScreen
        , menuMainSelection = 0
        , menuScenarioIndex = 0
        , menuWatchAgentIndices =
            [ agentIndexByName "Q-learning V4 Reformed 30k + fallback" agentOptions
            , agentIndexByName "Safe Greedy Food" agentOptions
            ]
        , menuWatchField = WatchMapField
        , menuPlayOpponentIndices =
    [agentIndexByName "Q-learning V4 Reformed 30k + fallback" agentOptions]
        , menuPlayField = PlayMapField
        }


-- -----------------------------------------------------------------------------
-- General selection helpers
-- -----------------------------------------------------------------------------

-- | Moves an index cyclically through a collection of the given size.
cycleIndex :: Int -> Int -> Int -> Int
cycleIndex itemCount change currentIndex
    | itemCount <= 0 = 0
    | otherwise = (currentIndex + change) `mod` itemCount


-- | Moves to the previous value of a bounded enumeration, wrapping at the start.
previousEnum :: (Eq value, Enum value, Bounded value) => value -> value
previousEnum value
    | value == minBound = maxBound
    | otherwise = pred value


-- | Moves to the next value of a bounded enumeration, wrapping at the end.
nextEnum :: (Eq value, Enum value, Bounded value) => value -> value
nextEnum value
    | value == maxBound = minBound
    | otherwise = succ value


-- | Safely returns one item from a list.
itemAt :: Int -> [value] -> Maybe value
itemAt index values
    | index < 0 || index >= length values = Nothing
    | otherwise = Just (values !! index)


-- | Applies one transformation to the item at the given list index.
updateAt :: Int -> (value -> value) -> [value] -> [value]
updateAt targetIndex transform values =
    zipWith updateValue [0 ..] values
  where
    updateValue index value
        | index == targetIndex = transform value
        | otherwise = value


-- | Returns the currently selected scenario.
selectedScenario :: [Scenario] -> MenuWorld -> Maybe Scenario
selectedScenario scenarios menu =
    itemAt (menuScenarioIndex menu) scenarios


-- | Returns the name of the currently selected scenario.
selectedScenarioName :: [Scenario] -> MenuWorld -> String
selectedScenarioName scenarios menu =
    case selectedScenario scenarios menu of
        Just scenario -> scenarioName scenario
        Nothing -> "No scenario"


-- | Returns the name of an agent selected by its menu index.
selectedAgentName :: [AgentOption] -> Int -> String
selectedAgentName agentOptions index =
    case itemAt index agentOptions of
        Just option -> agentOptionName option
        Nothing -> "No agent"


-- -----------------------------------------------------------------------------
-- Dynamic Watch setup helpers
-- -----------------------------------------------------------------------------

-- | Returns the maximum Watch roster size supported by one scenario.
watchAgentCapacity :: Scenario -> Int
watchAgentCapacity scenario =
    min maximumWatchAgentCount (scenarioMaximumWormCount scenario)


-- | Returns the ordered fields currently visible on the Watch setup screen.
--
-- One field is generated for every active worm, making menu navigation adapt to
-- additions and removals without hard-coded Worm 1 / Worm 2 constructors.
watchSetupFields :: MenuWorld -> [WatchSetupField]
watchSetupFields menu =
    [WatchMapField]
        ++ [WatchAgentField index | index <- [0 .. length (menuWatchAgentIndices menu) - 1]]
        ++ [ WatchAddAgentField
           , WatchRemoveAgentField
           , WatchStartField
           , WatchBackField
           ]


-- | Moves the selected Watch setup field by the given offset.
moveWatchField :: Int -> MenuWorld -> MenuWorld
moveWatchField change menu =
    menu {menuWatchField = fields !! newIndex}
  where
    fields = watchSetupFields menu

    currentIndex =
        case elemIndex (menuWatchField menu) fields of
            Just index -> index
            Nothing -> 0

    newIndex =
        cycleIndex (length fields) change currentIndex


-- | Keeps a Watch field valid after the number of configured agents changes.
normalizeWatchField :: Int -> WatchSetupField -> WatchSetupField
normalizeWatchField agentCount field =
    case field of
        WatchAgentField index
            | agentCount <= 0 -> WatchMapField
            | index >= agentCount -> WatchAgentField (agentCount - 1)

        _ ->
            field


-- | Adjusts the configured Watch roster to the capacity of a selected scenario.
--
-- This is primarily relevant when switching between scenarios with different
-- numbers of predefined starting positions.
normalizeWatchMenuForScenario :: Scenario -> MenuWorld -> MenuWorld
normalizeWatchMenuForScenario scenario menu =
    menu
        { menuWatchAgentIndices = trimmedIndices
        , menuWatchField = normalizeWatchField (length trimmedIndices) (menuWatchField menu)
        }
  where
    trimmedIndices =
        take (watchAgentCapacity scenario) (menuWatchAgentIndices menu)


-- | Returns whether another Watch agent can be added for the selected scenario.
canAddWatchAgent :: [Scenario] -> MenuWorld -> Bool
canAddWatchAgent scenarios menu =
    case selectedScenario scenarios menu of
        Nothing -> False
        Just scenario ->
            length (menuWatchAgentIndices menu) < watchAgentCapacity scenario


-- | Returns whether the final Watch agent can be removed.
canRemoveWatchAgent :: MenuWorld -> Bool
canRemoveWatchAgent menu =
    length (menuWatchAgentIndices menu) > minimumWatchAgentCount


-- | Adds one Watch agent using Safe Greedy Food as the initial controller.
--
-- Focus moves directly to the newly added worm so its controller can be changed
-- immediately with Left / Right.
addWatchAgent :: [AgentOption] -> [Scenario] -> MenuWorld -> MenuWorld
addWatchAgent agentOptions scenarios menu
    | not (canAddWatchAgent scenarios menu) = menu
    | otherwise =
        menu
            { menuWatchAgentIndices = newIndices
            , menuWatchField = WatchAgentField newAgentIndex
            }
  where
    newAgentIndex =
        length (menuWatchAgentIndices menu)

    defaultOptionIndex =
        agentIndexByName "Safe Greedy Food" agentOptions

    newIndices =
        menuWatchAgentIndices menu ++ [defaultOptionIndex]


-- | Removes the last configured Watch agent while preserving the minimum roster.
removeLastWatchAgent :: MenuWorld -> MenuWorld
removeLastWatchAgent menu
    | not (canRemoveWatchAgent menu) = menu
    | otherwise =
        menu
            { menuWatchAgentIndices = init (menuWatchAgentIndices menu)
            , menuWatchField = WatchRemoveAgentField
            }


-- | Changes the selected value of the current Watch setup field.
changeWatchValueBy :: Int -> [AgentOption] -> [Scenario] -> MenuWorld -> MenuWorld
changeWatchValueBy change agentOptions scenarios menu =
    case menuWatchField menu of
        WatchMapField ->
            changeScenario

        WatchAgentField agentPosition ->
            menu
                { menuWatchAgentIndices =
                    updateAt
                        agentPosition
                        (cycleIndex (length agentOptions) change)
                        (menuWatchAgentIndices menu)
                }

        _ ->
            menu
  where
    changedScenarioMenu =
        menu
            { menuScenarioIndex =
                cycleIndex
                    (length scenarios)
                    change
                    (menuScenarioIndex menu)
            }

    changeScenario =
        case selectedScenario scenarios changedScenarioMenu of
            Just scenario -> normalizeMenuForScenario scenario changedScenarioMenu
            Nothing -> changedScenarioMenu


-- -----------------------------------------------------------------------------
-- Dynamic Play setup helpers
-- -----------------------------------------------------------------------------

-- | Returns the maximum number of AI opponents available in one scenario.
--
-- One predefined start is reserved for the human-controlled Worm 1.
playOpponentCapacity :: Scenario -> Int
playOpponentCapacity scenario =
    min
        maximumPlayOpponentCount
        (max 0 (scenarioMaximumWormCount scenario - 1))


-- | Returns the ordered fields currently visible on the Play setup screen.
playSetupFields :: MenuWorld -> [PlaySetupField]
playSetupFields menu =
    [PlayMapField]
        ++ [PlayOpponentField index | index <- [0 .. length (menuPlayOpponentIndices menu) - 1]]
        ++ [ PlayAddOpponentField
           , PlayRemoveOpponentField
           , PlayStartField
           , PlayBackField
           ]


-- | Moves the selected Play setup field by the given offset.
movePlayField :: Int -> MenuWorld -> MenuWorld
movePlayField change menu =
    menu {menuPlayField = fields !! newIndex}
  where
    fields =
        playSetupFields menu

    currentIndex =
        case elemIndex (menuPlayField menu) fields of
            Just index -> index
            Nothing -> 0

    newIndex =
        cycleIndex (length fields) change currentIndex


-- | Keeps a Play field valid after the number of opponents changes.
normalizePlayField :: Int -> PlaySetupField -> PlaySetupField
normalizePlayField opponentCount field =
    case field of
        PlayOpponentField index
            | opponentCount <= 0 -> PlayMapField
            | index >= opponentCount -> PlayOpponentField (opponentCount - 1)

        _ ->
            field


-- | Restricts the configured Play roster to the selected scenario's capacity.
normalizePlayMenuForScenario :: Scenario -> MenuWorld -> MenuWorld
normalizePlayMenuForScenario scenario menu =
    menu
        { menuPlayOpponentIndices = trimmedIndices
        , menuPlayField =
            normalizePlayField
                (length trimmedIndices)
                (menuPlayField menu)
        }
  where
    trimmedIndices =
        take
            (playOpponentCapacity scenario)
            (menuPlayOpponentIndices menu)


-- | Normalizes both dynamic rosters after a scenario change.
normalizeMenuForScenario :: Scenario -> MenuWorld -> MenuWorld
normalizeMenuForScenario scenario =
    normalizePlayMenuForScenario scenario
        . normalizeWatchMenuForScenario scenario


-- | Returns whether another AI opponent can be added in Play mode.
canAddPlayOpponent :: [Scenario] -> MenuWorld -> Bool
canAddPlayOpponent scenarios menu =
    case selectedScenario scenarios menu of
        Nothing -> False
        Just scenario ->
            length (menuPlayOpponentIndices menu) < playOpponentCapacity scenario


-- | Returns whether the final Play opponent can be removed.
canRemovePlayOpponent :: MenuWorld -> Bool
canRemovePlayOpponent menu =
    length (menuPlayOpponentIndices menu) > minimumPlayOpponentCount


-- | Adds one AI opponent using Safe Greedy Food as its initial controller.
addPlayOpponent :: [AgentOption] -> [Scenario] -> MenuWorld -> MenuWorld
addPlayOpponent agentOptions scenarios menu
    | not (canAddPlayOpponent scenarios menu) = menu
    | otherwise =
        menu
            { menuPlayOpponentIndices = newIndices
            , menuPlayField = PlayOpponentField newOpponentIndex
            }
  where
    newOpponentIndex =
        length (menuPlayOpponentIndices menu)

    defaultOptionIndex =
        agentIndexByName "Safe Greedy Food" agentOptions

    newIndices =
        menuPlayOpponentIndices menu ++ [defaultOptionIndex]


-- | Removes the final configured AI opponent while preserving at least one.
removeLastPlayOpponent :: MenuWorld -> MenuWorld
removeLastPlayOpponent menu
    | not (canRemovePlayOpponent menu) = menu
    | otherwise =
        menu
            { menuPlayOpponentIndices = init (menuPlayOpponentIndices menu)
            , menuPlayField = PlayRemoveOpponentField
            }


-- | Changes the currently selected value of the Play setup.
changePlayValueBy :: Int -> [AgentOption] -> [Scenario] -> MenuWorld -> MenuWorld
changePlayValueBy change agentOptions scenarios menu =
    case menuPlayField menu of
        PlayMapField ->
            changeScenario

        PlayOpponentField opponentPosition ->
            menu
                { menuPlayOpponentIndices =
                    updateAt
                        opponentPosition
                        (cycleIndex (length agentOptions) change)
                        (menuPlayOpponentIndices menu)
                }

        _ ->
            menu
  where
    changedScenarioMenu =
        menu
            { menuScenarioIndex =
                cycleIndex
                    (length scenarios)
                    change
                    (menuScenarioIndex menu)
            }

    changeScenario =
        case selectedScenario scenarios changedScenarioMenu of
            Just scenario ->
                normalizeMenuForScenario scenario changedScenarioMenu

            Nothing ->
                changedScenarioMenu


-- -----------------------------------------------------------------------------
-- Game mode construction
-- -----------------------------------------------------------------------------

-- | Creates Watch Agents mode from the current dynamic roster.
--
-- Every configured controller is paired with the corresponding predefined worm
-- start. Authored map food is discarded and replaced with the adaptive
-- scenario-specific number of randomly positioned food items.
startWatch :: [AgentOption] -> [Scenario] -> MenuWorld -> IO (Maybe AppWorld)
startWatch agentOptions scenarios menu =
    case selectedScenario scenarios menu of
        Nothing ->
            pure Nothing

        Just scenario
            | wormCount < minimumWatchAgentCount ->
                pure Nothing

            | wormCount > watchAgentCapacity scenario ->
                pure Nothing

            | otherwise ->
                case
                    ( scenarioInitialStateForWormCount wormCount scenario
                    , mapM (\index -> itemAt index agentOptions) selectedIndices
                    )
                of
                    (Just baseState, Just selectedOptions) -> do
                        let foodTarget =
                                scenarioFoodCountForWorms wormCount scenario

                        initialState <-
                            randomizeFoodCountAwayFromWorms foodTarget baseState

                        let wormIds =
                                map wormId (gameWorms initialState)

                            agents =
                                zipWith3
                                    makeGuiAgent
                                    wormIds
                                    (take wormCount watchAgentColors)
                                    selectedOptions

                        if length agents == wormCount
                            then
                                pure $
                                    Just $
                                        AppWatch
                                            menu
                                            ( initialGuiWorldWithFoodTarget
                                                foodTarget
                                                agents
                                                initialState
                                            )

                            else
                                pure Nothing

                    _ ->
                        pure Nothing
  where
    selectedIndices =
        menuWatchAgentIndices menu

    wormCount =
        length selectedIndices


-- | Converts one selected agent option into a playable AI opponent.
makePlayOpponent :: Int -> Color -> AgentOption -> PlayOpponent
makePlayOpponent targetId opponentColor option =
    PlayOpponent
        { playOpponentWormId = targetId
        , playOpponentName = agentOptionName option
        , playOpponentController = agentOptionController option
        , playOpponentColor = opponentColor
        }

-- | Creates Play vs Agents mode from the current dynamic opponent roster.
--
-- Worm 1 is controlled by the human. Authored food is cleared and the scenario's
-- adaptive interactive food target is randomly distributed before play starts.
startPlay :: [AgentOption] -> [Scenario] -> MenuWorld -> IO (Maybe AppWorld)
startPlay agentOptions scenarios menu =
    case selectedScenario scenarios menu of
        Nothing ->
            pure Nothing

        Just scenario
            | opponentCount < minimumPlayOpponentCount ->
                pure Nothing

            | opponentCount > playOpponentCapacity scenario ->
                pure Nothing

            | otherwise ->
                case
                    ( scenarioInitialStateForWormCount wormCount scenario
                    , mapM (\index -> itemAt index agentOptions) selectedIndices
                    )
                of
                    (Just baseState, Just selectedOptions) -> do
                        let foodTarget =
                                scenarioFoodCountForWorms wormCount scenario

                        initialState <-
                            randomizeFoodCountAwayFromWorms foodTarget baseState

                        case map wormId (gameWorms initialState) of
                            humanId : opponentIds
                                | length opponentIds == opponentCount ->
                                    let colors =
                                            take opponentCount (drop 1 watchAgentColors)

                                        opponents =
                                            zipWith3
                                                makePlayOpponent
                                                opponentIds
                                                colors
                                                selectedOptions
                                    in
                                        pure $
                                            Just $
                                                AppPlay
                                                    menu
                                                    ( initialPlayWorldWithFoodTarget
                                                        foodTarget
                                                        humanId
                                                        opponents
                                                        initialState
                                                    )

                            _ ->
                                pure Nothing

                    _ ->
                        pure Nothing
  where
    selectedIndices =
        menuPlayOpponentIndices menu

    opponentCount =
        length selectedIndices

    wormCount =
        opponentCount + 1


-- -----------------------------------------------------------------------------
-- Main menu input
-- -----------------------------------------------------------------------------

-- | Handles keyboard input on the main menu.
handleMainMenuEvent :: Event -> MenuWorld -> IO AppWorld
handleMainMenuEvent (EventKey (SpecialKey KeyUp) Down _ _) menu =
    pure $ AppMenu menu {menuMainSelection = cycleIndex 3 (-1) (menuMainSelection menu)}

handleMainMenuEvent (EventKey (SpecialKey KeyDown) Down _ _) menu =
    pure $ AppMenu menu {menuMainSelection = cycleIndex 3 1 (menuMainSelection menu)}

handleMainMenuEvent (EventKey (SpecialKey KeyEnter) Down _ _) menu =
    case menuMainSelection menu of
        0 -> pure $ AppMenu menu {menuScreen = WatchSetupScreen}
        1 -> pure $ AppMenu menu {menuScreen = PlaySetupScreen}
        _ -> exitSuccess

handleMainMenuEvent _ menu =
    pure (AppMenu menu)


-- -----------------------------------------------------------------------------
-- Watch setup input
-- -----------------------------------------------------------------------------

-- | Handles keyboard input on the dynamic Watch Agents setup screen.
handleWatchSetupEvent :: [AgentOption] -> [Scenario] -> Event -> MenuWorld -> IO AppWorld
handleWatchSetupEvent _ _ (EventKey (SpecialKey KeyUp) Down _ _) menu =
    pure $ AppMenu $ moveWatchField (-1) menu

handleWatchSetupEvent _ _ (EventKey (SpecialKey KeyDown) Down _ _) menu =
    pure $ AppMenu $ moveWatchField 1 menu

handleWatchSetupEvent agentOptions scenarios (EventKey (SpecialKey KeyLeft) Down _ _) menu =
    pure $ AppMenu $ changeWatchValueBy (-1) agentOptions scenarios menu

handleWatchSetupEvent agentOptions scenarios (EventKey (SpecialKey KeyRight) Down _ _) menu =
    pure $ AppMenu $ changeWatchValueBy 1 agentOptions scenarios menu

handleWatchSetupEvent agentOptions scenarios (EventKey (SpecialKey KeyEnter) Down _ _) menu =
    case menuWatchField menu of
        WatchAddAgentField ->
            pure $ AppMenu $ addWatchAgent agentOptions scenarios menu

        WatchRemoveAgentField ->
            pure $ AppMenu $ removeLastWatchAgent menu

        WatchStartField -> do
            maybeWorld <-
                startWatch agentOptions scenarios menu

            pure $
                case maybeWorld of
                    Just world -> world
                    Nothing -> AppMenu menu

        WatchBackField ->
            pure $ AppMenu menu {menuScreen = MainMenuScreen}

        _ ->
            pure (AppMenu menu)

handleWatchSetupEvent _ _ (EventKey (SpecialKey KeyEsc) Down _ _) menu =
    pure $ AppMenu menu {menuScreen = MainMenuScreen}

handleWatchSetupEvent _ _ _ menu =
    pure (AppMenu menu)


-- -----------------------------------------------------------------------------
-- Play setup input
-- -----------------------------------------------------------------------------

-- | Handles keyboard input on the dynamic Play vs Agents setup screen.
handlePlaySetupEvent :: [AgentOption] -> [Scenario] -> Event -> MenuWorld -> IO AppWorld
handlePlaySetupEvent _ _ (EventKey (SpecialKey KeyUp) Down _ _) menu =
    pure $ AppMenu $ movePlayField (-1) menu

handlePlaySetupEvent _ _ (EventKey (SpecialKey KeyDown) Down _ _) menu =
    pure $ AppMenu $ movePlayField 1 menu

handlePlaySetupEvent agentOptions scenarios (EventKey (SpecialKey KeyLeft) Down _ _) menu =
    pure $ AppMenu $ changePlayValueBy (-1) agentOptions scenarios menu

handlePlaySetupEvent agentOptions scenarios (EventKey (SpecialKey KeyRight) Down _ _) menu =
    pure $ AppMenu $ changePlayValueBy 1 agentOptions scenarios menu

handlePlaySetupEvent agentOptions scenarios (EventKey (SpecialKey KeyEnter) Down _ _) menu =
    case menuPlayField menu of
        PlayAddOpponentField ->
            pure $ AppMenu $ addPlayOpponent agentOptions scenarios menu

        PlayRemoveOpponentField ->
            pure $ AppMenu $ removeLastPlayOpponent menu

        PlayStartField -> do
            maybeWorld <-
                startPlay agentOptions scenarios menu

            pure $
                case maybeWorld of
                    Just world -> world
                    Nothing -> AppMenu menu

        PlayBackField ->
            pure $ AppMenu menu {menuScreen = MainMenuScreen}

        _ ->
            pure (AppMenu menu)

handlePlaySetupEvent _ _ (EventKey (SpecialKey KeyEsc) Down _ _) menu =
    pure $ AppMenu menu {menuScreen = MainMenuScreen}

handlePlaySetupEvent _ _ _ menu =
    pure (AppMenu menu)


-- -----------------------------------------------------------------------------
-- Application input
-- -----------------------------------------------------------------------------

-- | Routes keyboard input to the currently active application mode.
handleAppEvent :: [AgentOption] -> [Scenario] -> Event -> AppWorld -> IO AppWorld
handleAppEvent agentOptions scenarios event (AppMenu menu) =
    case menuScreen menu of
        MainMenuScreen ->
            handleMainMenuEvent event menu

        WatchSetupScreen ->
            handleWatchSetupEvent agentOptions scenarios event menu

        PlaySetupScreen ->
            handlePlaySetupEvent agentOptions scenarios event menu

handleAppEvent _ _ (EventKey (SpecialKey KeyEsc) Down _ _) (AppWatch menu _) =
    pure $ AppMenu menu {menuScreen = WatchSetupScreen}

handleAppEvent _ _ event (AppWatch menu guiWorld) = do
    updatedGuiWorld <- handleGuiEvent event guiWorld
    pure (AppWatch menu updatedGuiWorld)

handleAppEvent _ _ (EventKey (SpecialKey KeyEsc) Down _ _) (AppPlay menu _) =
    pure $ AppMenu menu {menuScreen = PlaySetupScreen}

handleAppEvent _ _ event (AppPlay menu playWorld) = do
    updatedPlayWorld <- handlePlayEvent event playWorld
    pure (AppPlay menu updatedPlayWorld)


-- -----------------------------------------------------------------------------
-- Shared menu drawing
-- -----------------------------------------------------------------------------

-- | Draws one selectable row of the main menu.
drawMenuRow :: Bool -> Float -> String -> Picture
drawMenuRow selected y contents =
    drawMenuOptionRow (-290) y 580 selected contents


-- | Draws one compact controller-selection row on the Watch setup screen.
--
-- Compact rows allow the same layout to accommodate up to nine configured
-- worms without requiring scrolling.
drawCompactAgentSettingRow :: Float -> Bool -> String -> String -> Picture
drawCompactAgentSettingRow top selected label value =
    pictures
        [ drawBox (-325) top 650 34 rowFill
        , translate 0 (top - 17) $
            color borderColor $
                rectangleWire 650 34
        , drawBox (-325) top 5 34 accentFill
        , drawGuiText (-307) (top - 23) 0.085 uiMutedColor label
        , drawGuiText (-218) (top - 23) 0.088 textColor ("< " ++ shortenText 50 value ++ " >")
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


-- | Draws one compact Watch setup button, optionally in a disabled state.
drawCompactSetupButton :: Float -> Float -> Float -> Bool -> Bool -> String -> Picture
drawCompactSetupButton left top width selected enabled label =
    pictures
        [ drawBox left top width 34 rowFill
        , translate (left + width / 2) (top - 17) $
            color borderColor $
                rectangleWire width 34
        , drawBox left top 5 34 accentFill
        , drawGuiText (left + 18) (top - 23) 0.085 textColor label
        ]
  where
    rowFill
        | not enabled = makeColorI 20 23 28 255
        | selected = uiSelectedFill
        | otherwise = makeColorI 24 28 34 255

    borderColor
        | selected = uiAccentColor
        | otherwise = uiBorderColor

    accentFill
        | selected = uiAccentColor
        | otherwise = rowFill

    textColor
        | not enabled = greyN 0.48
        | selected = white
        | otherwise = greyN 0.86


-- -----------------------------------------------------------------------------
-- Main menu drawing
-- -----------------------------------------------------------------------------

-- | Draws the main CerviQ menu.
drawMainMenu :: MenuWorld -> Picture
drawMainMenu menu =
    pictures
        [ drawPanel (-360) 275 720 470
        , drawTitle (-285) 205 "CerviQ" "Q-learning worm arena"
        , drawMenuRow (menuMainSelection menu == 0) 95 "Watch Agents"
        , drawMenuRow (menuMainSelection menu == 1) 40 "Play vs Agents"
        , drawMenuRow (menuMainSelection menu == 2) (-15) "Quit"
        , drawFooter (-285) (-150) ["UP/DOWN select", "ENTER confirm"]
        ]


-- -----------------------------------------------------------------------------
-- Watch setup drawing
-- -----------------------------------------------------------------------------

-- | Draws the dynamic Watch Agents setup screen.
--
-- Agent rows are deliberately compact so the interface remains usable with up
-- to nine worms. Add and Remove controls modify only the setup roster; once a
-- simulation starts, its roster remains fixed.
drawWatchSetup :: [AgentOption] -> [Scenario] -> MenuWorld -> Picture
drawWatchSetup agentOptions scenarios menu =
    pictures
        ( [ drawPanel (-395) 350 790 730
          , drawTitle (-325) 300 "Watch Agents" "Configure multiple AI worms and inspect their decisions."
          , drawSettingRow (-325) 215 650 (menuWatchField menu == WatchMapField) "Map" scenarioName'
          , drawSectionHeader (-325) 118 white agentsHeader
          ]
            ++ agentRows
            ++ [ drawCompactSetupButton
                    (-325)
                    controlsTop
                    314
                    (menuWatchField menu == WatchAddAgentField)
                    addEnabled
                    "+ Add agent"

               , drawCompactSetupButton
                    11
                    controlsTop
                    314
                    (menuWatchField menu == WatchRemoveAgentField)
                    removeEnabled
                    "- Remove last"

               , drawCompactSetupButton
                    (-325)
                    actionsTop
                    314
                    (menuWatchField menu == WatchStartField)
                    True
                    "Start"

               , drawCompactSetupButton
                    11
                    actionsTop
                    314
                    (menuWatchField menu == WatchBackField)
                    True
                    "Back"

               , drawFooter
                    (-325)
                    (-358)
                    ["UP/DOWN field", "LEFT/RIGHT agent", "ENTER action", "ESC back"]
               ]
        )
  where
    agentIndices =
        menuWatchAgentIndices menu

    agentCount =
        length agentIndices

    scenarioName' =
        selectedScenarioName scenarios menu

    capacity =
        case selectedScenario scenarios menu of
            Just scenario -> watchAgentCapacity scenario
            Nothing -> 0

    agentsHeader =
        "Agents (" ++ show agentCount ++ " / " ++ show capacity ++ " available)"

    agentStartTop =
        96

    agentSpacing =
        38

    controlsTop =
        agentStartTop - fromIntegral agentCount * agentSpacing - 14

    actionsTop =
        controlsTop - 46

    addEnabled =
        canAddWatchAgent scenarios menu

    removeEnabled =
        canRemoveWatchAgent menu

    agentRows =
        zipWith drawAgentRow [0 ..] agentIndices

    drawAgentRow :: Int -> Int -> Picture
    drawAgentRow rosterIndex optionIndex =
        drawCompactAgentSettingRow
            (agentStartTop - fromIntegral rosterIndex * agentSpacing)
            (menuWatchField menu == WatchAgentField rosterIndex)
            ("Worm " ++ show (rosterIndex + 1))
            (selectedAgentName agentOptions optionIndex)


-- -----------------------------------------------------------------------------
-- Play setup drawing
-- -----------------------------------------------------------------------------

-- | Draws the dynamic Play vs Agents setup screen.
--
-- Worm 1 is reserved for the human. The remaining compact rows configure the
-- AI worms independently and adapt to the selected scenario's capacity.
drawPlaySetup :: [AgentOption] -> [Scenario] -> MenuWorld -> Picture
drawPlaySetup agentOptions scenarios menu =
    pictures
        ( [ drawPanel (-395) 350 790 730
          , drawTitle
                (-325)
                300
                "Play vs Agents"
                "Control Worm 1 and configure one or more AI opponents."

          , drawSettingRow
                (-325)
                215
                650
                (menuPlayField menu == PlayMapField)
                "Map"
                scenarioName'

          , drawSectionHeader
                (-325)
                118
                white
                opponentsHeader
          ]
            ++ opponentRows
            ++ [ drawCompactSetupButton
                    (-325)
                    controlsTop
                    314
                    (menuPlayField menu == PlayAddOpponentField)
                    addEnabled
                    "+ Add opponent"

               , drawCompactSetupButton
                    11
                    controlsTop
                    314
                    (menuPlayField menu == PlayRemoveOpponentField)
                    removeEnabled
                    "- Remove last"

               , drawCompactSetupButton
                    (-325)
                    actionsTop
                    314
                    (menuPlayField menu == PlayStartField)
                    True
                    "Start"

               , drawCompactSetupButton
                    11
                    actionsTop
                    314
                    (menuPlayField menu == PlayBackField)
                    True
                    "Back"

               , drawFooter
                    (-325)
                    (-330)
                    ["In game: ARROWS move", "SPACE pause", "R restart", "+/- speed"]

               , drawFooter
                    (-325)
                    (-358)
                    ["UP/DOWN field", "LEFT/RIGHT agent", "ENTER action", "ESC back"]
               ]
        )
  where
    opponentIndices =
        menuPlayOpponentIndices menu

    opponentCount =
        length opponentIndices

    scenarioName' =
        selectedScenarioName scenarios menu

    capacity =
        case selectedScenario scenarios menu of
            Just scenario -> playOpponentCapacity scenario
            Nothing -> 0

    opponentsHeader =
        "AI opponents ("
            ++ show opponentCount
            ++ " / "
            ++ show capacity
            ++ " available)"

    opponentStartTop =
        96

    opponentSpacing =
        38

    controlsTop =
        opponentStartTop
            - fromIntegral opponentCount * opponentSpacing
            - 14

    actionsTop =
        controlsTop - 46

    addEnabled =
        canAddPlayOpponent scenarios menu

    removeEnabled =
        canRemovePlayOpponent menu

    opponentRows =
        zipWith drawOpponentRow [0 ..] opponentIndices

    -- | Draws one independently configurable AI opponent.
    drawOpponentRow :: Int -> Int -> Picture
    drawOpponentRow rosterIndex optionIndex =
        drawCompactAgentSettingRow
            (opponentStartTop - fromIntegral rosterIndex * opponentSpacing)
            (menuPlayField menu == PlayOpponentField rosterIndex)
            ("Worm " ++ show (rosterIndex + 2))
            (selectedAgentName agentOptions optionIndex)


-- | Draws the currently active menu screen.
drawMenuWorld :: [AgentOption] -> [Scenario] -> MenuWorld -> Picture
drawMenuWorld agentOptions scenarios menu =
    case menuScreen menu of
        MainMenuScreen ->
            drawMainMenu menu

        WatchSetupScreen ->
            drawWatchSetup agentOptions scenarios menu

        PlaySetupScreen ->
            drawPlaySetup agentOptions scenarios menu


-- -----------------------------------------------------------------------------
-- Application drawing and updating
-- -----------------------------------------------------------------------------

-- | Draws the currently active application mode.
drawAppWorld :: [AgentOption] -> [Scenario] -> AppWorld -> IO Picture
drawAppWorld agentOptions scenarios (AppMenu menu) =
    pure (drawMenuWorld agentOptions scenarios menu)

drawAppWorld _ _ (AppWatch _ guiWorld) =
    drawGuiWorld guiWorld

drawAppWorld _ _ (AppPlay _ playWorld) =
    drawPlayWorld playWorld


-- | Updates the currently active application mode according to elapsed time.
updateAppWorld :: Float -> AppWorld -> IO AppWorld
updateAppWorld _ world@(AppMenu _) =
    pure world

updateAppWorld deltaTime (AppWatch menu guiWorld) = do
    updatedGuiWorld <- updateGuiWorld deltaTime guiWorld
    pure (AppWatch menu updatedGuiWorld)

updateAppWorld deltaTime (AppPlay menu playWorld) = do
    updatedPlayWorld <- updatePlayWorld deltaTime playWorld
    pure (AppPlay menu updatedPlayWorld)


-- -----------------------------------------------------------------------------
-- Application entry point
-- -----------------------------------------------------------------------------

-- | Loads application resources and starts the Gloss event loop.
runCerviQ :: IO ()
runCerviQ = do
    agentOptions <- loadAgentOptions

    playIO
        guiDisplay
        black
        60
        (AppMenu (initialMenuWorld agentOptions))
        (drawAppWorld agentOptions allScenarios)
        (handleAppEvent agentOptions allScenarios)
        updateAppWorld