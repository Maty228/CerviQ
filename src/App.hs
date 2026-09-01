{-|
Module      : App
Description : Top-level application state, menus, and mode transitions for CerviQ.

This module coordinates the complete Gloss application. It stores menu
configuration, constructs Watch Agents and Play vs Agent sessions, routes input
to the currently active mode, renders menu screens, and updates the active game.

The actual Watch simulation is implemented in "Gui", while human-controlled
gameplay is implemented in "PlayGui". This module is responsible for switching
between those modes rather than implementing their game logic.
-}

module App where

import AgentRegistry
import Gui
import PlayGui
import Scenario
import Scenarios
import Types

import Data.List (findIndex)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Exit (exitSuccess)


-- -----------------------------------------------------------------------------
-- Application state
-- -----------------------------------------------------------------------------

-- | Menu screen currently displayed by the application.
data MenuScreen
    = MainMenuScreen
    | WatchSetupScreen
    | PlaySetupScreen
    deriving (Show, Eq)


-- | Selected field of the Watch Agents setup screen.
data WatchSetupField
    = WatchMapField
    | WatchAgentOneField
    | WatchAgentTwoField
    | WatchStartField
    | WatchBackField
    deriving (Show, Eq, Enum, Bounded)


-- | Selected field of the Play vs Agent setup screen.
data PlaySetupField
    = PlayMapField
    | PlayOpponentField
    | PlayStartField
    | PlayBackField
    deriving (Show, Eq, Enum, Bounded)


-- | Complete state of the application menus and their current selections.
data MenuWorld = MenuWorld
    {
        menuScreen :: MenuScreen,
        menuMainSelection :: Int,
        menuScenarioIndex :: Int,
        menuAgentOneIndex :: Int,
        menuAgentTwoIndex :: Int,
        menuWatchField :: WatchSetupField,
        menuPlayOpponentIndex :: Int,
        menuPlayField :: PlaySetupField
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
        {
            menuScreen = MainMenuScreen,
            menuMainSelection = 0,
            menuScenarioIndex = 0,
            menuAgentOneIndex = agentIndexByName "Q-learning V4 Reformed 30k + fallback" agentOptions,
            menuAgentTwoIndex = agentIndexByName "Safe Greedy Food" agentOptions,
            menuWatchField = WatchMapField,
            menuPlayOpponentIndex = agentIndexByName "Q-learning V4 Reformed 30k + fallback" agentOptions,
            menuPlayField = PlayMapField
        }


-- -----------------------------------------------------------------------------
-- Selection helpers
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


-- | Returns the name of the currently selected scenario.
selectedScenarioName :: [Scenario] -> MenuWorld -> String
selectedScenarioName scenarios menu =
    case itemAt (menuScenarioIndex menu) scenarios of
        Just scenario -> scenarioName scenario
        Nothing -> "No scenario"


-- | Returns the name of an agent selected by its menu index.
selectedAgentName :: [AgentOption] -> Int -> String
selectedAgentName agentOptions index =
    case itemAt index agentOptions of
        Just option -> agentOptionName option
        Nothing -> "No agent"


-- -----------------------------------------------------------------------------
-- Game mode construction
-- -----------------------------------------------------------------------------

-- | Creates Watch Agents mode from the current setup selections.
--
-- Watch mode currently assigns controllers to the first two worms defined by
-- the selected scenario.
startWatch :: [AgentOption] -> [Scenario] -> MenuWorld -> Maybe AppWorld
startWatch agentOptions scenarios menu = do
    scenario <- itemAt (menuScenarioIndex menu) scenarios
    firstOption <- itemAt (menuAgentOneIndex menu) agentOptions
    secondOption <- itemAt (menuAgentTwoIndex menu) agentOptions

    case map wormId (scenarioWorms scenario) of
        firstId : secondId : _ ->
            let agents =
                    [ makeGuiAgent firstId (makeColorI 50 140 255 255) firstOption
                    , makeGuiAgent secondId (makeColorI 255 130 60 255) secondOption
                    ]
            in Just (AppWatch menu (initialGuiWorld agents (scenarioInitialState scenario)))

        _ -> Nothing


-- | Creates Play vs Agent mode from the current setup selections.
--
-- The first scenario worm is controlled by the human and the second by the
-- selected AI opponent.
startPlay :: [AgentOption] -> [Scenario] -> MenuWorld -> Maybe AppWorld
startPlay agentOptions scenarios menu = do
    scenario <- itemAt (menuScenarioIndex menu) scenarios
    opponentOption <- itemAt (menuPlayOpponentIndex menu) agentOptions

    case map wormId (scenarioWorms scenario) of
        humanId : opponentId : _ ->
            Just $
                AppPlay menu $
                    initialPlayWorld
                        humanId
                        opponentId
                        (agentOptionName opponentOption)
                        (agentOptionController opponentOption)
                        (scenarioInitialState scenario)

        _ -> Nothing


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

-- | Changes the currently selected Watch Agents value by the given offset.
changeWatchValueBy :: Int -> Int -> Int -> MenuWorld -> MenuWorld
changeWatchValueBy change scenarioCount agentCount menu =
    case menuWatchField menu of
        WatchMapField ->
            menu {menuScenarioIndex = cycleIndex scenarioCount change (menuScenarioIndex menu)}

        WatchAgentOneField ->
            menu {menuAgentOneIndex = cycleIndex agentCount change (menuAgentOneIndex menu)}

        WatchAgentTwoField ->
            menu {menuAgentTwoIndex = cycleIndex agentCount change (menuAgentTwoIndex menu)}

        _ -> menu


-- | Handles keyboard input on the Watch Agents setup screen.
handleWatchSetupEvent :: [AgentOption] -> [Scenario] -> Event -> MenuWorld -> IO AppWorld
handleWatchSetupEvent _ _ (EventKey (SpecialKey KeyUp) Down _ _) menu =
    pure $ AppMenu menu {menuWatchField = previousEnum (menuWatchField menu)}

handleWatchSetupEvent _ _ (EventKey (SpecialKey KeyDown) Down _ _) menu =
    pure $ AppMenu menu {menuWatchField = nextEnum (menuWatchField menu)}

handleWatchSetupEvent agentOptions scenarios (EventKey (SpecialKey KeyLeft) Down _ _) menu =
    pure $ AppMenu $ changeWatchValueBy (-1) (length scenarios) (length agentOptions) menu

handleWatchSetupEvent agentOptions scenarios (EventKey (SpecialKey KeyRight) Down _ _) menu =
    pure $ AppMenu $ changeWatchValueBy 1 (length scenarios) (length agentOptions) menu

handleWatchSetupEvent agentOptions scenarios (EventKey (SpecialKey KeyEnter) Down _ _) menu =
    case menuWatchField menu of
        WatchStartField ->
            case startWatch agentOptions scenarios menu of
                Just world -> pure world
                Nothing -> pure (AppMenu menu)

        WatchBackField ->
            pure $ AppMenu menu {menuScreen = MainMenuScreen}

        _ -> pure (AppMenu menu)

handleWatchSetupEvent _ _ (EventKey (SpecialKey KeyEsc) Down _ _) menu =
    pure $ AppMenu menu {menuScreen = MainMenuScreen}

handleWatchSetupEvent _ _ _ menu =
    pure (AppMenu menu)


-- -----------------------------------------------------------------------------
-- Play setup input
-- -----------------------------------------------------------------------------

-- | Changes the currently selected Play vs Agent value by the given offset.
changePlayValueBy :: Int -> Int -> Int -> MenuWorld -> MenuWorld
changePlayValueBy change scenarioCount agentCount menu =
    case menuPlayField menu of
        PlayMapField ->
            menu {menuScenarioIndex = cycleIndex scenarioCount change (menuScenarioIndex menu)}

        PlayOpponentField ->
            menu {menuPlayOpponentIndex = cycleIndex agentCount change (menuPlayOpponentIndex menu)}

        _ -> menu


-- | Handles keyboard input on the Play vs Agent setup screen.
handlePlaySetupEvent :: [AgentOption] -> [Scenario] -> Event -> MenuWorld -> IO AppWorld
handlePlaySetupEvent _ _ (EventKey (SpecialKey KeyUp) Down _ _) menu =
    pure $ AppMenu menu {menuPlayField = previousEnum (menuPlayField menu)}

handlePlaySetupEvent _ _ (EventKey (SpecialKey KeyDown) Down _ _) menu =
    pure $ AppMenu menu {menuPlayField = nextEnum (menuPlayField menu)}

handlePlaySetupEvent agentOptions scenarios (EventKey (SpecialKey KeyLeft) Down _ _) menu =
    pure $ AppMenu $ changePlayValueBy (-1) (length scenarios) (length agentOptions) menu

handlePlaySetupEvent agentOptions scenarios (EventKey (SpecialKey KeyRight) Down _ _) menu =
    pure $ AppMenu $ changePlayValueBy 1 (length scenarios) (length agentOptions) menu

handlePlaySetupEvent agentOptions scenarios (EventKey (SpecialKey KeyEnter) Down _ _) menu =
    case menuPlayField menu of
        PlayStartField ->
            case startPlay agentOptions scenarios menu of
                Just world -> pure world
                Nothing -> pure (AppMenu menu)

        PlayBackField ->
            pure $ AppMenu menu {menuScreen = MainMenuScreen}

        _ -> pure (AppMenu menu)

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
        MainMenuScreen -> handleMainMenuEvent event menu
        WatchSetupScreen -> handleWatchSetupEvent agentOptions scenarios event menu
        PlaySetupScreen -> handlePlaySetupEvent agentOptions scenarios event menu

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
-- Menu drawing
-- -----------------------------------------------------------------------------

-- | Draws one selectable row of the main menu.
drawMenuRow :: Bool -> Float -> String -> Picture
drawMenuRow selected y contents =
    drawMenuOptionRow (-290) y 580 selected contents


-- | Draws the main CerviQ menu.
drawMainMenu :: MenuWorld -> Picture
drawMainMenu menu =
    pictures
        [ drawPanel (-360) 275 720 470
        , drawTitle (-285) 205 "CerviQ" "Q-learning worm arena"
        , drawMenuRow (menuMainSelection menu == 0) 95 "Watch Agents"
        , drawMenuRow (menuMainSelection menu == 1) 40 "Play vs Agent"
        , drawMenuRow (menuMainSelection menu == 2) (-15) "Quit"
        , drawFooter (-285) (-150) ["UP/DOWN select", "ENTER confirm"]
        ]


-- | Draws the Watch Agents setup screen.
drawWatchSetup :: [AgentOption] -> [Scenario] -> MenuWorld -> Picture
drawWatchSetup agentOptions scenarios menu =
    pictures
        [ drawPanel (-395) 325 790 620
        , drawTitle (-325) 255 "Watch Agents" "Compare two controllers and inspect their decisions."
        , drawSettingRow (-325) 150 650 (menuWatchField menu == WatchMapField) "Map" scenarioName'
        , drawSettingRow (-325) 72 650 (menuWatchField menu == WatchAgentOneField) "Worm 1" firstAgentName
        , drawSettingRow (-325) (-6) 650 (menuWatchField menu == WatchAgentTwoField) "Worm 2" secondAgentName
        , drawActionRow (-325) (-108) 650 (menuWatchField menu == WatchStartField) "Start"
        , drawActionRow (-325) (-160) 650 (menuWatchField menu == WatchBackField) "Back"
        , drawFooter (-325) (-245) ["UP/DOWN field", "LEFT/RIGHT value", "ENTER confirm", "ESC back"]
        ]
  where
    scenarioName' = selectedScenarioName scenarios menu
    firstAgentName = selectedAgentName agentOptions (menuAgentOneIndex menu)
    secondAgentName = selectedAgentName agentOptions (menuAgentTwoIndex menu)


-- | Draws the Play vs Agent setup screen.
drawPlaySetup :: [AgentOption] -> [Scenario] -> MenuWorld -> Picture
drawPlaySetup agentOptions scenarios menu =
    pictures
        [ drawPanel (-395) 325 790 620
        , drawTitle (-325) 255 "Play vs Agent" "Control Worm 1 with global arrow movement."
        , drawSettingRow (-325) 145 650 (menuPlayField menu == PlayMapField) "Map" scenarioName'
        , drawSettingRow (-325) 67 650 (menuPlayField menu == PlayOpponentField) "Opponent" opponentName
        , drawActionRow (-325) (-34) 650 (menuPlayField menu == PlayStartField) "Start"
        , drawActionRow (-325) (-86) 650 (menuPlayField menu == PlayBackField) "Back"
        , drawFooter (-325) (-160) ["In game: ARROWS move", "SPACE pause", "R restart", "+/- speed"]
        , drawFooter (-325) (-245) ["UP/DOWN field", "LEFT/RIGHT value", "ENTER confirm", "ESC back"]
        ]
  where
    scenarioName' = selectedScenarioName scenarios menu
    opponentName = selectedAgentName agentOptions (menuPlayOpponentIndex menu)


-- | Draws the currently active menu screen.
drawMenuWorld :: [AgentOption] -> [Scenario] -> MenuWorld -> Picture
drawMenuWorld agentOptions scenarios menu =
    case menuScreen menu of
        MainMenuScreen -> drawMainMenu menu
        WatchSetupScreen -> drawWatchSetup agentOptions scenarios menu
        PlaySetupScreen -> drawPlaySetup agentOptions scenarios menu


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