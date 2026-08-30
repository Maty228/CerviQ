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

-- | Screen currently displayed by the application menu.
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


-- | Selected field of the Play setup screen.
data PlaySetupField
    = PlayMapField
    | PlayOpponentField
    | PlayStartField
    | PlayBackField
    deriving (Show, Eq, Enum, Bounded)


-- | Complete state of the application menu.
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


-- | Complete state of the CerviQ application.
data AppWorld
    = AppMenu MenuWorld
    | AppWatch MenuWorld GuiWorld
    | AppPlay MenuWorld PlayWorld


-- -----------------------------------------------------------------------------
-- Initial state
-- -----------------------------------------------------------------------------

-- | Returns the index of an agent with the given name.
agentIndexByName :: String -> [AgentOption] -> Int
agentIndexByName targetName options =
    case findIndex ((== targetName) . agentOptionName) options of
        Just index -> index
        Nothing -> 0


-- | Creates the initial application menu state.
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

-- | Moves an index cyclically through a list of the given length.
cycleIndex :: Int -> Int -> Int -> Int
cycleIndex itemCount change currentIndex
    | itemCount <= 0 = 0
    | otherwise = (currentIndex + change) `mod` itemCount


-- | Moves to the previous value of a bounded enumeration.
previousEnum :: (Eq value, Enum value, Bounded value) => value -> value
previousEnum value
    | value == minBound = maxBound
    | otherwise = pred value


-- | Moves to the next value of a bounded enumeration.
nextEnum :: (Eq value, Enum value, Bounded value) => value -> value
nextEnum value
    | value == maxBound = minBound
    | otherwise = succ value


-- | Safely returns one item from a list.
itemAt :: Int -> [value] -> Maybe value
itemAt index values
    | index < 0 = Nothing
    | index >= length values = Nothing
    | otherwise = Just (values !! index)


-- -----------------------------------------------------------------------------
-- Watch setup
-- -----------------------------------------------------------------------------

-- | Creates a debugger world from the current Watch Agents menu selection.
startWatch :: [AgentOption] -> [Scenario] -> MenuWorld -> Maybe AppWorld
startWatch agentOptions scenarios menu = do
    scenario <- itemAt (menuScenarioIndex menu) scenarios
    firstOption <- itemAt (menuAgentOneIndex menu) agentOptions
    secondOption <- itemAt (menuAgentTwoIndex menu) agentOptions

    case map wormId (scenarioWorms scenario) of
        firstId : secondId : _ ->
            let
                agents =
                    [
                        makeGuiAgent firstId (makeColorI 50 140 255 255) firstOption,
                        makeGuiAgent secondId (makeColorI 255 130 60 255) secondOption
                    ]

                initialState = scenarioInitialState scenario
            in
                Just ( AppWatch menu (initialGuiWorld agents initialState) )

        _ -> Nothing

-- -----------------------------------------------------------------------------
-- Play setup
-- -----------------------------------------------------------------------------

-- | Creates a human-versus-agent world from the current Play menu selection.
startPlay :: [AgentOption] -> [Scenario] -> MenuWorld -> Maybe AppWorld
startPlay agentOptions scenarios menu = do
    scenario <- itemAt (menuScenarioIndex menu) scenarios
    opponentOption <- itemAt (menuPlayOpponentIndex menu) agentOptions

    case map wormId (scenarioWorms scenario) of
        humanId : opponentId : _ ->
            Just
                ( AppPlay
                    menu
                    ( initialPlayWorld
                        humanId
                        opponentId
                        (agentOptionName opponentOption)
                        (agentOptionController opponentOption)
                        (scenarioInitialState scenario)
                    )
                )

        _ -> Nothing

-- -----------------------------------------------------------------------------
-- Main menu input
-- -----------------------------------------------------------------------------

-- | Handles input on the main menu.
handleMainMenuEvent :: Event -> MenuWorld -> IO AppWorld
handleMainMenuEvent (EventKey (SpecialKey KeyUp) Down _ _) menu =
    pure
        ( AppMenu menu { menuMainSelection = cycleIndex 3 (-1) (menuMainSelection menu) } )

handleMainMenuEvent (EventKey (SpecialKey KeyDown) Down _ _) menu =
    pure ( AppMenu menu { menuMainSelection = cycleIndex 3 1 (menuMainSelection menu) } )

handleMainMenuEvent (EventKey (SpecialKey KeyEnter) Down _ _) menu =
    case menuMainSelection menu of
        0 -> pure ( AppMenu menu { menuScreen = WatchSetupScreen } )
        1 -> pure ( AppMenu menu { menuScreen = PlaySetupScreen } )
        _ -> do exitSuccess

handleMainMenuEvent _ menu = pure (AppMenu menu)


-- -----------------------------------------------------------------------------
-- Watch setup input
-- -----------------------------------------------------------------------------

-- | Changes the selected Watch Agents configuration value.
changeWatchValue :: Int -> Int -> MenuWorld -> MenuWorld
changeWatchValue scenarioCount agentCount menu =
    case menuWatchField menu of
        WatchMapField -> menu { menuScenarioIndex = cycleIndex scenarioCount 1 (menuScenarioIndex menu) }
        WatchAgentOneField -> menu { menuAgentOneIndex = cycleIndex agentCount 1 (menuAgentOneIndex menu) }
        WatchAgentTwoField -> menu { menuAgentTwoIndex = cycleIndex agentCount 1 (menuAgentTwoIndex menu) }
        _ -> menu


-- | Changes the selected Watch Agents configuration value backwards.
changeWatchValueBackward :: Int -> Int -> MenuWorld -> MenuWorld
changeWatchValueBackward scenarioCount agentCount menu =
    case menuWatchField menu of
        WatchMapField -> menu { menuScenarioIndex = cycleIndex scenarioCount (-1) (menuScenarioIndex menu) }
        WatchAgentOneField -> menu { menuAgentOneIndex = cycleIndex agentCount (-1) (menuAgentOneIndex menu) }
        WatchAgentTwoField -> menu { menuAgentTwoIndex = cycleIndex agentCount (-1) (menuAgentTwoIndex menu) }
        _ -> menu


-- | Handles input on the Watch Agents setup screen.
handleWatchSetupEvent :: [AgentOption] -> [Scenario] -> Event -> MenuWorld -> IO AppWorld
handleWatchSetupEvent _ _ (EventKey (SpecialKey KeyUp) Down _ _) menu =
    pure ( AppMenu menu { menuWatchField = previousEnum (menuWatchField menu) } )

handleWatchSetupEvent _ _ (EventKey (SpecialKey KeyDown) Down _ _) menu =
    pure ( AppMenu menu { menuWatchField = nextEnum (menuWatchField menu) } )

handleWatchSetupEvent agentOptions scenarios (EventKey (SpecialKey KeyLeft) Down _ _) menu =
    pure ( AppMenu ( changeWatchValueBackward (length scenarios) (length agentOptions) menu ) )

handleWatchSetupEvent agentOptions scenarios (EventKey (SpecialKey KeyRight) Down _ _) menu =
    pure ( AppMenu ( changeWatchValue (length scenarios) (length agentOptions) menu ) )

handleWatchSetupEvent agentOptions scenarios (EventKey (SpecialKey KeyEnter) Down _ _) menu =
    case menuWatchField menu of
        WatchStartField ->
            case startWatch agentOptions scenarios menu of
                Just world -> pure world
                Nothing -> pure (AppMenu menu)

        WatchBackField -> pure ( AppMenu menu { menuScreen = MainMenuScreen } )
        _ -> pure (AppMenu menu)

handleWatchSetupEvent _ _ (EventKey (SpecialKey KeyEsc) Down _ _) menu =
    pure ( AppMenu menu { menuScreen = MainMenuScreen } )

handleWatchSetupEvent _ _ _ menu =
    pure (AppMenu menu)


-- -----------------------------------------------------------------------------
-- Play setup input
-- -----------------------------------------------------------------------------

-- | Handles the preliminary Play setup screen.
--
-- Actual human-controlled gameplay will be connected in the next implementation
-- step.
handlePlaySetupEvent :: [AgentOption] -> [Scenario] -> Event -> MenuWorld -> IO AppWorld
handlePlaySetupEvent _ _ (EventKey (SpecialKey KeyUp) Down _ _) menu =
    pure ( AppMenu menu { menuPlayField = previousEnum (menuPlayField menu) } )

handlePlaySetupEvent _ _ (EventKey (SpecialKey KeyDown) Down _ _) menu =
    pure ( AppMenu menu { menuPlayField = nextEnum (menuPlayField menu) } )

handlePlaySetupEvent agentOptions scenarios (EventKey (SpecialKey KeyLeft) Down _ _) menu =
    pure
        ( AppMenu
            ( case menuPlayField menu of
                PlayMapField -> menu { menuScenarioIndex = cycleIndex (length scenarios) (-1) (menuScenarioIndex menu) }
                PlayOpponentField -> menu { menuPlayOpponentIndex = cycleIndex (length agentOptions) (-1) (menuPlayOpponentIndex menu) }
                _ -> menu
            )
        )


handlePlaySetupEvent agentOptions scenarios (EventKey (SpecialKey KeyRight) Down _ _) menu =
    pure
        ( AppMenu
            ( case menuPlayField menu of
                PlayMapField -> menu { menuScenarioIndex = cycleIndex (length scenarios) 1 (menuScenarioIndex menu) }
                PlayOpponentField -> menu { menuPlayOpponentIndex = cycleIndex (length agentOptions) 1 (menuPlayOpponentIndex menu) }
                _ -> menu
            )
        )

handlePlaySetupEvent agentOptions scenarios (EventKey (SpecialKey KeyEnter) Down _ _) menu =
    case menuPlayField menu of
        PlayStartField ->
            case startPlay agentOptions scenarios menu of
                Just world -> pure world
                Nothing -> pure (AppMenu menu)

        PlayBackField ->
            pure ( AppMenu menu { menuScreen = MainMenuScreen } )

        _ ->
            pure (AppMenu menu)

handlePlaySetupEvent _ _ (EventKey (SpecialKey KeyEsc) Down _ _) menu =
    pure ( AppMenu menu { menuScreen = MainMenuScreen } )

handlePlaySetupEvent _ _ _ menu =
    pure (AppMenu menu)




-- -----------------------------------------------------------------------------
-- Application input
-- -----------------------------------------------------------------------------

-- | Handles keyboard input for the complete application.
handleAppEvent :: [AgentOption] -> [Scenario] -> Event -> AppWorld -> IO AppWorld
handleAppEvent agentOptions scenarios event (AppMenu menu) =
    case menuScreen menu of
        MainMenuScreen -> handleMainMenuEvent event menu

        WatchSetupScreen ->
            handleWatchSetupEvent agentOptions scenarios event menu

        PlaySetupScreen ->
            handlePlaySetupEvent agentOptions scenarios event menu

handleAppEvent _ _ (EventKey (SpecialKey KeyEsc) Down _ _) (AppWatch menu _) =
    pure ( AppMenu menu { menuScreen = WatchSetupScreen } )

handleAppEvent _ _ event (AppWatch menu guiWorld) = do
    updatedGuiWorld <- handleGuiEvent event guiWorld

    pure (AppWatch menu updatedGuiWorld)

handleAppEvent _ _ (EventKey (SpecialKey KeyEsc) Down _ _) (AppPlay menu _) =
    pure ( AppMenu menu { menuScreen = PlaySetupScreen } )

handleAppEvent _ _ event (AppPlay menu playWorld) = do
    updatedPlayWorld <- handlePlayEvent event playWorld

    pure (AppPlay menu updatedPlayWorld)

-- -----------------------------------------------------------------------------
-- Menu drawing
-- -----------------------------------------------------------------------------

-- | Draws one row of a menu.
drawMenuRow :: Bool -> Float -> String -> Picture
drawMenuRow selected y contents =
    drawMenuOptionRow (-290) y 580 selected contents


-- | Draws the main CerviQ menu.
drawMainMenu :: MenuWorld -> Picture
drawMainMenu menu =
    pictures
        [
            drawPanel (-360) 275 720 470,
            drawTitle (-285) 205 "CerviQ" "Q-learning worm arena",
            drawMenuRow (menuMainSelection menu == 0) 95 "Watch Agents",
            drawMenuRow (menuMainSelection menu == 1) 40 "Play vs Agent",
            drawMenuRow (menuMainSelection menu == 2) (-15) "Quit",
            drawFooter (-285) (-150) ["UP/DOWN select", "ENTER confirm"]
        ]


-- | Draws the Watch Agents setup screen.
drawWatchSetup :: [AgentOption] -> [Scenario] -> MenuWorld -> Picture
drawWatchSetup agentOptions scenarios menu =
    pictures
        [
            drawPanel (-395) 325 790 620,
            drawTitle (-325) 255 "Watch Agents" "Compare two controllers and inspect their decisions.",
            drawSettingRow (-325) 150 650 (menuWatchField menu == WatchMapField) "Map" selectedScenarioName,
            drawSettingRow (-325) 72 650 (menuWatchField menu == WatchAgentOneField) "Worm 1" firstAgentName,
            drawSettingRow (-325) (-6) 650 (menuWatchField menu == WatchAgentTwoField) "Worm 2" secondAgentName,
            drawActionRow (-325) (-108) 650 (menuWatchField menu == WatchStartField) "Start",
            drawActionRow (-325) (-160) 650 (menuWatchField menu == WatchBackField) "Back",
            drawFooter (-325) (-245) ["UP/DOWN field", "LEFT/RIGHT value", "ENTER confirm", "ESC back"]
        ]
  where
    selectedScenarioName =
        case itemAt (menuScenarioIndex menu) scenarios of
            Just scenario -> scenarioName scenario
            Nothing -> "No scenario"

    firstAgentName =
        case itemAt (menuAgentOneIndex menu) agentOptions of
            Just option -> agentOptionName option
            Nothing -> "No agent"

    secondAgentName =
        case itemAt (menuAgentTwoIndex menu) agentOptions of
            Just option -> agentOptionName option
            Nothing -> "No agent"


-- | Draws the Play setup screen.
drawPlaySetup :: [AgentOption] -> [Scenario] -> MenuWorld -> Picture
drawPlaySetup agentOptions scenarios menu =
    pictures
        [
            drawPanel (-395) 325 790 620,
            drawTitle (-325) 255 "Play vs Agent" "Control Worm 1 with global arrow movement.",
            drawSettingRow (-325) 145 650 (menuPlayField menu == PlayMapField) "Map" selectedScenarioName,
            drawSettingRow (-325) 67 650 (menuPlayField menu == PlayOpponentField) "Opponent" selectedOpponentName,
            drawActionRow (-325) (-34) 650 (menuPlayField menu == PlayStartField) "Start",
            drawActionRow (-325) (-86) 650 (menuPlayField menu == PlayBackField) "Back",
            drawFooter (-325) (-160) ["In game: ARROWS move", "SPACE pause", "R restart", "+/- speed"],
            drawFooter (-325) (-245) ["UP/DOWN field", "LEFT/RIGHT value", "ENTER confirm", "ESC back"]
        ]
  where
    selectedScenarioName =
        case itemAt (menuScenarioIndex menu) scenarios of
            Just scenario -> scenarioName scenario
            Nothing -> "No scenario"

    selectedOpponentName =
        case itemAt (menuPlayOpponentIndex menu) agentOptions of
            Just option -> agentOptionName option
            Nothing -> "No agent"


-- | Draws the current application menu screen.
drawMenuWorld :: [AgentOption] -> [Scenario] -> MenuWorld -> Picture
drawMenuWorld agentOptions scenarios menu =
    case menuScreen menu of
        MainMenuScreen -> drawMainMenu menu
        WatchSetupScreen -> drawWatchSetup agentOptions scenarios menu
        PlaySetupScreen -> drawPlaySetup agentOptions scenarios menu


-- -----------------------------------------------------------------------------
-- Application drawing and updating
-- -----------------------------------------------------------------------------

-- | Draws the complete CerviQ application.
drawAppWorld :: [AgentOption] -> [Scenario] -> AppWorld -> IO Picture
drawAppWorld agentOptions scenarios (AppMenu menu) =
    pure (drawMenuWorld agentOptions scenarios menu)

drawAppWorld _ _ (AppWatch _ guiWorld) =
    drawGuiWorld guiWorld

drawAppWorld _ _ (AppPlay _ playWorld) =
    drawPlayWorld playWorld


-- | Updates the currently active part of the application.
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

-- | Starts the complete CerviQ graphical application.
runCerviQ :: IO ()
runCerviQ = do
    agentOptions <- loadAgentOptions

    let scenarios = allScenarios

    playIO
        guiDisplay
        black
        60
        (AppMenu (initialMenuWorld agentOptions))
        (drawAppWorld agentOptions scenarios)
        (handleAppEvent agentOptions scenarios)
        updateAppWorld
