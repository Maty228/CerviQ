{-|
Module      : Main
Description : Automated unit tests for the core CerviQ game and Q-learning logic.

The suite focuses on deterministic behaviour that can be verified independently
of the graphical interface and stochastic agent performance. Integration and
performance evaluation are handled separately by CerviQ's evaluation tools.
-}

module Main (main) where

import Collision
import Maps
import Movement
import Types
import Game
import Evaluation
import PlayGui
import Scenario
import Scenarios

import qualified QLearning.Core as Core

import Test.Tasty
import Test.Tasty.HUnit


-- -----------------------------------------------------------------------------
-- Test fixtures
-- -----------------------------------------------------------------------------

-- | Empty statistics used by deterministic test worms.
testStats :: WormStats
testStats =
    WormStats
        { foodEaten = 0
        , kills = 0
        , age = 0
        }


-- | Creates a living worm suitable for unit-test scenarios.
testWorm :: Int -> [Position] -> Direction -> Worm
testWorm targetId body direction =
    Worm
        { wormId = targetId
        , wormBody = body
        , wormDirection = direction
        , wormAlive = True
        , wormStats = testStats
        }


-- | Open five-by-five map used by collision tests that do not need obstacles.
openMap :: GameMap
openMap =
    fromAsciiMap
        [ "....."
        , "....."
        , "....."
        , "....."
        , "....."
        ]


-- | Returns the death reason recorded for one worm identifier.
deathReasonFor :: Int -> [(Worm, DeathReason)] -> Maybe DeathReason
deathReasonFor targetId deaths =
    case
        [ reason
        | (worm, reason) <- deaths
        , wormId worm == targetId
        ]
    of
        reason : _ -> Just reason
        [] -> Nothing


-- | Returns whether a death reason represents any head-to-head collision.
isHeadToHead :: Maybe DeathReason -> Bool
isHeadToHead (Just (HeadToHead _)) = True
isHeadToHead _ = False


-- | Asserts approximate equality of two floating-point values.
assertApproxEqual :: String -> Double -> Double -> Assertion
assertApproxEqual message expected actual =
    assertBool
        ( message
            ++ ": expected "
            ++ show expected
            ++ ", got "
            ++ show actual
        )
        (abs (expected - actual) < 1e-9)


-- | Finds a worm by its identifier in a game state.
wormById :: Int -> GameState -> Maybe Worm
wormById targetId state =
    case filter ((== targetId) . wormId) (gameWorms state) of
        worm : _ -> Just worm
        [] -> Nothing


-- | Returns the head of one worm from a game state.
wormHeadById :: Int -> GameState -> Maybe Position
wormHeadById targetId state =
    case wormById targetId state of
        Just worm ->
            case wormBody worm of
                headPosition : _ -> Just headPosition
                [] -> Nothing

        Nothing ->
            Nothing


-- | Returns the number of kills of one worm in a game state.
wormKillsById :: Int -> GameState -> Int
wormKillsById targetId state =
    case wormById targetId state of
        Just worm -> kills (wormStats worm)
        Nothing -> 0


-- | Builds a deterministic game state from a map and collection of worms.
testGameState :: GameMap -> [Worm] -> GameState
testGameState currentMap worms =
    GameState
        { gameMap = currentMap
        , gameWorms = worms
        , gameTick = 0
        , gameHeadHistory = initialHeadHistory worms
        }


-- | Marks a test worm as dead while preserving its remaining state.
deadTestWorm :: Worm -> Worm
deadTestWorm worm =
    worm {wormAlive = False}


-- -----------------------------------------------------------------------------
-- Movement tests
-- -----------------------------------------------------------------------------

-- | Unit tests for relative actions, directions, and worm body movement.
movementTests :: TestTree
movementTests =
    testGroup
        "Movement"
        [ testCase "turning left from North faces West" $
            turnLeft North @?= West

        , testCase "turning right from West faces North" $
            turnRight West @?= North

        , testCase "GoStraight preserves direction" $
            applyAction South GoStraight @?= South

        , testCase "180-degree direction change is rejected" $
            directionToAction North South @?= Nothing

        , testCase "headAfterAction applies relative turn before movement" $
            let worm = testWorm 1 [(2, 2), (2, 3)] North
            in headAfterAction worm TurnRight @?= (3, 2)

        , testCase "normal movement preserves body length" $
            advanceBody False (3, 2) [(2, 2), (1, 2), (0, 2)]
                @?= [(3, 2), (2, 2), (1, 2)]

        , testCase "growing movement preserves old tail" $
            advanceBody True (3, 2) [(2, 2), (1, 2), (0, 2)]
                @?= [(3, 2), (2, 2), (1, 2), (0, 2)]

        , testCase "moveWormAfterAction updates direction and body" $
            let worm = testWorm 1 [(2, 2), (2, 3), (2, 4)] North
                moved = moveWormAfterAction False TurnRight worm
            in do
                wormDirection moved @?= East
                wormBody moved @?= [(3, 2), (2, 2), (2, 3)]
        ]


-- -----------------------------------------------------------------------------
-- Collision tests
-- -----------------------------------------------------------------------------

-- | Unit tests for deterministic collision detection and simultaneous movement.
collisionTests :: TestTree
collisionTests =
    testGroup
        "Collision"
        [ testCase "moving outside the map causes HitWall" $
            let worm = testWorm 1 [(0, 2)] West
                (survivors, deaths) =
                    simulateTurn
                        openMap
                        [(False, GoStraight, worm)]
            in do
                survivors @?= []
                deathReasonFor 1 deaths @?= Just HitWall

        , testCase "moving onto a wall causes HitWall" $
            let wallMap =
                    fromAsciiMap
                        [ "....."
                        , "..#.."
                        , "....."
                        ]

                worm =
                    testWorm 1 [(1, 1)] East

                (survivors, deaths) =
                    simulateTurn
                        wallMap
                        [(False, GoStraight, worm)]
            in do
                survivors @?= []
                deathReasonFor 1 deaths @?= Just HitWall

        , testCase "moving onto poison causes HitPoison" $
            let poisonMap =
                    fromAsciiMap
                        [ ".P..."
                        , "....."
                        ]

                worm =
                    testWorm 1 [(0, 0)] East

                (_, deaths) =
                    simulateTurn
                        poisonMap
                        [(False, GoStraight, worm)]
            in
                deathReasonFor 1 deaths @?= Just HitPoison

        , testCase "moving into own non-vacating body causes HitOwnBody" $
            let worm =
                    testWorm
                        1
                        [(2, 2), (2, 3), (1, 3), (1, 2), (1, 1)]
                        West

                (_, deaths) =
                    simulateTurn
                        openMap
                        [(False, GoStraight, worm)]
            in
                deathReasonFor 1 deaths @?= Just HitOwnBody

        , testCase "two worms moving to the same head position both die" $
            let wormOne =
                    testWorm 1 [(1, 2)] East

                wormTwo =
                    testWorm 2 [(3, 2)] West

                (survivors, deaths) =
                    simulateTurn
                        openMap
                        [ (False, GoStraight, wormOne)
                        , (False, GoStraight, wormTwo)
                        ]
            in do
                survivors @?= []
                assertBool
                    "Worm 1 should die head-to-head"
                    (isHeadToHead (deathReasonFor 1 deaths))
                assertBool
                    "Worm 2 should die head-to-head"
                    (isHeadToHead (deathReasonFor 2 deaths))

        , testCase "three-way head-to-head collision kills all three worms" $
            let wormOne =
                    testWorm 1 [(1, 2)] East

                wormTwo =
                    testWorm 2 [(3, 2)] West

                wormThree =
                    testWorm 3 [(2, 1)] South

                (survivors, deaths) =
                    simulateTurn
                        openMap
                        [ (False, GoStraight, wormOne)
                        , (False, GoStraight, wormTwo)
                        , (False, GoStraight, wormThree)
                        ]
            in do
                survivors @?= []
                length deaths @?= 3
                assertBool
                    "Worm 1 should die head-to-head"
                    (isHeadToHead (deathReasonFor 1 deaths))
                assertBool
                    "Worm 2 should die head-to-head"
                    (isHeadToHead (deathReasonFor 2 deaths))
                assertBool
                    "Worm 3 should die head-to-head"
                    (isHeadToHead (deathReasonFor 3 deaths))

        , testCase "head inside another worm body is attributed to that worm" $
            let attacker =
                    testWorm 1 [(2, 2)] East

                bodyOwner =
                    testWorm 2 [(4, 2), (3, 2), (2, 2)] East

                reason =
                    fmap snd
                        ( deathReason
                            openMap
                            [attacker, bodyOwner]
                            attacker
                        )
            in
                reason @?= Just (HitOtherBody 2)

        , testCase "unrelated worm survives another pair's head-to-head collision" $
            let wormOne =
                    testWorm 1 [(1, 2)] East

                wormTwo =
                    testWorm 2 [(3, 2)] West

                wormThree =
                    testWorm 3 [(0, 4)] East

                (survivors, deaths) =
                    simulateTurn
                        openMap
                        [ (False, GoStraight, wormOne)
                        , (False, GoStraight, wormTwo)
                        , (False, GoStraight, wormThree)
                        ]
            in do
                map wormId survivors @?= [3]
                length deaths @?= 2
        ]


-- -----------------------------------------------------------------------------
-- Game tests
-- -----------------------------------------------------------------------------

-- | Unit tests for complete one-tick game-state updates.
gameTests :: TestTree
gameTests =
    testGroup
        "Game"
        [ testCase "missing action defaults to GoStraight" $
            let worm =
                    testWorm
                        1
                        [(2, 2), (1, 2)]
                        East
            in
                actionForWorm [] worm @?= GoStraight

        , testCase "explicit action is selected by worm ID" $
            let worm =
                    testWorm
                        3
                        [(2, 2), (1, 2)]
                        East
            in
                actionForWorm
                    [(1, TurnLeft), (3, TurnRight)]
                    worm
                    @?= TurnRight

        , testCase "four living worms move in one simultaneous tick" $
            let wormOne =
                    testWorm 1 [(1, 1)] East

                wormTwo =
                    testWorm 2 [(3, 1)] East

                wormThree =
                    testWorm 3 [(1, 3)] East

                wormFour =
                    testWorm 4 [(3, 3)] East

                initialState =
                    GameState
                        { gameMap = openMap
                        , gameWorms =
                            [ wormOne
                            , wormTwo
                            , wormThree
                            , wormFour
                            ]
                        , gameTick = 0
                        , gameHeadHistory =
                            initialHeadHistory
                                [ wormOne
                                , wormTwo
                                , wormThree
                                , wormFour
                                ]
                        }

                result =
                    stepGameDetailed
                        [ (1, GoStraight)
                        , (2, GoStraight)
                        , (3, GoStraight)
                        , (4, GoStraight)
                        ]
                        initialState

                finalState =
                    gameStepState result
            in do
                gameTick finalState @?= 1
                wormHeadById 1 finalState @?= Just (2, 1)
                wormHeadById 2 finalState @?= Just (4, 1)
                wormHeadById 3 finalState @?= Just (2, 3)
                wormHeadById 4 finalState @?= Just (4, 3)

        , testCase "worm without supplied action still moves straight" $
            let wormOne =
                    testWorm 1 [(1, 1)] East

                wormTwo =
                    testWorm 2 [(1, 3)] East

                initialState =
                    GameState
                        { gameMap = openMap
                        , gameWorms = [wormOne, wormTwo]
                        , gameTick = 0
                        , gameHeadHistory =
                            initialHeadHistory [wormOne, wormTwo]
                        }

                result =
                    stepGameDetailed
                        [(1, TurnRight)]
                        initialState

                finalState =
                    gameStepState result
            in do
                wormHeadById 1 finalState @?= Just (1, 2)
                wormHeadById 2 finalState @?= Just (2, 3)

        , testCase "eating food grows worm and increments food statistic" $
            let foodMap =
                    placeFood
                        (2, 1)
                        openMap

                worm =
                    testWorm
                        1
                        [(1, 1), (0, 1)]
                        East

                initialState =
                    GameState
                        { gameMap = foodMap
                        , gameWorms = [worm]
                        , gameTick = 0
                        , gameHeadHistory =
                            initialHeadHistory [worm]
                        }

                result =
                    stepGameDetailed
                        [(1, GoStraight)]
                        initialState

                finalState =
                    gameStepState result
            in
                case wormById 1 finalState of
                    Just finalWorm -> do
                        length (wormBody finalWorm) @?= 3
                        foodEaten (wormStats finalWorm) @?= 1
                        isFood (gameMap finalState) (2, 1) @?= False

                    Nothing ->
                        assertFailure "Worm 1 disappeared unexpectedly."

        , testCase "ordinary movement preserves worm length" $
            let worm =
                    testWorm
                        1
                        [(1, 1), (0, 1)]
                        East

                initialState =
                    GameState
                        { gameMap = openMap
                        , gameWorms = [worm]
                        , gameTick = 0
                        , gameHeadHistory =
                            initialHeadHistory [worm]
                        }

                finalState =
                    gameStepState
                        ( stepGameDetailed
                            [(1, GoStraight)]
                            initialState
                        )
            in
                case wormById 1 finalState of
                    Just finalWorm ->
                        length (wormBody finalWorm) @?= 2

                    Nothing ->
                        assertFailure "Worm 1 disappeared unexpectedly."

        , testCase "body collision credits kill to body owner" $
            let attacker =
                    testWorm
                        1
                        [(1, 2)]
                        East

                defender =
                    testWorm
                        2
                        [(3, 1), (2, 1), (2, 2), (2, 3)]
                        East

                initialState =
                    GameState
                        { gameMap = openMap
                        , gameWorms = [attacker, defender]
                        , gameTick = 0
                        , gameHeadHistory =
                            initialHeadHistory [attacker, defender]
                        }

                result =
                    stepGameDetailed
                        [ (1, GoStraight)
                        , (2, GoStraight)
                        ]
                        initialState

                finalState =
                    gameStepState result
            in do
                lookup 1 (gameStepDeaths result)
                    @?= Just (HitOtherBody 2)

                wormKillsById 2 finalState
                    @?= 1

        , testCase "moving into a tail that vacates this tick is allowed" $
            let attacker =
                    testWorm
                        1
                        [(1, 2)]
                        East

                defender =
                    testWorm
                        2
                        [(3, 1), (2, 1), (2, 2)]
                        East

                initialState =
                    GameState
                        { gameMap = openMap
                        , gameWorms = [attacker, defender]
                        , gameTick = 0
                        , gameHeadHistory =
                            initialHeadHistory [attacker, defender]
                        }

                result =
                    stepGameDetailed
                        [ (1, GoStraight)
                        , (2, GoStraight)
                        ]
                        initialState

                finalState =
                    gameStepState result
            in do
                lookup 1 (gameStepDeaths result) @?= Nothing
                wormHeadById 1 finalState @?= Just (2, 2)

        , testCase "head-to-head collision does not credit kills" $
            let wormOne =
                    testWorm
                        1
                        [(1, 2)]
                        East

                wormTwo =
                    testWorm
                        2
                        [(3, 2)]
                        West

                initialState =
                    GameState
                        { gameMap = openMap
                        , gameWorms = [wormOne, wormTwo]
                        , gameTick = 0
                        , gameHeadHistory =
                            initialHeadHistory [wormOne, wormTwo]
                        }

                finalState =
                    gameStepState
                        ( stepGameDetailed
                            [ (1, GoStraight)
                            , (2, GoStraight)
                            ]
                            initialState
                        )
            in do
                wormKillsById 1 finalState @?= 0
                wormKillsById 2 finalState @?= 0

        , testCase "unrelated third worm survives another pair's collision" $
            let wormOne =
                    testWorm
                        1
                        [(1, 2)]
                        East

                wormTwo =
                    testWorm
                        2
                        [(3, 2)]
                        West

                wormThree =
                    testWorm
                        3
                        [(1, 4)]
                        East

                initialState =
                    GameState
                        { gameMap = openMap
                        , gameWorms =
                            [ wormOne
                            , wormTwo
                            , wormThree
                            ]
                        , gameTick = 0
                        , gameHeadHistory =
                            initialHeadHistory
                                [ wormOne
                                , wormTwo
                                , wormThree
                                ]
                        }

                finalState =
                    gameStepState
                        ( stepGameDetailed
                            [ (1, GoStraight)
                            , (2, GoStraight)
                            , (3, GoStraight)
                            ]
                            initialState
                        )
            in
                case wormById 3 finalState of
                    Just worm ->
                        wormAlive worm @?= True

                    Nothing ->
                        assertFailure "Worm 3 disappeared unexpectedly."
        ]

-- -----------------------------------------------------------------------------
-- Evaluation tests
-- -----------------------------------------------------------------------------

-- | Unit tests for deterministic evaluation bookkeeping.
evaluationTests :: TestTree
evaluationTests =
    testGroup
        "Evaluation"
        [ testCase "lastStandingWormId returns Nothing when multiple worms live" $
            let wormOne =
                    testWorm 1 [(1, 1)] East

                wormTwo =
                    testWorm 2 [(3, 1)] West

                state =
                    GameState
                        { gameMap = openMap
                        , gameWorms = [wormOne, wormTwo]
                        , gameTick = 0
                        , gameHeadHistory =
                            initialHeadHistory [wormOne, wormTwo]
                        }
            in
                lastStandingWormId state @?= Nothing

        , testCase "lastStandingWormId returns sole living worm" $
            let living =
                    testWorm 1 [(1, 1)] East

                dead =
                    (testWorm 2 [(3, 1)] West)
                        { wormAlive = False }

                state =
                    GameState
                        { gameMap = openMap
                        , gameWorms = [living, dead]
                        , gameTick = 0
                        , gameHeadHistory =
                            initialHeadHistory [living, dead]
                        }
            in
                lastStandingWormId state @?= Just 1

        , testCase "recordLastStandingWorm records sole survivor once" $
            let living =
                    testWorm 3 [(1, 1)] East

                deadOne =
                    (testWorm 1 [(2, 1)] East)
                        { wormAlive = False }

                deadTwo =
                    (testWorm 2 [(3, 1)] East)
                        { wormAlive = False }

                state =
                    GameState
                        { gameMap = openMap
                        , gameWorms = [deadOne, deadTwo, living]
                        , gameTick = 10
                        , gameHeadHistory =
                            initialHeadHistory [deadOne, deadTwo, living]
                        }
            in do
                recordLastStandingWorm state []
                    @?= [3]

                recordLastStandingWorm state [3]
                    @?= [3]
        ]


-- -----------------------------------------------------------------------------
-- Scenario tests
-- -----------------------------------------------------------------------------

-- | Unit tests for predefined scenarios and multi-worm initial-state creation.
scenarioTests :: TestTree
scenarioTests =
    testGroup
        "Scenario"
        [ testCase "all built-in scenarios are valid" $
            mapM_
                (\scenario ->
                    validateScenario scenario @?= []
                )
                allScenarios

        , testCase "default scenario states preserve historical two-worm setup" $
            map
                (length . gameWorms . scenarioInitialState)
                allScenarios
                @?= [2, 2, 2, 2]

        , testCase "Arena supports six predefined worms" $
            scenarioMaximumWormCount arenaScenario @?= 6

        , testCase "Cave supports eight predefined worms" $
            scenarioMaximumWormCount caveScenario @?= 8

        , testCase "Corridors supports six predefined worms" $
            scenarioMaximumWormCount corridorScenario @?= 6

        , testCase "Large Cave supports nine predefined worms" $
            scenarioMaximumWormCount largeCaveScenario @?= 9

        , testCase "maximum-capacity states can be constructed for every scenario" $
            mapM_
                assertMaximumState
                allScenarios

        , testCase "invalid worm counts are rejected" $ do
            scenarioInitialStateForWormCount 0 arenaScenario
                @?= Nothing

            scenarioInitialStateForWormCount (-1) arenaScenario
                @?= Nothing

            scenarioInitialStateForWormCount 7 arenaScenario
                @?= Nothing

            scenarioInitialStateForWormCount 9 caveScenario
                @?= Nothing

            scenarioInitialStateForWormCount 7 corridorScenario
                @?= Nothing

            scenarioInitialStateForWormCount 10 largeCaveScenario
                @?= Nothing

        , testCase "Large Cave nine-worm state preserves predefined worm IDs" $
            case scenarioInitialStateForWormCount 9 largeCaveScenario of
                Just state ->
                    map wormId (gameWorms state)
                        @?= [1 .. 9]

                Nothing ->
                    assertFailure
                        "Large Cave should support a nine-worm initial state."
        ]
  where
    -- | Asserts that one scenario can construct its advertised maximum worm count.
    assertMaximumState :: Scenario -> Assertion
    assertMaximumState scenario =
        case
            scenarioInitialStateForWormCount
                (scenarioMaximumWormCount scenario)
                scenario
        of
            Just state ->
                length (gameWorms state)
                    @?= scenarioMaximumWormCount scenario

            Nothing ->
                assertFailure
                    ( "Could not construct maximum-capacity state for "
                        ++ scenarioName scenario
                    )


-- -----------------------------------------------------------------------------
-- Adaptive food tests
-- -----------------------------------------------------------------------------

-- | Unit tests for scenario food scaling and interactive food spawning.
foodTests :: TestTree
foodTests =
    testGroup
        "Adaptive Food"
        [ testCase "Arena food target scales with worm count" $
            map
                (`scenarioFoodCountForWorms` arenaScenario)
                [2 .. 6]
                @?= [2, 3, 3, 4, 4]

        , testCase "Large Cave food target scales to seven items" $
            map
                (`scenarioFoodCountForWorms` largeCaveScenario)
                [2 .. 9]
                @?= [3, 4, 4, 5, 5, 6, 6, 7]

        , testCase "food target never exceeds scenario maximum" $ do
            scenarioFoodCountForWorms 100 arenaScenario
                @?= scenarioMaximumFoodCount arenaScenario

            scenarioFoodCountForWorms 100 caveScenario
                @?= scenarioMaximumFoodCount caveScenario

            scenarioFoodCountForWorms 100 corridorScenario
                @?= scenarioMaximumFoodCount corridorScenario

            scenarioFoodCountForWorms 100 largeCaveScenario
                @?= scenarioMaximumFoodCount largeCaveScenario

        , testCase "clearAllFood removes every food tile" $
            let foodMap =
                    placeFood (1, 1) $
                        placeFood (3, 3) openMap

                state =
                    testGameState foodMap []

                clearedState =
                    clearAllFood state
            in
                foodPositions (gameMap clearedState)
                    @?= []

        , testCase "interactive food distance accepts exactly distance three" $
            let worm =
                    testWorm 1 [(3, 3)] East

                state =
                    testGameState openSevenMap [worm]
            in do
                farEnoughFromLivingWorms state (4, 3)
                    @?= False

                farEnoughFromLivingWorms state (5, 3)
                    @?= False

                farEnoughFromLivingWorms state (6, 3)
                    @?= True

        , testCase "dead worms do not constrain interactive food distance" $
            let deadWorm =
                    deadTestWorm
                        (testWorm 1 [(3, 3)] East)

                state =
                    testGameState openSevenMap [deadWorm]
            in
                farEnoughFromLivingWorms state (3, 3)
                    @?= True

        , testCase "randomized interactive food reaches requested count and respects distance" $ do
            let worm =
                    testWorm 1 [(3, 3), (2, 3)] East

                initialMap =
                    placeFood (3, 2) $
                        placeFood (4, 3) openSevenMap

                initialState =
                    testGameState initialMap [worm]

            randomizedState <-
                randomizeFoodCountAwayFromWorms
                    5
                    initialState

            let spawnedFood =
                    foodPositions
                        (gameMap randomizedState)

            length spawnedFood @?= 5

            assertBool
                "Every spawned food item should respect the preferred head distance."
                ( all
                    (farEnoughFromLivingWorms randomizedState)
                    spawnedFood
                )

            assertBool
                "Food must not overlap the living worm body."
                ( all
                    (`notElem` wormBody worm)
                    spawnedFood
                )

        , testCase "interactive food falls back when no distant position exists" $ do
            let tinyMap =
                    fromAsciiMap
                        ["..."]

                worm =
                    testWorm 1 [(1, 0)] East

                state =
                    testGameState tinyMap [worm]

            maybePosition <-
                randomFoodPositionAwayFromWorms state

            case maybePosition of
                Nothing ->
                    assertFailure
                        "A valid fallback food position should exist."

                Just position -> do
                    assertBool
                        "Fallback must choose a free map position."
                        (position `elem` [(0, 0), (2, 0)])

                    assertBool
                        "This test should exercise the below-distance fallback."
                        ( manhattanDistance position (1, 0)
                            < interactiveFoodHeadDistance
                        )
        ]
  where
    -- | Open seven-by-seven map with enough space for distance-sensitive food tests.
    openSevenMap :: GameMap
    openSevenMap =
        fromAsciiMap
            [ "......."
            , "......."
            , "......."
            , "......."
            , "......."
            , "......."
            , "......."
            ]


-- -----------------------------------------------------------------------------
-- Play tests
-- -----------------------------------------------------------------------------

-- | Unit tests for human-versus-multiple-opponents result semantics.
playTests :: TestTree
playTests =
    testGroup
        "Play"
        [ testCase "game runs while human and opponents are alive" $
            let human =
                    testWorm 1 [(1, 1)] East

                opponentOne =
                    testWorm 2 [(2, 2)] East

                opponentTwo =
                    testWorm 3 [(3, 3)] West

                state =
                    testGameState
                        openMap
                        [human, opponentOne, opponentTwo]
            in
                resultForState 1 [2, 3] state
                    @?= PlayRunning

        , testCase "death of one opponent does not end multi-opponent game" $
            let human =
                    testWorm 1 [(1, 1)] East

                deadOpponent =
                    deadTestWorm
                        (testWorm 2 [(2, 2)] East)

                livingOpponent =
                    testWorm 3 [(3, 3)] West

                state =
                    testGameState
                        openMap
                        [human, deadOpponent, livingOpponent]
            in
                resultForState 1 [2, 3] state
                    @?= PlayRunning

        , testCase "human wins after every configured opponent dies" $
            let human =
                    testWorm 1 [(1, 1)] East

                opponentOne =
                    deadTestWorm
                        (testWorm 2 [(2, 2)] East)

                opponentTwo =
                    deadTestWorm
                        (testWorm 3 [(3, 3)] West)

                state =
                    testGameState
                        openMap
                        [human, opponentOne, opponentTwo]
            in
                resultForState 1 [2, 3] state
                    @?= PlayerWon

        , testCase "human loses when dead while an opponent remains alive" $
            let human =
                    deadTestWorm
                        (testWorm 1 [(1, 1)] East)

                opponentOne =
                    deadTestWorm
                        (testWorm 2 [(2, 2)] East)

                opponentTwo =
                    testWorm 3 [(3, 3)] West

                state =
                    testGameState
                        openMap
                        [human, opponentOne, opponentTwo]
            in
                resultForState 1 [2, 3] state
                    @?= PlayerLost

        , testCase "simultaneous elimination of human and all opponents is a draw" $
            let human =
                    deadTestWorm
                        (testWorm 1 [(1, 1)] East)

                opponentOne =
                    deadTestWorm
                        (testWorm 2 [(2, 2)] East)

                opponentTwo =
                    deadTestWorm
                        (testWorm 3 [(3, 3)] West)

                state =
                    testGameState
                        openMap
                        [human, opponentOne, opponentTwo]
            in
                resultForState 1 [2, 3] state
                    @?= PlayDraw
        ]


-- -----------------------------------------------------------------------------
-- Map tests
-- -----------------------------------------------------------------------------

-- | Unit tests for sparse map representation and ASCII parsing.
mapTests :: TestTree
mapTests =
    testGroup
        "Maps"
        [ testCase "ASCII parser preserves map dimensions" $
            let gameMap' =
                    fromAsciiMap
                        [ "...."
                        , ".#F."
                        , "...."
                        ]
            in do
                mapWidth gameMap' @?= 4
                mapHeight gameMap' @?= 3

        , testCase "unstored in-bounds cells behave as Empty" $
            let gameMap' =
                    fromAsciiMap
                        [ "..."
                        , "..."
                        ]
            in
                tileAt gameMap' (1, 1) @?= Just Empty

        , testCase "ASCII food and poison tiles are recognized" $
            let gameMap' =
                    fromAsciiMap
                        [ ".FP"
                        ]
            in do
                isFood gameMap' (1, 0) @?= True
                isPoison gameMap' (2, 0) @?= True
        ]


-- -----------------------------------------------------------------------------
-- Q-learning core tests
-- -----------------------------------------------------------------------------

-- | Unit tests for generic tabular Q-learning operations.
qLearningCoreTests :: TestTree
qLearningCoreTests =
    testGroup
        "Q-learning Core"
        [ testCase "unseen state-action pair has Q-value zero" $
            Core.qValue
                (Core.emptyQTable :: Core.QTable Int)
                0
                GoStraight
                @?= 0

        , testCase "explicitly learned zero differs from an unseen entry" $
            let table =
                    Core.setQValue
                        0
                        GoStraight
                        0
                        (Core.emptyQTable :: Core.QTable Int)
            in do
                Core.qValue table 0 GoStraight @?= 0
                Core.hasQValue table 0 GoStraight @?= True
                Core.hasQValue table 0 TurnLeft @?= False

        , testCase "bestQActions returns every action tied for maximum value" $
            let table =
                    Core.setQValue 0 TurnLeft 5 $
                        Core.setQValue 0 GoStraight 2 $
                            Core.setQValue
                                0
                                TurnRight
                                5
                                (Core.emptyQTable :: Core.QTable Int)
            in
                Core.bestQActions table 0 @?= [TurnLeft, TurnRight]

        , testCase "terminal Q update ignores future value" $
            let table =
                    Core.setQValue
                        0
                        GoStraight
                        10
                        (Core.emptyQTable :: Core.QTable Int)

                updated =
                    Core.updateQValue
                        0.1
                        0.9
                        0
                        GoStraight
                        5
                        Nothing
                        table

                actual =
                    Core.qValue updated 0 GoStraight
            in
                assertApproxEqual
                    "Terminal Q update"
                    9.5
                    actual

        , testCase "non-terminal Q update uses discounted best future value" $
            let table =
                    Core.setQValue 1 TurnLeft 20 $
                        Core.setQValue 1 GoStraight 4 $
                            Core.setQValue 1 TurnRight 8 $
                                Core.setQValue
                                    0
                                    GoStraight
                                    10
                                    (Core.emptyQTable :: Core.QTable Int)

                updated =
                    Core.updateQValue
                        0.1
                        0.9
                        0
                        GoStraight
                        5
                        (Just 1)
                        table

                actual =
                    Core.qValue updated 0 GoStraight
            in
                assertApproxEqual
                    "Non-terminal Q update"
                    11.3
                    actual
        ]


-- -----------------------------------------------------------------------------
-- Test runner
-- -----------------------------------------------------------------------------

-- | Runs all deterministic CerviQ unit-test groups.
main :: IO ()
main =
    defaultMain $
        testGroup
            "CerviQ"
            [ movementTests
            , collisionTests
            , gameTests
            , mapTests
            , scenarioTests
            , foodTests
            , playTests
            , qLearningCoreTests
            , evaluationTests
            ]