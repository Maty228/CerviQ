module Game where

import Types
import Maps
import Movement
import Collision

import System.Random (randomRIO)

-- -----------------------------------------------------------------------------
-- Food handling
-- -----------------------------------------------------------------------------

-- | Returns True if executing the given action would move the worm onto food.
wormWillEatFood :: GameMap -> Action -> Worm -> Bool
wormWillEatFood currentMap action worm =
    let newDirection = applyAction (wormDirection worm) action
        turnedWorm = worm {wormDirection = newDirection}
        newHead = nextHeadPosition turnedWorm
    in isFood currentMap newHead


-- | Removes food eaten by the given worms.
removeEatenFood :: [Worm] -> GameMap -> GameMap
removeEatenFood worms currentMap = 
    foldr removeFood currentMap (map wormHead worms)


-- -----------------------------------------------------------------------------
-- Food spawning
-- -----------------------------------------------------------------------------

-- | Checks whether food can be placed at the given position.
isFreeForFood :: GameState -> Position -> Bool
isFreeForFood state pos =
    isEmptyTile stateMap pos &&
    not (positionOccupied pos wormPositions)
  where
    stateMap = gameMap state
    wormPositions = occupiedPositions (filter wormAlive (gameWorms state))


-- | Returns all valid positions where food may be spawned.
freeFoodPositions :: GameState -> [Position]
freeFoodPositions state = filter (isFreeForFood state) (allMapPositions (gameMap state))


-- | Places food on the given position.
spawnFoodAt :: Position -> GameState -> GameState
spawnFoodAt pos state =
    state {gameMap = placeFood pos (gameMap state)}


-- -----------------------------------------------------------------------------
-- Action helpers
-- -----------------------------------------------------------------------------

-- | Returns the action assigned to the given worm.
-- If no action is specified, the worm continues straight.
actionForWorm :: [(Int, Action)] -> Worm -> Action
actionForWorm actions worm =
    case lookup (wormId worm) actions of
        Just action -> action
        Nothing -> GoStraight


-- -----------------------------------------------------------------------------
-- Worm statistics
-- -----------------------------------------------------------------------------

-- | Increases the worm's age by one game tick.
increaseAge :: Worm -> Worm
increaseAge worm =
    worm {
        wormStats =
            (wormStats worm) {age = age (wormStats worm) + 1}
    }


-- | Increases the number of eaten food items.
increaseFoodEaten :: Worm -> Worm
increaseFoodEaten worm =
    worm {
        wormStats =
            (wormStats worm) {foodEaten = foodEaten (wormStats worm) + 1}
    }


-- | Returns True if the worm's head is currently on a food tile.
wormAteFood :: GameMap -> Worm -> Bool
wormAteFood currentMap worm =
    isFood currentMap (wormHead worm)


-- | Increases the number of kills by the given amount.
increaseKills :: Int -> Worm -> Worm
increaseKills amount worm =
    worm
        { wormStats =
            (wormStats worm)
                { kills = kills (wormStats worm) + amount }
        }


-- | Extracts the killer ID from a death reason, if there is one.
killerFromDeath :: (Worm, DeathReason) -> Maybe Int
killerFromDeath (_, HitOtherBody killerId) = Just killerId
killerFromDeath _ = Nothing


-- | Counts how many kills should be awarded to the given worm.
killCountForWorm :: [Int] -> Worm -> Int
killCountForWorm killerIds worm =
    length (filter (== wormId worm) killerIds)


-- | Applies kill rewards to a worm.
applyKillStats :: [Int] -> Worm -> Worm
applyKillStats killerIds worm =
    increaseKills (killCountForWorm killerIds worm) worm


-- -----------------------------------------------------------------------------
-- Food spawning
-- -----------------------------------------------------------------------------

-- | Returns a random valid position for spawning food.
--
-- Returns Nothing if no free position exists.
randomFoodPosition :: GameState -> IO (Maybe Position)
randomFoodPosition state = do
    let freePositions = freeFoodPositions state
    case freePositions of
        [] -> return Nothing
        _ -> do 
            index <- randomRIO (0, length freePositions - 1)
            return (Just (freePositions !! index))


-- | Ensures that at least the given number of food items
-- are present on the map.
maintainFoodCount :: Int -> GameState -> IO GameState
maintainFoodCount maxFood state
    | length (foodPositions (gameMap state)) >= maxFood = return state
    | otherwise = do
        maybePosition <- randomFoodPosition state
        case maybePosition of
            Nothing -> return state
            Just position -> maintainFoodCount maxFood (spawnFoodAt position state)


-- | Result of one game step including the updated state and deaths.
data GameStepResult = GameStepResult
    {
        gameStepState :: GameState,
        gameStepDeaths :: [(Int, DeathReason)]
    }
    deriving (Show, Eq)


-- -----------------------------------------------------------------------------
-- Game simulation
-- -----------------------------------------------------------------------------

-- | Advances the game by one tick and returns detailed information about deaths.
--
-- For each living worm:
--   * determines its action,
--   * checks whether it will grow,
--   * simulates movement and collisions,
--   * updates statistics,
--   * removes eaten food,
--   * increments the global game tick.
stepGameDetailed :: [(Int, Action)] -> GameState -> GameStepResult
stepGameDetailed actions state =
    let currentMap = gameMap state
        alive = filter wormAlive (gameWorms state)
        alreadyDead = filter (not . wormAlive) (gameWorms state)

        moves =
            map
                (\worm ->
                    let action = actionForWorm actions worm
                        grows = wormWillEatFood currentMap action worm
                    in (grows, action, worm)
                )
                alive

        (survivors, deaths) =
            simulateTurn currentMap moves

        collided =
            map fst deaths

        killerIds =
            [ killerId
            | death <- deaths
            , Just killerId <- [killerFromDeath death]
            ]

        deathEvents =
            [ (wormId worm, reason)
            | (worm, reason) <- deaths
            ]

        newMap = removeEatenFood survivors currentMap

        updatedSurvivors =
            map
                (\worm ->
                    let agedWorm = increaseAge worm
                        fedWorm =
                            if wormAteFood currentMap worm
                                then increaseFoodEaten agedWorm
                                else agedWorm
                    in applyKillStats killerIds fedWorm
                )
                survivors

        updatedCollided =
            map
                (\worm ->
                    applyKillStats killerIds (increaseAge worm)
                        { wormAlive = False }
                )
                collided

        updatedWorms =
            updatedSurvivors ++ updatedCollided ++ alreadyDead

        updatedState =
            state
                {
                    gameMap = newMap,
                    gameWorms = updatedWorms,
                    gameTick = gameTick state + 1
                }

    in
        GameStepResult
            {
                gameStepState = updatedState,
                gameStepDeaths = deathEvents
            }


-- | Advances the game by one tick.
stepGame :: [(Int, Action)] -> GameState -> GameState
stepGame actions state =
    gameStepState (stepGameDetailed actions state)