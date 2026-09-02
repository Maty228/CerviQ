{-|
Module      : Game
Description : High-level game-state updates and one-tick CerviQ simulation.

This module coordinates the individual parts of the game engine. It prepares
worm actions, determines growth, delegates simultaneous movement and collision
resolution to "Collision", updates worm statistics and food, manages random
food initialization and replenishment, records recent head positions, and
advances the global game tick.

The central simulation functions are 'stepGameDetailed' and 'stepGame'.
-}

module Game where

import Collision
import Maps
import Movement
import Types

import qualified Data.Map as Map
import System.Random (randomRIO)


-- -----------------------------------------------------------------------------
-- Actions and food consumption
-- -----------------------------------------------------------------------------

-- | Returns the action assigned to a worm.
--
-- A worm without an explicitly supplied action continues straight.
actionForWorm :: [(Int, Action)] -> Worm -> Action
actionForWorm actions worm =
    case lookup (wormId worm) actions of
        Just action -> action
        Nothing -> GoStraight


-- | Returns whether executing an action would move a worm onto food.
wormWillEatFood :: GameMap -> Action -> Worm -> Bool
wormWillEatFood currentMap action worm =
    isFood currentMap (headAfterAction worm action)


-- | Returns whether a moved worm's head is currently on food.
wormAteFood :: GameMap -> Worm -> Bool
wormAteFood currentMap worm =
    isFood currentMap (wormHead worm)


-- | Removes food occupied by the heads of the given worms.
removeEatenFood :: [Worm] -> GameMap -> GameMap
removeEatenFood worms currentMap =
    foldr removeFood currentMap (map wormHead worms)


-- -----------------------------------------------------------------------------
-- Food spawning
-- -----------------------------------------------------------------------------

-- | Returns whether food may be spawned at a position.
--
-- Food can only be placed on an empty tile not occupied by a living worm.
isFreeForFood :: GameState -> Position -> Bool
isFreeForFood state pos =
    isEmptyTile stateMap pos && not (positionOccupied pos wormPositions)
  where
    stateMap = gameMap state
    wormPositions = occupiedPositions (filter wormAlive (gameWorms state))


-- | Returns all positions currently suitable for spawning food.
freeFoodPositions :: GameState -> [Position]
freeFoodPositions state =
    filter (isFreeForFood state) (allMapPositions (gameMap state))


-- | Places food at a position without performing additional validation.
spawnFoodAt :: Position -> GameState -> GameState
spawnFoodAt pos state =
    state {gameMap = placeFood pos (gameMap state)}


-- | Returns a random currently valid position for spawning food.
--
-- Returns 'Nothing' if the map contains no free position.
randomFoodPosition :: GameState -> IO (Maybe Position)
randomFoodPosition state =
    case freeFoodPositions state of
        [] -> pure Nothing
        freePositions -> do
            index <- randomRIO (0, length freePositions - 1)
            pure (Just (freePositions !! index))


-- | Minimum Manhattan distance between newly spawned interactive food and the
-- head of every living worm.
interactiveFoodHeadDistance :: Int
interactiveFoodHeadDistance = 3


-- | Returns the Manhattan distance between two map positions.
manhattanDistance :: Position -> Position -> Int
manhattanDistance (x1, y1) (x2, y2) =
    abs (x1 - x2) + abs (y1 - y2)


-- | Returns whether a position is sufficiently far from every living worm head.
--
-- This prevents interactive food from randomly appearing immediately beside a
-- worm and giving it an accidental near-guaranteed reward.
farEnoughFromLivingWorms :: GameState -> Position -> Bool
farEnoughFromLivingWorms state pos =
    all
        (\headPos -> manhattanDistance pos headPos >= interactiveFoodHeadDistance)
        livingHeads
  where
    livingHeads =
        [ headPos
        | worm <- gameWorms state
        , wormAlive worm
        , headPos : _ <- [wormBody worm]
        ]


-- | Returns a random valid food position that preferably avoids living worms.
--
-- If the preferred-distance restriction leaves no candidate, the function
-- falls back to every otherwise valid free position. This keeps food spawning
-- possible even in small or heavily occupied multi-worm games.
randomFoodPositionAwayFromWorms :: GameState -> IO (Maybe Position)
randomFoodPositionAwayFromWorms state =
    chooseRandom candidatePositions
  where
    freePositions =
        freeFoodPositions state

    preferredPositions =
        filter (farEnoughFromLivingWorms state) freePositions

    candidatePositions =
        if null preferredPositions
            then freePositions
            else preferredPositions

    -- | Selects one random position from a non-empty candidate list.
    chooseRandom :: [Position] -> IO (Maybe Position)
    chooseRandom [] =
        pure Nothing

    chooseRandom positions = do
        index <- randomRIO (0, length positions - 1)
        pure (Just (positions !! index))


-- | Maintains the requested number of food items while preferring new food
-- positions that are not immediately beside a living worm.
maintainFoodCountAwayFromWorms :: Int -> GameState -> IO GameState
maintainFoodCountAwayFromWorms foodTarget state
    | length (foodPositions (gameMap state)) >= foodTarget =
        pure state

    | otherwise = do
        maybePosition <-
            randomFoodPositionAwayFromWorms state

        case maybePosition of
            Nothing ->
                pure state

            Just position ->
                maintainFoodCountAwayFromWorms
                    foodTarget
                    (spawnFoodAt position state)


-- | Replaces all existing food with randomly positioned interactive food while
-- avoiding cells immediately beside living worm heads whenever possible.
randomizeFoodCountAwayFromWorms :: Int -> GameState -> IO GameState
randomizeFoodCountAwayFromWorms foodTarget =
    maintainFoodCountAwayFromWorms foodTarget . clearAllFood

-- | Removes every food tile from the current game state.
--
-- Removed food is represented by absence from the sparse map, which is
-- equivalent to an empty in-bounds tile.
clearAllFood :: GameState -> GameState
clearAllFood state =
    state
        { gameMap =
            currentMap
                { mapTiles = Map.filter (/= Food) (mapTiles currentMap)
                }
        }
  where
    currentMap =
        gameMap state


-- | Replaces all existing food with the requested number of randomly placed items.
--
-- The state is first cleared of every predefined food tile and then populated
-- through 'maintainFoodCount'. The requested count is therefore reached exactly
-- whenever the map contains enough valid free positions.
randomizeFoodCount :: Int -> GameState -> IO GameState
randomizeFoodCount foodTarget =
    maintainFoodCount foodTarget . clearAllFood

-- | Ensures that the map contains at least the requested number of food items.
--
-- Food is added recursively until the requested count is reached or no valid
-- spawn position remains.
maintainFoodCount :: Int -> GameState -> IO GameState
maintainFoodCount maxFood state
    | length (foodPositions (gameMap state)) >= maxFood = pure state
    | otherwise = do
        maybePosition <- randomFoodPosition state
        case maybePosition of
            Nothing -> pure state
            Just position -> maintainFoodCount maxFood (spawnFoodAt position state)


-- -----------------------------------------------------------------------------
-- Worm statistics
-- -----------------------------------------------------------------------------

-- | Increases a worm's age by one game tick.
increaseAge :: Worm -> Worm
increaseAge worm =
    worm {wormStats = stats {age = age stats + 1}}
  where
    stats = wormStats worm


-- | Increases a worm's number of eaten food items by one.
increaseFoodEaten :: Worm -> Worm
increaseFoodEaten worm =
    worm {wormStats = stats {foodEaten = foodEaten stats + 1}}
  where
    stats = wormStats worm


-- | Increases a worm's kill count by the given amount.
increaseKills :: Int -> Worm -> Worm
increaseKills amount worm =
    worm {wormStats = stats {kills = kills stats + amount}}
  where
    stats = wormStats worm


-- | Extracts the killer worm ID from a death caused by another worm's body.
killerFromDeath :: (Worm, DeathReason) -> Maybe Int
killerFromDeath (_, HitOtherBody killerId) = Just killerId
killerFromDeath _ = Nothing


-- | Counts deaths credited to a particular worm.
killCountForWorm :: [Int] -> Worm -> Int
killCountForWorm killerIds worm =
    length (filter (== wormId worm) killerIds)


-- | Applies all kills credited to a worm during the current tick.
applyKillStats :: [Int] -> Worm -> Worm
applyKillStats killerIds worm =
    increaseKills (killCountForWorm killerIds worm) worm


-- | Updates age, food, and kill statistics of a surviving moved worm.
updateSurvivorStats :: GameMap -> [Int] -> Worm -> Worm
updateSurvivorStats previousMap killerIds worm =
    applyKillStats killerIds fedWorm
  where
    agedWorm = increaseAge worm
    fedWorm
        | wormAteFood previousMap worm = increaseFoodEaten agedWorm
        | otherwise = agedWorm


-- | Updates statistics of a worm that died during the current tick and marks it
-- as no longer alive.
updateDeadWorm :: [Int] -> Worm -> Worm
updateDeadWorm killerIds worm =
    (applyKillStats killerIds (increaseAge worm)) {wormAlive = False}


-- -----------------------------------------------------------------------------
-- Head history
-- -----------------------------------------------------------------------------

-- | Maximum number of recent head positions retained for each worm.
headHistoryLimit :: Int
headHistoryLimit = 256


-- | Creates head-position history containing the current head of each worm.
initialHeadHistory :: [Worm] -> Map.Map Int [Position]
initialHeadHistory worms =
    Map.fromList
        [ (wormId worm, [headPosition])
        | worm <- worms
        , headPosition : _ <- [wormBody worm]
        ]


-- | Resets all stored head histories to the worms' current positions.
resetHeadHistory :: GameState -> GameState
resetHeadHistory state =
    state {gameHeadHistory = initialHeadHistory (gameWorms state)}


-- | Prepends each worm's current head to its stored history.
--
-- Only the most recent 'headHistoryLimit' positions are retained. The caller
-- supplies both living and dead worms, so a dead worm's final head continues to
-- be recorded on later global ticks while another worm remains alive.
recordHeadHistory :: [Worm] -> Map.Map Int [Position] -> Map.Map Int [Position]
recordHeadHistory worms history =
    foldr recordWorm history worms
  where
    recordWorm worm currentHistory =
        case wormBody worm of
            [] -> currentHistory
            headPosition : _ ->
                Map.insert
                    (wormId worm)
                    (take headHistoryLimit (headPosition : previousHistory))
                    currentHistory
              where
                previousHistory = Map.findWithDefault [] (wormId worm) currentHistory


-- -----------------------------------------------------------------------------
-- Game simulation
-- -----------------------------------------------------------------------------

-- | Result of one game tick together with death events produced during it.
data GameStepResult = GameStepResult
    {
        gameStepState :: GameState,
        gameStepDeaths :: [(Int, DeathReason)]
    }
    deriving (Show, Eq)


-- | Prepares one living worm for simultaneous turn simulation.
--
-- The returned tuple contains whether the worm grows, its selected action, and
-- its current state before movement.
prepareWormMove :: GameMap -> [(Int, Action)] -> Worm -> (Bool, Action, Worm)
prepareWormMove currentMap actions worm =
    (wormWillEatFood currentMap action worm, action, worm)
  where
    action = actionForWorm actions worm


-- | Advances the complete game by one tick and reports deaths from that tick.
--
-- One tick is processed in these phases:
--
-- 1. select an action and determine growth for every living worm;
-- 2. move all living worms and resolve collisions simultaneously;
-- 3. remove food eaten by surviving worms;
-- 4. update worm statistics and alive status;
-- 5. record head history and increment the global tick counter.
stepGameDetailed :: [(Int, Action)] -> GameState -> GameStepResult
stepGameDetailed actions state =
    GameStepResult
        {
            gameStepState = updatedState,
            gameStepDeaths = deathEvents
        }
  where
    currentMap = gameMap state
    aliveWorms = filter wormAlive (gameWorms state)
    alreadyDeadWorms = filter (not . wormAlive) (gameWorms state)

    moves = map (prepareWormMove currentMap actions) aliveWorms
    (survivors, deaths) = simulateTurn currentMap moves

    collidedWorms = map fst deaths
    killerIds = [killerId | death <- deaths, Just killerId <- [killerFromDeath death]]
    deathEvents = [(wormId worm, reason) | (worm, reason) <- deaths]

    newMap = removeEatenFood survivors currentMap
    updatedSurvivors = map (updateSurvivorStats currentMap killerIds) survivors
    updatedCollided = map (updateDeadWorm killerIds) collidedWorms
    updatedWorms = updatedSurvivors ++ updatedCollided ++ alreadyDeadWorms

    updatedHeadHistory = recordHeadHistory updatedWorms (gameHeadHistory state)

    updatedState =
        state
            {
                gameMap = newMap,
                gameWorms = updatedWorms,
                gameTick = gameTick state + 1,
                gameHeadHistory = updatedHeadHistory
            }


-- | Advances the complete game by one tick and returns only the new game state.
stepGame :: [(Int, Action)] -> GameState -> GameState
stepGame actions state =
    gameStepState (stepGameDetailed actions state)
