module Game where

import Types
import Maps
import Movement
import Collision

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
    stateMap = currentMap state
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


-- -----------------------------------------------------------------------------
-- Game simulation
-- -----------------------------------------------------------------------------

-- | Advances the game by one tick.
--
-- For each living worm:
--   * determines its action,
--   * checks whether it will grow,
--   * simulates movement and collisions,
--   * updates statistics,
--   * removes eaten food,
--   * increments the global game tick.


stepGame :: [(Int, Action)] -> GameState -> GameState
stepGame actions state =
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
    
        (survivors, collided) =
            simulateTurn currentMap moves
        
        newMap = removeEatenFood survivors currentMap

        updatedSurvivors =
            map
                (\worm ->
                    let agedWorm = increaseAge worm
                    in
                        if wormAteFood currentMap worm
                            then increaseFoodEaten agedWorm
                            else agedWorm
                )
                survivors

        updatedCollided =
            map
                (\worm ->
                    (increaseAge worm) { wormAlive = False }
                )
                collided

        updatedWorms =
            updatedSurvivors ++ updatedCollided ++ alreadyDead
    in 
        state
            {
                gameMap = newMap,
                gameWorms = updatedWorms,
                gameTick = gameTick state + 1
            }