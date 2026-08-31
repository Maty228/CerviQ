{-|
Module      : Agent
Description : Baseline and heuristic AI agents used by CerviQ.

This module implements the non-learning agents used for gameplay, training
opponents, and evaluation baselines. The agents range from fully random
movement to simple safety-aware food seeking and opponent pursuit.

The safety checks are intentionally local: they evaluate immediate danger in
the current game state but do not predict the future actions of other worms.
More advanced learned behaviour is implemented separately in the Q-learning
modules.
-}

module Agent where

import Collision
import Maps
import Movement
import Types

import Data.List (minimumBy)
import Data.Ord (comparing)
import System.Random (randomRIO)


-- -----------------------------------------------------------------------------
-- Shared action helpers
-- -----------------------------------------------------------------------------

-- | All relative actions available to a worm.
allActions :: [Action]
allActions = [TurnLeft, GoStraight, TurnRight]


-- | Chooses a uniformly random element from a non-empty list.
randomChoice :: [a] -> IO a
randomChoice values = do
    index <- randomRIO (0, length values - 1)
    pure (values !! index)


-- | Returns all actions minimizing an integer evaluation function.
--
-- Keeping all equally best actions allows callers to choose randomly between
-- ties instead of introducing a fixed left/straight/right preference.
bestActions :: (Action -> Int) -> [Action] -> [Action]
bestActions evaluate actions =
    filter (\action -> evaluate action == bestScore) actions
  where
    bestScore = minimum (map evaluate actions)

-- -----------------------------------------------------------------------------
-- Immediate safety
-- -----------------------------------------------------------------------------

-- | Returns whether a worm's head overlaps its own remaining body.
hitsOwnBody :: Worm -> Bool
hitsOwnBody worm =
    case wormBody worm of
        [] -> False
        headPosition : bodyPositions -> headPosition `elem` bodyPositions


-- | Returns whether an action avoids all immediately visible danger.
--
-- The action is simulated as a non-growing movement. The resulting head must
-- avoid walls, poison, the worm's own body, and positions currently occupied
-- by other living worms. Other worms' future actions are not predicted.
isSafeAction :: GameState -> Worm -> Action -> Bool
isSafeAction state worm action =
    not (isBlocked currentMap newHead)
        && not (isPoison currentMap newHead)
        && not (hitsOwnBody movedWorm)
        && not (positionOccupied newHead otherOccupied)
  where
    movedWorm = moveWormAfterAction False action worm
    newHead = wormHead movedWorm
    currentMap = gameMap state

    otherWorms =
        filter (\other -> wormAlive other && wormId other /= wormId worm) (gameWorms state)

    otherOccupied = occupiedPositions otherWorms


-- | Returns all immediately safe actions available to a worm.
safeActions :: GameState -> Worm -> [Action]
safeActions state worm =
    filter (isSafeAction state worm) allActions


-- -----------------------------------------------------------------------------
-- Target selection
-- -----------------------------------------------------------------------------

-- | Returns Manhattan distance between two map positions.
--
-- Manhattan distance matches the four-directional grid movement of the game
-- and is used by the simple greedy heuristic agents.
distance :: Position -> Position -> Int
distance (x1, y1) (x2, y2) =
    abs (x1 - x2) + abs (y1 - y2)


-- | Returns the nearest food position according to Manhattan distance.
nearestFood :: GameState -> Worm -> Maybe Position
nearestFood state worm =
    case foodPositions (gameMap state) of
        [] -> Nothing
        foods -> Just (minimumBy (comparing (distance (wormHead worm))) foods)


-- | Returns the distance from the worm's next head to a target after an action.
distanceAfterAction :: Worm -> Action -> Position -> Int
distanceAfterAction worm action target =
    distance (headAfterAction worm action) target


-- | Returns all living worms except the controlled worm.
enemyWorms :: GameState -> Worm -> [Worm]
enemyWorms state worm =
    filter isEnemy (gameWorms state)
  where
    isEnemy other =
        wormAlive other && wormId other /= wormId worm


-- | Returns the nearest living enemy head according to Manhattan distance.
nearestEnemyHead :: GameState -> Worm -> Maybe Position
nearestEnemyHead state worm =
    case map wormHead (enemyWorms state worm) of
        [] -> Nothing
        enemyHeads -> Just (minimumBy (comparing (distance (wormHead worm))) enemyHeads)


-- -----------------------------------------------------------------------------
-- Baseline agents
-- -----------------------------------------------------------------------------

-- | Chooses uniformly from all possible actions.
--
-- The game state is ignored, so the selected action may immediately cause a
-- collision.
randomAgent :: Agent
randomAgent _ _ =
    randomChoice allActions


-- | Chooses randomly from actions that avoid immediate danger.
--
-- If no safe action exists, the agent falls back to a fully random action.
safeRandomAgent :: Agent
safeRandomAgent state worm =
    case safeActions state worm of
        [] -> randomAgent state worm
        actions -> randomChoice actions


-- | Chooses an action minimizing Manhattan distance to the nearest food.
--
-- Safety is not considered. If no food exists, the agent behaves randomly.
greedyFoodAgent :: Agent
greedyFoodAgent state worm =
    case nearestFood state worm of
        Nothing -> randomAgent state worm
        Just food ->
            randomChoice (bestActions evaluate allActions)
          where
            evaluate action = distanceAfterAction worm action food


-- | Chooses an immediately safe action minimizing distance to the nearest food.
--
-- If no food exists, the agent falls back to 'safeRandomAgent'. If every action
-- is unsafe, it falls back to a fully random action.
safeGreedyFoodAgent :: Agent
safeGreedyFoodAgent state worm =
    case nearestFood state worm of
        Nothing -> safeRandomAgent state worm
        Just food ->
            case safeActions state worm of
                [] -> randomAgent state worm
                actions -> randomChoice (bestActions evaluate actions)
          where
            evaluate action = distanceAfterAction worm action food


-- | Chooses an immediately safe action moving towards the nearest enemy head.
--
-- This is a simple pursuit heuristic rather than a predictive combat planner.
-- Without an enemy it falls back to 'safeGreedyFoodAgent'; if trapped, it falls
-- back to a random action.
safeHunterAgent :: Agent
safeHunterAgent state worm =
    case nearestEnemyHead state worm of
        Nothing -> safeGreedyFoodAgent state worm
        Just enemyHead ->
            case safeActions state worm of
                [] -> randomAgent state worm
                actions -> randomChoice (bestActions evaluate actions)
          where
            evaluate action = distanceAfterAction worm action enemyHead