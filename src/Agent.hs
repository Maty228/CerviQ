module Agent where

import Collision
import Maps
import Movement
import Types


import System.Random (randomRIO)
import Data.List (minimumBy)
import Data.Ord (comparing)


-- | All actions available to an agent.
allActions :: [Action]
allActions = [TurnLeft, GoStraight, TurnRight]


-- | Choose a random element from a non-empty list.
randomChoice :: [a] -> IO a
randomChoice values = do
    index <- randomRIO (0, length values -1)
    pure (values !! index)


findWormById :: Int -> GameState -> Maybe Worm
findWormById targetId state =
    case filter (\worm -> wormId worm == targetId && wormAlive worm) (gameWorms state) of
        worm : _ -> Just worm
        [] -> Nothing


type AgentAssignment = (Int, Agent)

runAgents :: [AgentAssignment] -> GameState -> IO [(Int, Action)]
runAgents assignments state =
    mapM runAgent assignments
  where
    runAgent :: AgentAssignment -> IO (Int, Action)
    runAgent (controlledWormId, agent) =
        case findWormById controlledWormId state of
            Just worm -> do
                action <- agent state worm
                pure (controlledWormId, action)

            Nothing ->
                pure (controlledWormId, GoStraight)




-- | Chooses a random action.
--
-- Every legal action has the same probability. The agent does not
-- consider the game state and may choose actions that immediately
-- lead to collisions.
randomAgent :: Agent
randomAgent _ _ = do
    randomChoice allActions


-- | Return True if performing the given action is immediately safe.
--
-- The check uses only the current game state. It avoids walls, poison
-- and positions currently occupied by living worms.
isSafeAction :: GameState -> Worm -> Action -> Bool
isSafeAction state worm action =
    let movedWorm = moveWormAfterAction False action worm
        newHead = wormHead movedWorm
        currentMap = gameMap state
        occupied = occupiedPositions (filter wormAlive (gameWorms state))
    in not (isBlocked currentMap newHead)
    && not (isPoison currentMap newHead)
    && not (positionOccupied newHead occupied)


-- | Returns all immediately safe actions for the given worm.
safeActions :: GameState -> Worm -> [Action]
safeActions state worm =
    filter (isSafeAction state worm) allActions

-- | Chooses a random action that avoids immediate danger.
--
-- If no safe action exists, it falls back to the fully random agent.
safeRandomAgent :: Agent
safeRandomAgent state worm =
    case safeActions state worm of
        [] -> randomAgent state worm
        actions -> randomChoice actions


distance :: Position -> Position -> Int
distance (x1, y1) (x2, y2) =
    abs (x1 - x2) + abs (y1 - y2)


nearestFood :: GameState -> Worm -> Maybe Position
nearestFood state worm =
    case foodPositions (gameMap state) of
        [] -> Nothing
        foods -> Just $ minimumBy (comparing (distance (wormHead worm))) foods

-- | Returns the distance to the target position after performing the given action.
distanceAfterAction :: Worm -> Action -> Position -> Int
distanceAfterAction worm action target =
    distance (headAfterAction worm action) target

-- | Returns all actions that minimize the given evaluation function.
bestActions :: (Action -> Int) -> [Action] -> [Action]
bestActions evaluate actions =
    let bestScore = minimum (map evaluate actions)
    in filter (\action -> evaluate action == bestScore) allActions

-- | Chooses an action that minimizes the distance to the nearest food.
greedyFoodAgent :: Agent
greedyFoodAgent state worm =
    case nearestFood state worm of
        Nothing -> randomAgent state worm
        Just food -> do
            let actions = bestActions (\action -> distanceAfterAction worm action food) allActions
            randomChoice actions

-- | Chooses the safe action that minimizes the distance to the nearest food.
safeGreedyFoodAgent :: Agent
safeGreedyFoodAgent state worm =
    case nearestFood state worm of
        Nothing -> safeRandomAgent state worm
        Just food -> case safeActions state worm of
            [] -> randomAgent state worm
            actions -> do
                let best = bestActions (\action -> distanceAfterAction worm action food) actions
                randomChoice best