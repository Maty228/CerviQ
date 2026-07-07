module Controller where

import Agent
import Input
import Types

-- -----------------------------------------------------------------------------
-- Controller configuration
-- -----------------------------------------------------------------------------

-- | Assigns controllers to worm IDs.
--
-- This is the place where game modes are selected:
-- * one Human controller means player-only mode,
-- * Human + AI means player versus computer,
-- * only AI controllers means automatic simulation.
controllers :: [(Int, Controller)]
controllers = 
    [ (1, AI safeRandomAgent)
    , (2, AI safeGreedyFoodAgent)

    ]


-- -----------------------------------------------------------------------------
-- Controller execution
-- -----------------------------------------------------------------------------

-- | Finds a living worm by its ID
findLivingWormById :: Int -> GameState -> Maybe Worm
findLivingWormById targetId state =
    case filter matches (gameWorms state) of
        worm : _ -> Just worm
        [] -> Nothing
    where
        matches :: Worm -> Bool
        matches worm =
            wormId worm == targetId && wormAlive worm


-- | Runs one controller and returns the selected action.
runController :: GameState -> (Int, Controller) -> IO (Int, Action)
runController state (controlledWormId, controller) =
    case findLivingWormById controlledWormId state of
        Just worm -> do
            action <- controllerAction controller state worm
            pure (controlledWormId, action)
        Nothing -> pure (controlledWormId, GoStraight)


-- | Converts a controller into an action.
controllerAction :: Controller -> GameState -> Worm -> IO Action
controllerAction Human _ worm = getHumanAction worm
controllerAction (AI agent) state worm = agent state worm


-- | Collects actions from all configured controllers.
collectActions :: [(Int, Controller)] -> GameState -> IO [(Int, Action)]
collectActions assignedControllers state =
    mapM (runController state) assignedControllers


hasHumanController :: [(Int, Controller)] -> Bool
hasHumanController assignedControllers = any isHuman assignedControllers
  where
    isHuman :: (Int, Controller) -> Bool
    isHuman (_, Human) = True
    isHuman _ = False
