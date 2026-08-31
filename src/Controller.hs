{-|
Module      : Controller
Description : Execution of AI controllers assigned to CerviQ worms.

This module connects controller assignments to concrete game actions. A
controller is associated with a worm ID and delegates action selection to the
corresponding AI agent.

The same controller mechanism is reused by Watch Agents mode, training, and
evaluation. Human input in graphical play mode is handled separately by
"PlayGui".
-}

module Controller where

import Types


-- -----------------------------------------------------------------------------
-- Controller execution
-- -----------------------------------------------------------------------------

-- | Finds a living worm by its unique ID.
findLivingWormById :: Int -> GameState -> Maybe Worm
findLivingWormById targetId state =
    case filter (\worm -> wormId worm == targetId && wormAlive worm) (gameWorms state) of
        worm : _ -> Just worm
        [] -> Nothing


-- | Obtains an action from a controller for one worm.
controllerAction :: Controller -> GameState -> Worm -> IO Action
controllerAction (AI agent) state worm =
    agent state worm


-- | Runs one controller assignment and returns the selected action.
--
-- If the assigned worm no longer exists or is dead, 'GoStraight' is returned.
-- This allows controller assignment lists to remain unchanged after a worm dies.
runController :: GameState -> (Int, Controller) -> IO (Int, Action)
runController state (controlledWormId, controller) =
    case findLivingWormById controlledWormId state of
        Just worm -> do
            action <- controllerAction controller state worm
            pure (controlledWormId, action)
        Nothing ->
            pure (controlledWormId, GoStraight)


-- | Collects one action from every assigned controller.
collectActions :: [(Int, Controller)] -> GameState -> IO [(Int, Action)]
collectActions assignedControllers state =
    mapM (runController state) assignedControllers