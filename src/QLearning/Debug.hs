{-|
Module      : QLearning.Debug
Description : Version-independent debug interface for Q-learning agents.

This module defines the common diagnostic representation consumed by the Watch
Agents GUI. Individual Q-learning versions describe their own encoded state,
while the shared provider computes Q-values and currently preferred actions.

This abstraction prevents the GUI from depending directly on V1, V2, V3, or V4
state types.
-}

module QLearning.Debug
    ( AgentDebugInfo(..)
    , AgentDebugProvider
    , makeQDebugProvider
    ) where

import qualified QLearning.Core as Core
import Types


-- -----------------------------------------------------------------------------
-- Debug information
-- -----------------------------------------------------------------------------

-- | Version-independent diagnostic information for one Q-learning decision.
data AgentDebugInfo = AgentDebugInfo
    {
        -- | Human-readable description of the encoded RL state.
        debugStateLines :: [(String, String)],

        -- | Current Q-value of every available action.
        debugQValues :: [(Action, Double)],

        -- | Actions currently tied for the greatest Q-value.
        debugBestActions :: [Action]
    }
    deriving (Show, Eq)


-- | Produces diagnostic information for one agent in a game state.
type AgentDebugProvider = GameState -> Worm -> AgentDebugInfo


-- | Creates a generic debug provider for one Q-learning version.
--
-- The version supplies only a function describing its encoded state. State
-- encoding and Q-table queries are performed through the shared Q-learning
-- specification and table.
makeQDebugProvider
    :: Ord state
    => Core.QLearningSpec state
    -> (state -> [(String, String)])
    -> Core.QTable state
    -> AgentDebugProvider
makeQDebugProvider spec describeState table gameState worm =
    AgentDebugInfo
        { debugStateLines = describeState rlState
        , debugQValues = Core.qValuesForState table rlState
        , debugBestActions = Core.bestQActions table rlState
        }
  where
    rlState = Core.qlEncodeState spec gameState worm