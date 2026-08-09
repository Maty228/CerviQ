module QLearning.Debug
    ( AgentDebugInfo(..)
    , AgentDebugProvider
    , makeQDebugProvider
    ) where
        
import qualified QLearning.Core as Core
import Types


-- -----------------------------------------------------------------------------
-- Agent debug information
-- -----------------------------------------------------------------------------

-- | Version-independent debugging information for one Q-learning decision.
data AgentDebugInfo = AgentDebugInfo
    {
        debugStateLines :: [(String, String)],
        debugQValues :: [(Action, Double)],
        debugBestActions :: [Action]
    }
    deriving (Show, Eq)


-- | Produces debugging information for one agent in the current game state.
type AgentDebugProvider = GameState -> Worm -> AgentDebugInfo


-- | Creates a generic Q-learning debug provider.
--
-- The state-description function is version-specific, while the Q-value
-- calculation is shared by all Q-learning versions.
makeQDebugProvider :: Ord state => Core.QLearningSpec state -> (state -> [(String, String)]) -> Core.QTable state -> AgentDebugProvider
makeQDebugProvider spec describeState table gameState worm =
    let rlState = Core.qlEncodeState spec gameState worm
    in
        AgentDebugInfo
            {
                debugStateLines = describeState rlState,
                debugQValues = Core.qValuesForState table rlState,
                debugBestActions = Core.bestQActions table rlState
            }