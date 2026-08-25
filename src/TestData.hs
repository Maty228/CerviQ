module TestData where

import Scenario
import Scenarios
import Types


-- | Initial worm statistics kept for backwards compatibility with test code.
initialWormStats :: WormStats
initialWormStats =
    emptyWormStats


-- | Original development map kept for backwards compatibility.
testMap :: GameMap
testMap =
    arenaMap


-- | Original first development worm kept for backwards compatibility.
testWorm :: Worm
testWorm =
    arenaWormOne


-- | Original second development worm kept for backwards compatibility.
testAiWorm :: Worm
testAiWorm =
    arenaWormTwo


-- | Original development game state kept for backwards compatibility.
testGame :: GameState
testGame =
    scenarioInitialState arenaScenario