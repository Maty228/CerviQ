{-|
Module      : Config
Description : Global default configuration values used by CerviQ.

This module contains small project-wide defaults such as the conventional player
worm identifier, the fixed food target used by training and legacy/default
simulation constructors, and the tick delay used by loops with explicit timing.

Interactive Watch and Play sessions use scenario-specific adaptive food targets
defined in "Scenario" rather than changing the global 'maxFoodCount'.
-}
module Config
    ( playerWormId
    , maxFoodCount
    , tickDelay
    ) where


-- -----------------------------------------------------------------------------
-- Game configuration
-- -----------------------------------------------------------------------------

-- | Conventional identifier used for the player-controlled worm in game modes
-- that rely on a fixed player ID.
playerWormId :: Int
playerWormId = 1


-- | Default fixed food target used by training, evaluation, and compatibility
-- constructors.
--
-- Interactive Watch and Play games compute their own target from the selected
-- scenario and configured worm count.
maxFoodCount :: Int
maxFoodCount = 1


-- | Fixed delay between ticks, in microseconds, for game loops that use an
-- explicit delay.
tickDelay :: Int
tickDelay = 100000