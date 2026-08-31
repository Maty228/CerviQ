{-|
Module      : Config
Description : Global configuration values used by the CerviQ application.

This module contains simple project-wide constants such as the conventional
player worm identifier, the maintained food count, and the tick delay used by
loops that rely on an explicit timing interval. It does not contain any game
logic.
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


-- | Maximum number of food tiles automatically maintained during a game.
maxFoodCount :: Int
maxFoodCount = 1


-- | Fixed delay between ticks, in microseconds, for game loops that use an
-- explicit delay.
tickDelay :: Int
tickDelay = 100000