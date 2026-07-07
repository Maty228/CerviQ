module Input where

import Types
import Movement

-- -----------------------------------------------------------------------------
-- Keyboard input
-- -----------------------------------------------------------------------------

-- | Converts a keyboard key into the corresponding direction.
--
-- Unsupported keys return Nothing.
charToDirection :: Char -> Maybe Direction
charToDirection 'w' = Just North
charToDirection 's' = Just South
charToDirection 'a' = Just West
charToDirection 'd' = Just East
charToDirection _ = Nothing

-- | Converts player keyboard input into a game action.
--
-- Invalid input or illegal 180-degree turns result in GoStraight.
playerActionFromInput :: Worm -> String -> Action
playerActionFromInput worm input =
    case input of
        c : _ ->
            let desiredDirection = charToDirection c
                action = desiredDirection >>= directionToAction (wormDirection worm)
            in maybe GoStraight id action

        [] -> GoStraight
