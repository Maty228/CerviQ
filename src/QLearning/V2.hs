module QLearning.V2
    ( Danger(..)
    , SpaceLevel(..)
    , ForwardFoodDirection(..)
    , SideFoodDirection(..)
    , RLState(..)

    , encodeState
    , rewardForStep
    , reachableAreaAfterAction

    , v2Spec
    , QTable

    , saveQTable
    , loadQTable
    , qLearningAgent

    , describeState
    , v2DebugProvider
    ) where

import Agent
import Collision
import Maps
import Movement
import Types

import qualified Data.Set as Set
import qualified QLearning.Core as Core
import qualified QLearning.Debug as Debug

-- -----------------------------------------------------------------------------
-- RL state representation
-- -----------------------------------------------------------------------------

-- | Binary immediate-danger signal used by Q-learning version 2.
data Danger
    = Safe
    | Dangerous
    deriving (Show, Read, Eq, Ord)


-- | Coarse estimate of the reachable space after an action.
data SpaceLevel
    = Trapped
    | Tight
    | Open
    deriving (Show, Read, Eq, Ord)


-- | Forward position of the nearest food relative to the worm's orientation.
data ForwardFoodDirection
    = FoodAhead
    | FoodSameForward
    | FoodBehind
    deriving (Show, Read, Eq, Ord)


-- | Side position of the nearest food relative to the worm's orientation.
data SideFoodDirection
    = FoodLeft
    | FoodSameSide
    | FoodRight
    deriving (Show, Read, Eq, Ord)


-- | Version-2 RL state.
--
-- Unlike version 1, spatial safety is evaluated separately after each possible
-- action and food direction is represented relative to the worm's orientation.
data RLState = RLState
    {
        dangerLeft :: Danger,
        dangerStraight :: Danger,
        dangerRight :: Danger,
        spaceLeft :: SpaceLevel,
        spaceStraight :: SpaceLevel,
        spaceRight :: SpaceLevel,
        foodForward :: ForwardFoodDirection,
        foodSideways :: SideFoodDirection
    }
    deriving (Show, Read, Eq, Ord)


-- -----------------------------------------------------------------------------
-- Conversion helpers
-- -----------------------------------------------------------------------------

-- | Converts a Boolean danger flag into a Danger value.
dangerFromBool :: Bool -> Danger
dangerFromBool True = Dangerous

dangerFromBool False = Safe


-- -----------------------------------------------------------------------------
-- Relative food direction
-- -----------------------------------------------------------------------------

-- | Returns the nearest food displacement in worm-relative coordinates.
--
-- The first component is positive in front of the worm.
-- The second component is positive to the worm's right.
relativeFoodDeltas :: Worm -> Position -> (Int, Int)
relativeFoodDeltas worm (foodX, foodY) =
    case wormDirection worm of
        North -> (-dy, dx)
        East -> (dx, dy)
        South -> (dy, -dx)
        West -> (-dx, -dy)
  where
    (headX, headY) = wormHead worm
    dx = foodX - headX
    dy = foodY - headY

-- | Computes the forward relation of food to the worm.
forwardFoodDirection :: Worm -> Position -> ForwardFoodDirection
forwardFoodDirection worm food =
    case compare forwardDelta 0 of
        GT -> FoodAhead
        EQ -> FoodSameForward
        LT -> FoodBehind
  where
    (forwardDelta, _) = relativeFoodDeltas worm food


-- | Computes the sideways relation of food to the worm.
sideFoodDirection :: Worm -> Position -> SideFoodDirection
sideFoodDirection worm food =
    case compare sideDelta 0 of
        LT -> FoodLeft
        EQ -> FoodSameSide
        GT -> FoodRight
  where
    (_, sideDelta) = relativeFoodDeltas worm food
    

-- -----------------------------------------------------------------------------
-- Reachable space
-- -----------------------------------------------------------------------------

-- | Returns all four neighbouring cells of a map position.
neighbours :: Position -> [Position]
neighbours (x, y) =
    [
        (x + 1, y),
        (x - 1, y),
        (x, y + 1),
        (x, y - 1)
    ]

-- | Returns True if a cell blocks the version-2 reachable-area search.
blocksReachableArea :: GameState -> Worm -> Position -> Bool
blocksReachableArea state worm position =
    isBlocked currentMap position || isPoison currentMap position || position `Set.member` occupiedWithoutOwnHead
  where
    currentMap = gameMap state
    aliveWorms = filter wormAlive (gameWorms state)
    occupied = Set.fromList (occupiedPositions aliveWorms)
    occupiedWithoutOwnHead = Set.delete (wormHead worm) occupied


-- | Counts cells reachable from the worm's current head.
reachableArea :: GameState -> Worm -> Int
reachableArea state worm = floodFill Set.empty [wormHead worm]
  where
    floodFill visited [] = Set.size visited
    floodFill visited (position : rest)
        | position `Set.member` visited = floodFill visited rest
        | blocksReachableArea state worm position = floodFill visited rest
        | otherwise =
            let newVisited = Set.insert position visited
                newFrontier = neighbours position ++ rest
            in
                floodFill newVisited newFrontier

-- | Converts reachable area into a space category relative to worm length.
spaceFromArea :: Worm -> Int -> SpaceLevel
spaceFromArea worm area
    | area <= wormLength = Trapped
    | area <= 2 * wormLength = Tight
    | otherwise = Open
  where
    wormLength = length (wormBody worm)


-- | Replaces one worm in a list with its updated version.
replaceWorm :: Worm -> [Worm] -> [Worm]
replaceWorm updatedWorm worms =
    [
        if wormId worm == wormId updatedWorm
            then updatedWorm
            else worm
        | worm <- worms
    ]

-- | Computes the hypothetical worm state after one action.
wormAfterAction :: GameState -> Worm -> Action -> Worm
wormAfterAction state worm action =
    moveWormAfterAction grows action worm
  where
    nextHead = headAfterAction worm action
    grows = isFood (gameMap state) nextHead

-- | Computes the raw reachable area after performing one candidate action.
--
-- This is mainly used for debugging and future state representations.
-- It returns zero for immediately unsafe actions.
reachableAreaAfterAction :: GameState -> Worm -> Action -> Int
reachableAreaAfterAction state worm action
    | action `notElem` safeActions state worm = 0
    | otherwise = reachableArea hypotheticalState movedWorm

  where
    movedWorm = wormAfterAction state worm action
    hypotheticalState = state { gameWorms = replaceWorm movedWorm (gameWorms state) }

-- | Estimates reachable space after performing one candidate action.
--
-- Immediately unsafe actions are considered trapped. Safe actions are first
-- simulated and reachable space is then measured from the resulting position.
spaceAfterAction :: GameState -> Worm -> Action -> SpaceLevel
spaceAfterAction state worm action =
    spaceFromArea worm (reachableAreaAfterAction state worm action)


-- | Returns how many safe actions would be available on the next turn
-- after performing one candidate action.
--
-- Immediately unsafe actions return zero. Opponent movement is not predicted;
-- opponents remain at their current positions in the hypothetical state.
nextSafeMoveCountAfterAction :: GameState -> Worm -> Action -> Int
nextSafeMoveCountAfterAction state worm action
    | action `notElem` safeActions state worm = 0
    | otherwise = length (safeActions hypotheticalState movedWorm)

  where
    movedWorm = wormAfterAction state worm action
    hypotheticalState = state { gameWorms = replaceWorm movedWorm (gameWorms state) }

-- -----------------------------------------------------------------------------
-- State encoding
-- -----------------------------------------------------------------------------

-- | Encodes the game from one worm's perspective using version-2 features.
encodeState :: GameState -> Worm -> RLState
encodeState state worm =
    RLState
        {
            dangerLeft = dangerFromBool (TurnLeft `notElem` safe),
            dangerStraight = dangerFromBool (GoStraight `notElem` safe),
            dangerRight = dangerFromBool (TurnRight `notElem` safe),

            spaceLeft = spaceAfterAction state worm TurnLeft,
            spaceStraight = spaceAfterAction state worm GoStraight,
            spaceRight = spaceAfterAction state worm TurnRight,

            foodForward =
                case nearest of
                    Nothing -> FoodSameForward
                    Just food -> forwardFoodDirection worm food,

            foodSideways =
                case nearest of
                    Nothing -> FoodSameSide
                    Just food -> sideFoodDirection worm food
        }
  where
    safe = safeActions state worm
    nearest = nearestFood state worm


-- -----------------------------------------------------------------------------
-- Reward
-- -----------------------------------------------------------------------------

-- | Computes the distance from the worm to the nearest food.
distanceToNearestFood :: GameState -> Worm -> Maybe Int
distanceToNearestFood state worm = fmap (distance (wormHead worm)) (nearestFood state worm)


-- | Computes shaping reward for moving closer to or farther from food.
foodDistanceReward :: GameState -> Worm -> GameState -> Worm -> Double
foodDistanceReward beforeState beforeWorm afterState afterWorm
    | Core.wormFoodDelta beforeWorm afterWorm > 0 = 0
    | otherwise =
        case
            (
                distanceToNearestFood beforeState beforeWorm,
                distanceToNearestFood afterState afterWorm
            )
        of
            (Just beforeDistance, Just afterDistance)
                | afterDistance < beforeDistance -> 2
                | afterDistance > beforeDistance -> -2
                | otherwise -> 0
            _ -> 0

-- | Computes the version-2 reward for one transition.
--
-- Version 2 intentionally keeps the same reward function as version 1 so the
-- first V1/V2 comparison isolates the effect of the state representation.
rewardForStep :: GameState -> Worm -> GameState -> Worm -> Double
rewardForStep beforeState beforeWorm afterState afterWorm =
    foodReward +  killReward+ deathPenalty+ distanceReward+ timePenalty
  where
    foodReward = 50 * fromIntegral (Core.wormFoodDelta beforeWorm afterWorm)

    killReward = 10 * fromIntegral (Core.wormKillDelta beforeWorm afterWorm)

    deathPenalty =
        if Core.wormDied beforeWorm afterWorm
            then -100
            else 0

    distanceReward = foodDistanceReward beforeState beforeWorm afterState afterWorm
    timePenalty = -1


-- -----------------------------------------------------------------------------
-- V2 Q-learning specification
-- -----------------------------------------------------------------------------

-- | Complete Q-learning specification of version 2.
v2Spec :: Core.QLearningSpec RLState
v2Spec =
    Core.QLearningSpec
        {
            Core.qlEncodeState = encodeState,
            Core.qlRewardForStep = rewardForStep
        }


-- | Q-table used by Q-learning version 2.
type QTable = Core.QTable RLState


-- | Saves a version-2 Q-table.
saveQTable :: FilePath -> QTable -> IO ()
saveQTable = Core.saveQTable


-- | Loads a version-2 Q-table.
loadQTable :: FilePath -> IO QTable
loadQTable = Core.loadQTable


-- | Creates the greedy version-2 learned agent.
qLearningAgent :: QTable -> Agent
qLearningAgent = Core.qLearningAgent v2Spec

-- -----------------------------------------------------------------------------
-- V2 debugging
-- -----------------------------------------------------------------------------

-- | Converts a version-2 state into human-readable debug values.
describeState :: RLState -> [(String, String)]
describeState state =
    [
        ( "Danger L/S/R", show (dangerLeft state) ++ " / " ++ show (dangerStraight state) ++ " / " ++ show (dangerRight state) ),
        ( "Space L/S/R", show (spaceLeft state) ++ " / " ++ show (spaceStraight state) ++ " / " ++ show (spaceRight state) ),
        ( "Food forward", show (foodForward state) ),
        ( "Food sideways", show (foodSideways state) )
    ]


-- | Creates debugging information for a version-2 learned agent.
v2DebugProvider :: QTable -> Debug.AgentDebugProvider
v2DebugProvider table gameState worm =
    let
        state = encodeState gameState worm
        leftArea = reachableAreaAfterAction gameState worm TurnLeft
        straightArea = reachableAreaAfterAction gameState worm GoStraight
        rightArea = reachableAreaAfterAction gameState worm TurnRight
        leftNextMoves = nextSafeMoveCountAfterAction gameState worm TurnLeft
        straightNextMoves = nextSafeMoveCountAfterAction gameState worm GoStraight
        rightNextMoves = nextSafeMoveCountAfterAction gameState worm TurnRight
        qValues = Core.qValuesForState table state
        best = Core.bestQActions table state

    in
        Debug.AgentDebugInfo
            {
                Debug.debugStateLines =
                    describeState state
                    ++
                    [
                        ( "Area L/S/R", show leftArea ++ " / " ++ show straightArea ++ " / " ++ show rightArea ),
                        ( "Next moves L/S/R", show leftNextMoves ++ " / " ++ show straightNextMoves ++ " / " ++ show rightNextMoves),
                        ( "Length", show (length (wormBody worm)) )
                    ],

                Debug.debugQValues = qValues,
                Debug.debugBestActions = best
            }