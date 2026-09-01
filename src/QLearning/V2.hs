{-|
Module      : QLearning.V2
Description : Second tabular Q-learning state representation for CerviQ.

Version 2 redesigns the V1 state while intentionally keeping the same reward
function. Food direction becomes relative to the worm's orientation, and
reachable space is evaluated separately after each possible action rather than
as one global value.

Keeping the reward unchanged makes the V1/V2 comparison primarily a comparison
of state representations. The generic Q-learning algorithm remains in
"QLearning.Core".
-}

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


-- | Coarse estimate of the reachable space after one candidate action.
data SpaceLevel
    = Trapped
    | Tight
    | Open
    deriving (Show, Read, Eq, Ord)


-- | Position of the nearest food along the worm's forward/backward axis.
data ForwardFoodDirection
    = FoodAhead
    | FoodSameForward
    | FoodBehind
    deriving (Show, Read, Eq, Ord)


-- | Position of the nearest food along the worm's left/right axis.
data SideFoodDirection
    = FoodLeft
    | FoodSameSide
    | FoodRight
    deriving (Show, Read, Eq, Ord)


-- | Version-2 state used as the key of the tabular Q-table.
--
-- Unlike V1, food direction is relative to the worm's orientation and the
-- available space is evaluated separately after turning left, continuing
-- straight, or turning right.
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
-- State conversion helpers
-- -----------------------------------------------------------------------------

-- | Converts a Boolean danger flag into the V2 danger category.
dangerFromBool :: Bool -> Danger
dangerFromBool True = Dangerous
dangerFromBool False = Safe


-- -----------------------------------------------------------------------------
-- Relative food direction
-- -----------------------------------------------------------------------------

-- | Converts the displacement to food into worm-relative coordinates.
--
-- The first returned component is positive in front of the worm and negative
-- behind it. The second is positive to the worm's right and negative to its
-- left.
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


-- | Returns the nearest food's forward/backward relation to the worm.
forwardFoodDirection :: Worm -> Position -> ForwardFoodDirection
forwardFoodDirection worm food =
    case compare forwardDelta 0 of
        GT -> FoodAhead
        EQ -> FoodSameForward
        LT -> FoodBehind
  where
    (forwardDelta, _) = relativeFoodDeltas worm food


-- | Returns the nearest food's left/right relation to the worm.
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

-- | Returns all four orthogonal neighbours of a map position.
neighbours :: Position -> [Position]
neighbours (x, y) =
    [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]


-- | Returns whether a position blocks the V2 reachable-area search.
--
-- The controlled worm's current head is removed from the occupied set so it
-- can serve as the flood-fill starting position.
blocksReachableArea :: GameState -> Worm -> Position -> Bool
blocksReachableArea state worm position =
    isBlocked currentMap position
        || isPoison currentMap position
        || position `Set.member` occupiedWithoutOwnHead
  where
    currentMap = gameMap state
    aliveWorms = filter wormAlive (gameWorms state)
    occupied = Set.fromList (occupiedPositions aliveWorms)
    occupiedWithoutOwnHead = Set.delete (wormHead worm) occupied


-- | Counts cells reachable from the worm's current head using flood fill.
reachableArea :: GameState -> Worm -> Int
reachableArea state worm =
    floodFill Set.empty [wormHead worm]
  where
    floodFill visited [] = Set.size visited
    floodFill visited (position : rest)
        | position `Set.member` visited = floodFill visited rest
        | blocksReachableArea state worm position = floodFill visited rest
        | otherwise =
            let newVisited = Set.insert position visited
                newFrontier = neighbours position ++ rest
            in floodFill newVisited newFrontier


-- | Converts reachable area into a category relative to the worm's length.
--
-- Unlike V1's fixed thresholds, V2 scales its categories with body length.
spaceFromArea :: Worm -> Int -> SpaceLevel
spaceFromArea worm area
    | area <= wormLength = Trapped
    | area <= 2 * wormLength = Tight
    | otherwise = Open
  where
    wormLength = length (wormBody worm)


-- | Replaces one worm in a complete worm list with a hypothetical version.
replaceWorm :: Worm -> [Worm] -> [Worm]
replaceWorm updatedWorm worms =
    [ if wormId worm == wormId updatedWorm then updatedWorm else worm
    | worm <- worms
    ]


-- | Simulates the controlled worm after one candidate action.
--
-- Growth is included when the candidate head position currently contains food.
-- Other worms are not moved.
wormAfterAction :: GameState -> Worm -> Action -> Worm
wormAfterAction state worm action =
    moveWormAfterAction grows action worm
  where
    nextHead = headAfterAction worm action
    grows = isFood (gameMap state) nextHead


-- | Computes raw reachable area after performing one candidate action.
--
-- Immediately unsafe actions return zero. Otherwise the controlled worm is
-- replaced by its hypothetical moved version and flood fill is performed in
-- that resulting local state. Opponent movement is not predicted.
reachableAreaAfterAction :: GameState -> Worm -> Action -> Int
reachableAreaAfterAction state worm action
    | action `notElem` safeActions state worm = 0
    | otherwise = reachableArea hypotheticalState movedWorm
  where
    movedWorm = wormAfterAction state worm action
    hypotheticalState = state {gameWorms = replaceWorm movedWorm (gameWorms state)}


-- | Converts reachable area after one candidate action into a space category.
--
-- Every immediately unsafe action is therefore categorized as 'Trapped'.
spaceAfterAction :: GameState -> Worm -> Action -> SpaceLevel
spaceAfterAction state worm action =
    spaceFromArea worm (reachableAreaAfterAction state worm action)


-- | Returns how many immediately safe actions would remain on the next turn.
--
-- This value is used only for diagnostics, not as a field of the V2 Q-learning
-- state. Opponents remain at their current positions in the hypothetical state.
nextSafeMoveCountAfterAction :: GameState -> Worm -> Action -> Int
nextSafeMoveCountAfterAction state worm action
    | action `notElem` safeActions state worm = 0
    | otherwise = length (safeActions hypotheticalState movedWorm)
  where
    movedWorm = wormAfterAction state worm action
    hypotheticalState = state {gameWorms = replaceWorm movedWorm (gameWorms state)}


-- -----------------------------------------------------------------------------
-- State encoding
-- -----------------------------------------------------------------------------

-- | Encodes the current game situation from one worm's V2 perspective.
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

-- | Returns Manhattan distance from the worm to the nearest food.
distanceToNearestFood :: GameState -> Worm -> Maybe Int
distanceToNearestFood state worm =
    fmap (distance (wormHead worm)) (nearestFood state worm)


-- | Computes reward shaping for progress towards the nearest food.
--
-- Eating already has its own larger reward, so distance shaping is suppressed
-- on transitions where food was consumed.
foodDistanceReward :: GameState -> Worm -> GameState -> Worm -> Double
foodDistanceReward beforeState beforeWorm afterState afterWorm
    | Core.wormFoodDelta beforeWorm afterWorm > 0 = 0
    | otherwise =
        case (distanceToNearestFood beforeState beforeWorm, distanceToNearestFood afterState afterWorm) of
            (Just beforeDistance, Just afterDistance)
                | afterDistance < beforeDistance -> 2
                | afterDistance > beforeDistance -> -2
                | otherwise -> 0
            _ -> 0


-- | Computes the V2 reward for one transition.
--
-- V2 intentionally keeps exactly the same shaped reward as V1 so their
-- comparison primarily measures the effect of the redesigned state
-- representation.
rewardForStep :: GameState -> Worm -> GameState -> Worm -> Double
rewardForStep beforeState beforeWorm afterState afterWorm =
    foodReward + killReward + deathPenalty + distanceReward + timePenalty
  where
    foodReward = 50 * fromIntegral (Core.wormFoodDelta beforeWorm afterWorm)
    killReward = 10 * fromIntegral (Core.wormKillDelta beforeWorm afterWorm)
    deathPenalty = if Core.wormDied beforeWorm afterWorm then -100 else 0
    distanceReward = foodDistanceReward beforeState beforeWorm afterState afterWorm
    timePenalty = -1


-- -----------------------------------------------------------------------------
-- V2 Q-learning specification
-- -----------------------------------------------------------------------------

-- | Complete version-specific specification passed to the shared Q-learning core.
v2Spec :: Core.QLearningSpec RLState
v2Spec =
    Core.QLearningSpec
        {
            Core.qlEncodeState = encodeState,
            Core.qlRewardForStep = rewardForStep
        }


-- | Q-table whose keys use the V2 state representation.
type QTable = Core.QTable RLState


-- | Saves a V2 Q-table.
saveQTable :: FilePath -> QTable -> IO ()
saveQTable = Core.saveQTable


-- | Loads a V2 Q-table.
loadQTable :: FilePath -> IO QTable
loadQTable = Core.loadQTable


-- | Creates a greedy learned agent from a V2 Q-table.
qLearningAgent :: QTable -> Agent
qLearningAgent = Core.qLearningAgent v2Spec


-- -----------------------------------------------------------------------------
-- V2 debugging
-- -----------------------------------------------------------------------------

-- | Converts a V2 encoded state into human-readable diagnostic values.
describeState :: RLState -> [(String, String)]
describeState state =
    [ ("Danger L/S/R", show (dangerLeft state) ++ " / " ++ show (dangerStraight state) ++ " / " ++ show (dangerRight state))
    , ("Space L/S/R", show (spaceLeft state) ++ " / " ++ show (spaceStraight state) ++ " / " ++ show (spaceRight state))
    , ("Food forward", show (foodForward state))
    , ("Food sideways", show (foodSideways state))
    ]


-- | Creates version-independent GUI diagnostics for a V2 learned agent.
--
-- In addition to the encoded state and Q-values, the V2 debugger exposes raw
-- reachable areas and the number of safe next moves. These extra values help
-- inspect the state abstraction but are not part of the learned state itself.
v2DebugProvider :: QTable -> Debug.AgentDebugProvider
v2DebugProvider table gameState worm =
    Debug.AgentDebugInfo
        {
            Debug.debugStateLines =
                describeState state
                    ++ [ ("Area L/S/R", show leftArea ++ " / " ++ show straightArea ++ " / " ++ show rightArea)
                       , ("Next moves L/S/R", show leftNextMoves ++ " / " ++ show straightNextMoves ++ " / " ++ show rightNextMoves)
                       , ("Length", show (length (wormBody worm)))
                       ],
            Debug.debugQValues = Core.qValuesForState table state,
            Debug.debugBestActions = Core.bestQActions table state
        }
  where
    state = encodeState gameState worm

    leftArea = reachableAreaAfterAction gameState worm TurnLeft
    straightArea = reachableAreaAfterAction gameState worm GoStraight
    rightArea = reachableAreaAfterAction gameState worm TurnRight

    leftNextMoves = nextSafeMoveCountAfterAction gameState worm TurnLeft
    straightNextMoves = nextSafeMoveCountAfterAction gameState worm GoStraight
    rightNextMoves = nextSafeMoveCountAfterAction gameState worm TurnRight