module Game where

import Types
import Maps

--
-- Direction and position helpers
--

turnLeft :: Direction -> Direction
turnLeft North = West
turnLeft East = North
turnLeft South = East
turnLeft West = South


turnRight :: Direction -> Direction
turnRight North = East
turnRight East = South
turnRight South = West
turnRight West = North


moveForward :: Position -> Direction -> Position
moveForward (x,y) North = (x, y-1)
moveForward (x,y) East = (x+1, y)
moveForward (x,y) South = (x, y+1)
moveForward (x,y) West = (x-1, y)


charToDirection :: Char -> Maybe Direction
charToDirection 'w' = Just North
charToDirection 's' = Just South
charToDirection 'a' = Just West
charToDirection 'd' = Just East
charToDirection _ = Nothing


directionToAction :: Direction -> Direction -> Maybe Action
directionToAction current desired
    | desired == current = Just GoStraight
    | desired == turnLeft current = Just TurnLeft
    | desired == turnRight current = Just TurnRight
    | otherwise = Nothing


{-
Using Image coordination system:
0,0 1,0 2,0 3,0 4,0
0,1 1,1 2,1 3,1 4,1
0,2 1,2 2,2 3,2 4,2
0,3 1,3 2,3 3,3 4,3
0,4 1,4 2,4 3,4 4,4
-}


--
-- Worm helpers
--


-- Returns the head position of a worm
-- An alive worm always has a non-empty body and the first position is its head
wormHead :: Worm -> Position
wormHead worm = 
    case wormBody worm of
        headPosition : _ -> headPosition
        [] -> error "wormHead: wormBody is empty"


applyAction :: Direction -> Action -> Direction
applyAction dir TurnLeft = turnLeft dir
applyAction dir TurnRight = turnRight dir
applyAction dir GoStraight = dir


nextHeadPosition :: Worm -> Position
nextHeadPosition worm = moveForward (wormHead worm) (wormDirection worm)



removeTail :: [Position] -> [Position]
removeTail [_] = []
removeTail (segment : rest) = segment : removeTail rest
removeTail [] = []

--  | Build a new worm body after moving to a new head position
-- If the worm grows, the tail is kept, otherwise the last body segment is removed
advanceBody :: Bool -> Position -> [Position] -> [Position]
advanceBody grows newHead oldBody =
    case oldBody of
        [] -> [newHead]
        _
            | grows -> newHead : oldBody
            | otherwise -> newHead : removeTail oldBody


moveWorm :: Bool -> Worm -> Worm
moveWorm grows worm =
    worm {wormBody = advanceBody grows (nextHeadPosition worm) (wormBody worm)}


moveWormAfterAction :: Bool -> Action -> Worm -> Worm
moveWormAfterAction grows action worm =
    let newDirection = applyAction (wormDirection worm) action
        turnedWorm = worm {wormDirection = newDirection}
    in moveWorm grows turnedWorm


--
-- Map collision helpers
--


isBlocked :: GameMap -> Position -> Bool
isBlocked gamemap pos =
    case tileAt gamemap pos of
        Nothing -> True
        Just tile -> tile == Wall 


wouldHitWall :: GameMap -> Worm -> Bool
wouldHitWall gamemap worm =
    isBlocked gamemap (nextHeadPosition worm)


wouldHitWallAfterAction :: GameMap -> Action -> Worm -> Bool
wouldHitWallAfterAction gamemap action worm =
    let newDirection = applyAction (wormDirection worm) action
        turnedWorm = worm {wormDirection = newDirection}
    in wouldHitWall gamemap turnedWorm


occupiedPositions :: [Worm] -> [Position]
occupiedPositions worms =
    concatMap wormBody worms


futureWorms :: [(Bool, Action, Worm)] -> [Worm]
futureWorms moves =
    map (\(grows, action, worm) -> moveWormAfterAction grows action worm) moves

futureOccupiedPositions :: [(Bool, Action, Worm)] -> [Position]
futureOccupiedPositions moves =
    occupiedPositions (futureWorms moves)


countPosition :: Position -> [Position] -> Int
countPosition pos positions =
    length (filter (== pos) positions)

headCollision :: [Position] -> Worm -> Bool
headCollision positions worm =
    countPosition (wormHead worm) positions > 1

positionOccupied :: Position -> [Position] -> Bool
positionOccupied pos positions =
    pos `elem` positions

wormCollidesWithBodies :: [Worm] -> Worm -> Bool
wormCollidesWithBodies worms worm =
    headCollision (occupiedPositions worms) worm

wormCollidesWithMap :: GameMap -> Worm -> Bool
wormCollidesWithMap gamemap worm =
    isBlocked gamemap (wormHead worm)


wormCollides :: GameMap -> [Worm] -> Worm -> Bool
wormCollides gamemap worms worm =
    wormCollidesWithMap gamemap worm ||
    wormCollidesWithBodies worms worm



wormWillEatFood :: GameMap -> Action -> Worm -> Bool
wormWillEatFood gamemap action worm =
    let newDirection = applyAction (wormDirection worm) action
        turnedWorm = worm {wormDirection = newDirection}
        newHead = nextHeadPosition turnedWorm
    in isFood gamemap newHead


removeEatenFood :: [Worm] -> GameMap -> GameMap
removeEatenFood worms currentMap = 
    foldr removeFood currentMap (map wormHead worms)


isFreeForFood :: GameState -> Position -> Bool
isFreeForFood state pos =
    isEmptyTile stateMap pos &&
    not (positionOccupied pos wormPositions)
  where
    stateMap = gameMap state
    wormPositions = occupiedPositions (filter wormAlive (gameWorms state))


freeFoodPositions :: GameState -> [Position]
freeFoodPositions state = filter (isFreeForFood state) (allMapPositions (gameMap state))

spawnFoodAt :: Position -> GameState -> GameState
spawnFoodAt pos state =
    state {gameMap = placeFood pos (gameMap state)}


simulateTurn :: GameMap -> [(Bool, Action, Worm)] -> ([Worm], [Worm])
simulateTurn gamemap moves =
    let movedWorms = futureWorms moves
        collidingWorms = filter (wormCollides gamemap movedWorms) movedWorms
        survivingWorms = filter (not . wormCollides gamemap movedWorms) movedWorms
    in (survivingWorms, collidingWorms)



actionForWorm :: [(Int, Action)] -> Worm -> Action
actionForWorm actions worm =
    case lookup (wormId worm) actions of
        Just action -> action
        Nothing -> GoStraight


playerActionFromInput :: Worm -> String -> Action
playerActionFromInput worm input =
    case input of
        c : _ -> case charToDirection c of
            Just desiredDirection -> case directionToAction (wormDirection worm) desiredDirection of
                Just action -> action
                Nothing -> GoStraight
            Nothing -> GoStraight
        [] -> GoStraight


increaseAge :: Worm -> Worm
increaseAge worm =
    worm {
        wormStats =
            (wormStats worm) {age = age (wormStats worm) + 1}
    }

increaseFoodEaten :: Worm -> Worm
increaseFoodEaten worm =
    worm {
        wormStats =
            (wormStats worm) {foodEaten = foodEaten (wormStats worm) + 1}
    }

wormAteFood :: GameMap -> Worm -> Bool
wormAteFood currentMap worm =
    isFood currentMap (wormHead worm)

stepGame :: [(Int, Action)] -> GameState -> GameState
stepGame actions state =
    let currentMap = gameMap state
        alive = filter wormAlive (gameWorms state)
        alreadyDead = filter (not . wormAlive) (gameWorms state)

        moves =
            map
                (\worm ->
                    let action = actionForWorm actions worm
                        grows = wormWillEatFood currentMap action worm
                    in (grows, action, worm)
                )
                alive
    
        (survivors, collided) =
            simulateTurn currentMap moves
        
        newMap = removeEatenFood survivors currentMap

        updatedSurvivors =
            map
                (\worm ->
                    let agedWorm = increaseAge worm
                    in
                        if wormAteFood currentMap worm
                            then increaseFoodEaten agedWorm
                            else agedWorm
                )
                survivors

        updatedCollided =
            map
                (\worm ->
                    (increaseAge worm) { wormAlive = False }
                )
                collided

        updatedWorms =
            updatedSurvivors ++ updatedCollided ++ alreadyDead
    in 
        state
            {
                gameMap = newMap,
                gameWorms = updatedWorms,
                gameTick = gameTick state + 1
            }