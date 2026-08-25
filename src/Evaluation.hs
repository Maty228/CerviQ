module Evaluation where

import Config
import Controller
import Collision
import Game
import Types
import Agent(safeGreedyFoodAgent)

import Data.List(sort)



-- -----------------------------------------------------------------------------
-- Episode results
-- -----------------------------------------------------------------------------

-- | Summary of one completed episode.
data EpisodeResult = EpisodeResult
    {
        episodeTicks :: Int,
        episodeFinalWorms :: [Worm],
        episodeLastStandingWormIds :: [Int],
        episodeDeaths :: [(Int, DeathReason)]
    }
    deriving (Show, Eq)


-- | Aggregated statistics over multiple episodes.
data EvaluationSummary = EvaluationSummary
    {
        evaluatedEpisodes :: Int,
        avgFoodEaten :: Double,
        avgEpisodeLength :: Double,
        avgKills :: Double,
        survivalRate :: Double
    }
    deriving (Show, Eq)


-- | Aggregated statistics for one specific worm across multiple episodes.
data WormEvaluationSummary = WormEvaluationSummary
    {
        evaluatedWormId :: Int,
        wormEvaluatedEpisodes :: Int,
        wormAvgFoodEaten :: Double,
        wormFoodPer100Ticks :: Double,
        wormAvgAge :: Double,
        wormMedianAge :: Double,
        wormP90Age :: Int,
        wormMaxAge :: Int,
        wormAvgKills :: Double,
        wormAvgFinalLength :: Double,
        wormMaxFinalLength :: Int,
        wormSurvivalRate :: Double,
        wormLastStandingRate :: Double,
        wormWallDeathRate :: Double,
        wormPoisonDeathRate :: Double,
        wormOwnBodyDeathRate :: Double,
        wormOtherBodyDeathRate :: Double,
        wormHeadToHeadDeathRate :: Double
    }
    deriving (Show, Eq)


-- | Computes summary statistics for a collection of episode results.
summarizeResults :: [EpisodeResult] -> EvaluationSummary
summarizeResults results =
    EvaluationSummary
        {
            evaluatedEpisodes = length results,
            avgFoodEaten = averageFoodEaten results,
            avgEpisodeLength = averageEpisodeLength results,
            avgKills = averageKills results,
            survivalRate = averageSurvivalRate results
        }


-- | Computes summary statistics for one specific worm.
summarizeWormResults :: Int -> [EpisodeResult] -> WormEvaluationSummary
summarizeWormResults targetId results =
    WormEvaluationSummary
        {
            evaluatedWormId = targetId,
            wormEvaluatedEpisodes = length results,
            wormAvgFoodEaten = averageFoodEatenByWorm targetId results,
            wormFoodPer100Ticks = foodPer100TicksByWorm targetId results,
            wormAvgAge = averageAgeByWorm targetId results,
            wormMedianAge = medianAgeByWorm targetId results,
            wormP90Age = p90AgeByWorm targetId results,
            wormMaxAge = maxAgeByWorm targetId results,
            wormAvgKills = averageKillsByWorm targetId results,
            wormAvgFinalLength = averageFinalLengthByWorm targetId results,
            wormMaxFinalLength = maxFinalLengthByWorm targetId results,
            wormSurvivalRate = survivalRateByWorm targetId results,
            wormLastStandingRate = lastStandingRateByWorm targetId results,
            wormWallDeathRate = wallDeathRateByWorm targetId results,
            wormPoisonDeathRate = poisonDeathRateByWorm targetId results,
            wormOwnBodyDeathRate = ownBodyDeathRateByWorm targetId results,
            wormOtherBodyDeathRate = otherBodyDeathRateByWorm targetId results,
            wormHeadToHeadDeathRate = headToHeadDeathRateByWorm targetId results
        }


-- | Returns the ID of the only living worm if exactly one worm is alive.
lastStandingWormId :: GameState -> Maybe Int
lastStandingWormId state =
    case filter wormAlive (gameWorms state) of
        [worm] -> Just (wormId worm)
        _ -> Nothing


-- | Records a worm if it has become the only living worm in the episode.
recordLastStandingWorm :: GameState -> [Int] -> [Int]
recordLastStandingWorm state recordedIds =
    case lastStandingWormId state of
        Nothing -> recordedIds

        Just targetId ->
            if targetId `elem` recordedIds
                then recordedIds
                else targetId : recordedIds



-- -----------------------------------------------------------------------------
-- Episode simulation
-- -----------------------------------------------------------------------------

-- | Returns True if the episode should continue running

episodeRunning :: Int -> GameState -> Bool
episodeRunning maxTicks state =
    gameTick state < maxTicks && any wormAlive (gameWorms state)



-- | Runs a single episode until all worms are dead or the tick limit is reached.
runEpisode :: Int -> [(Int, Controller)] -> GameState -> IO EpisodeResult
runEpisode maxTicks assignedControllers initialState = do
    stateWithFood <- maintainFoodCount maxFoodCount initialState
    (finalState, lastStandingIds, deaths) <- runLoop stateWithFood [] []

    pure
        EpisodeResult
            {
                episodeTicks = gameTick finalState,
                episodeFinalWorms = gameWorms finalState,
                episodeLastStandingWormIds = lastStandingIds,
                episodeDeaths = deaths
            }
  where

    -- | Runs game ticks while recording last-standing worms and deaths.
    runLoop :: GameState -> [Int] -> [(Int, DeathReason)] -> IO (GameState, [Int], [(Int, DeathReason)])
    runLoop state lastStandingIds deaths
        | not (episodeRunning maxTicks state) =
            pure (state, updatedLastStandingIds, deaths)

        | otherwise = do
            actions <- collectActions assignedControllers state

            let stepResult = stepGameDetailed actions state
                steppedState = gameStepState stepResult
                updatedDeaths = deaths ++ gameStepDeaths stepResult

            stateWithFood <- maintainFoodCount maxFoodCount steppedState

            runLoop
                stateWithFood
                updatedLastStandingIds
                updatedDeaths
      where
        updatedLastStandingIds =
            recordLastStandingWorm state lastStandingIds


-- | Runs multiple independent episodes using the same initial state.
runEpisodes :: Int -> Int -> [(Int, Controller)] -> GameState -> IO [EpisodeResult]
runEpisodes episodeCount maxTicks assignedControllers initialState =
    mapM
        (\_ -> runEpisode maxTicks assignedControllers initialState) [1 .. episodeCount]



-- -----------------------------------------------------------------------------
-- Metrics
-- -----------------------------------------------------------------------------

-- | Returns the total number of food items eaten in one episode
totalFoodEaten :: EpisodeResult -> Int
totalFoodEaten result =
    sum (map (foodEaten . wormStats) (episodeFinalWorms result))


-- | Returns the average number of food items eaten per episode
averageFoodEaten :: [EpisodeResult] -> Double
averageFoodEaten results = average (map totalFoodEaten results)


-- | Returns the amount of food eaten by the selected worm in one episode.
foodEatenByWorm :: Int -> EpisodeResult -> Int
foodEatenByWorm targetId result =
    sum
        [ foodEaten (wormStats worm) | worm <- episodeFinalWorms result,
            wormId worm == targetId
        ]


-- | Returns the average amount of food eaten by the selected worm.
averageFoodEatenByWorm :: Int -> [EpisodeResult] -> Double
averageFoodEatenByWorm targetId results =
    average (map (foodEatenByWorm targetId) results)


-- | Returns the amount of food eaten per 100 ticks lived by the selected worm.
foodPer100TicksByWorm :: Int -> [EpisodeResult] -> Double
foodPer100TicksByWorm targetId results
    | totalTicks == 0 = 0
    | otherwise =
        100 * fromIntegral totalFood / fromIntegral totalTicks
  where
    totalFood =
        sum (map (foodEatenByWorm targetId) results)

    totalTicks =
        sum (map (ageByWorm targetId) results)

-- | Returns the age reached by the selected worm in one episode.
ageByWorm :: Int -> EpisodeResult -> Int
ageByWorm targetId result =
    sum
        [ age (wormStats worm) | worm <- episodeFinalWorms result
        , wormId worm == targetId
        ]


-- | Returns the average age reached by the selected worm.
averageAgeByWorm :: Int -> [EpisodeResult] -> Double
averageAgeByWorm targetId results =
    average (map (ageByWorm targetId) results)

-- | Returns the median age reached by the selected worm.
medianAgeByWorm :: Int -> [EpisodeResult] -> Double
medianAgeByWorm targetId results =
    median (map (ageByWorm targetId) results)


-- | Returns the 90th percentile of ages reached by the selected worm.
p90AgeByWorm :: Int -> [EpisodeResult] -> Int
p90AgeByWorm targetId results =
    percentile 0.9 (map (ageByWorm targetId) results)


-- | Returns the maximum age reached by the selected worm.
maxAgeByWorm :: Int -> [EpisodeResult] -> Int
maxAgeByWorm targetId results =
    maximumOrZero (map (ageByWorm targetId) results)


-- | Returns the number of kills by the selected worm in one episode.
killsByWorm :: Int -> EpisodeResult -> Int
killsByWorm targetId result =
    sum
        [ kills (wormStats worm) | worm <- episodeFinalWorms result
        , wormId worm == targetId
        ]


-- | Returns the average number of kills by the selected worm.
averageKillsByWorm :: Int -> [EpisodeResult] -> Double
averageKillsByWorm targetId results =
    average (map (killsByWorm targetId) results)


-- | Returns the final body length of the selected worm in one episode.
finalLengthByWorm :: Int -> EpisodeResult -> Int
finalLengthByWorm targetId result =
    sum
        [ length (wormBody worm)
        | worm <- episodeFinalWorms result
        , wormId worm == targetId
        ]


-- | Returns the average final body length of the selected worm.
averageFinalLengthByWorm :: Int -> [EpisodeResult] -> Double
averageFinalLengthByWorm targetId results =
    average (map (finalLengthByWorm targetId) results)


-- | Returns the maximum final body length reached by the selected worm.
maxFinalLengthByWorm :: Int -> [EpisodeResult] -> Int
maxFinalLengthByWorm targetId results =
    maximumOrZero (map (finalLengthByWorm targetId) results)


-- | Returns True if the selected worm survived until the end of one episode.
wormSurvivedEpisode :: Int -> EpisodeResult -> Bool
wormSurvivedEpisode targetId result =
    any
        (\worm -> wormId worm == targetId && wormAlive worm)
        (episodeFinalWorms result)


-- | Returns the fraction of episodes survived by the selected worm.
survivalRateByWorm :: Int -> [EpisodeResult] -> Double
survivalRateByWorm targetId results =
    average
        [ if wormSurvivedEpisode targetId result then 1 else 0
        | result <- results
        ]

-- | Returns True if the selected worm became the only living worm during an episode.
wormWasLastStanding :: Int -> EpisodeResult -> Bool
wormWasLastStanding targetId result =
    targetId `elem` episodeLastStandingWormIds result


-- | Returns the fraction of episodes in which the selected worm became the only living worm.
lastStandingRateByWorm :: Int -> [EpisodeResult] -> Double
lastStandingRateByWorm targetId results =
    average
        [ if wormWasLastStanding targetId result then 1 else 0
        | result <- results
        ]



-- | Returns the average number of ticks survived per episode
averageEpisodeLength :: [EpisodeResult] -> Double
averageEpisodeLength results = average (map episodeTicks results)


-- | Returns the total number of kills in one episode.
totalKills :: EpisodeResult -> Int
totalKills result = sum (map (kills . wormStats) (episodeFinalWorms result))


-- | Returns True if at least one worm survived until the end of the episode.
episodeHasSurvivor :: EpisodeResult -> Bool
episodeHasSurvivor result = any wormAlive (episodeFinalWorms result)


-- | Returns the average number of kills per episode.
averageKills :: [EpisodeResult] -> Double
averageKills results = average (map totalKills results)


-- | Returns the fraction of episodes where at least one worm survived.
averageSurvivalRate :: [EpisodeResult] -> Double
averageSurvivalRate results =
    average
        [ if episodeHasSurvivor result then 1 else 0 | result <- results]


-- | Computes the average of a list of integer values.
average :: [Int] -> Double
average [] = 0
average values = fromIntegral (sum values) / fromIntegral (length values)

-- | Computes the median of a list of integer values.
median :: [Int] -> Double
median [] = 0
median values
    | odd valueCount =
        fromIntegral (sortedValues !! middleIndex)

    | otherwise =
        fromIntegral
            (sortedValues !! (middleIndex - 1) + sortedValues !! middleIndex)
            / 2
  where
    sortedValues = sort values
    valueCount = length sortedValues
    middleIndex = valueCount `div` 2


-- | Returns an observed value at the requested percentile.
percentile :: Double -> [Int] -> Int
percentile _ [] = 0
percentile requestedPercentile values =
    sortedValues !! percentileIndex
  where
    sortedValues = sort values

    boundedPercentile =
        max 0 (min 1 requestedPercentile)

    percentileIndex =
        floor
            (boundedPercentile * fromIntegral (length sortedValues - 1))


-- | Returns the maximum value or zero for an empty list.
maximumOrZero :: [Int] -> Int
maximumOrZero [] = 0
maximumOrZero values = maximum values


-- -----------------------------------------------------------------------------
-- Death reason metrics
-- -----------------------------------------------------------------------------

-- | Returns the death reason of the selected worm in one episode, if it died.
deathReasonByWorm :: Int -> EpisodeResult -> Maybe DeathReason
deathReasonByWorm targetId result =
    lookup targetId (episodeDeaths result)


-- | Returns the fraction of episodes where the selected worm died for a reason
-- matching the given condition.
deathReasonRateByWorm :: (DeathReason -> Bool) -> Int -> [EpisodeResult] -> Double
deathReasonRateByWorm matchesReason targetId results =
    average
        [ case deathReasonByWorm targetId result of
            Just reason ->
                if matchesReason reason
                    then 1
                    else 0

            Nothing ->
                0

        | result <- results
        ]


-- | Returns the fraction of episodes where the selected worm hit a wall.
wallDeathRateByWorm :: Int -> [EpisodeResult] -> Double
wallDeathRateByWorm targetId results =
    deathReasonRateByWorm
        isWallDeath
        targetId
        results
  where

    -- | Returns True for wall collision deaths.
    isWallDeath :: DeathReason -> Bool
    isWallDeath HitWall = True
    isWallDeath _ = False


-- | Returns the fraction of episodes where the selected worm hit poison.
poisonDeathRateByWorm :: Int -> [EpisodeResult] -> Double
poisonDeathRateByWorm targetId results =
    deathReasonRateByWorm
        isPoisonDeath
        targetId
        results 
  where

    -- | Returns True for poison deaths.
    isPoisonDeath :: DeathReason -> Bool
    isPoisonDeath HitPoison = True
    isPoisonDeath _ = False


-- | Returns the fraction of episodes where the selected worm hit its own body.
ownBodyDeathRateByWorm :: Int -> [EpisodeResult] -> Double
ownBodyDeathRateByWorm targetId results =
    deathReasonRateByWorm
        isOwnBodyDeath
        targetId
        results
  where

    -- | Returns True for own-body collision deaths.
    isOwnBodyDeath :: DeathReason -> Bool
    isOwnBodyDeath HitOwnBody = True
    isOwnBodyDeath _ = False


-- | Returns the fraction of episodes where the selected worm hit another worm's body.
otherBodyDeathRateByWorm :: Int -> [EpisodeResult] -> Double
otherBodyDeathRateByWorm targetId results =
    deathReasonRateByWorm
        isOtherBodyDeath
        targetId
        results
  where

    -- | Returns True for collisions with another worm's body.
    isOtherBodyDeath :: DeathReason -> Bool
    isOtherBodyDeath (HitOtherBody _) = True
    isOtherBodyDeath _ = False


-- | Returns the fraction of episodes where the selected worm died head-to-head.
headToHeadDeathRateByWorm :: Int -> [EpisodeResult] -> Double
headToHeadDeathRateByWorm targetId results =
    deathReasonRateByWorm
        isHeadToHeadDeath
        targetId
        results
  where

    -- | Returns True for head-to-head deaths.
    isHeadToHeadDeath :: DeathReason -> Bool
    isHeadToHeadDeath (HeadToHead _) = True
    isHeadToHeadDeath _ = False


-- | Summary of one evaluated agent including its name and statistics.
data AgentEvaluation = AgentEvaluation
    {
        evaluatedAgentName :: String,
        evaluatedAgentId :: Int,
        evaluationSummary :: EvaluationSummary,
        wormSummary :: WormEvaluationSummary
    }
    deriving (Show, Eq)


-- | Evaluates one agent against the default test environment.
--
-- Runs multiple episodes and returns aggregated statistics for the agent.
evaluateAgent :: String -> Int -> Controller -> Int -> GameState -> IO AgentEvaluation
evaluateAgent name agentId controller episodes initialState = do
    results <-
        runEpisodes
            episodes
            500
            [
                (agentId, controller),
                (2, AI safeGreedyFoodAgent)
            ]
            initialState

    pure
        AgentEvaluation
            {
                evaluatedAgentName = name,
                evaluatedAgentId = agentId,
                evaluationSummary = summarizeResults results,
                wormSummary = summarizeWormResults agentId results
            }