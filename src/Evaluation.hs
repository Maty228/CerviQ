{-|
Module      : Evaluation
Description : Episode simulation and quantitative evaluation of CerviQ agents.

This module runs controlled game episodes and computes both game-wide and
worm-specific evaluation metrics. Episodes use the normal CerviQ game engine,
controller infrastructure, collision detection, food spawning, and death
reasons.

Besides aggregate food, kills, and episode duration, the evaluator records
per-worm survival, age, final length, last-standing events, and individual
collision causes. This allows learned agents and heuristic baselines to be
compared using more than a single reward or win metric.
-}

module Evaluation where

import Agent (safeGreedyFoodAgent)
import Collision
import Config
import Controller
import Game
import Types

import Data.List (sort)


-- -----------------------------------------------------------------------------
-- Evaluation results
-- -----------------------------------------------------------------------------

-- | Summary of one completed evaluation episode.
data EpisodeResult = EpisodeResult
    {
        -- | Final game tick of the episode.
        episodeTicks :: Int,

        -- | Complete final worm states, including dead worms.
        episodeFinalWorms :: [Worm],

        -- | IDs of worms that became the sole living worm at any point.
        --
        -- A worm remains recorded even if it later dies before the episode
        -- reaches its tick limit.
        episodeLastStandingWormIds :: [Int],

        -- | Recorded death reason of every worm that died during the episode.
        episodeDeaths :: [(Int, DeathReason)]
    }
    deriving (Show, Eq)


-- | Game-wide aggregate statistics over multiple episodes.
--
-- These values summarize the complete match rather than one particular worm.
-- In particular, 'survivalRate' means that at least one worm survived until
-- the episode ended; use 'WormEvaluationSummary' for agent-specific survival.
data EvaluationSummary = EvaluationSummary
    { evaluatedEpisodes :: Int
    , avgFoodEaten :: Double
    , avgEpisodeLength :: Double
    , avgKills :: Double
    , survivalRate :: Double
    }
    deriving (Show, Eq)


-- | Aggregate evaluation statistics for one specific worm.
data WormEvaluationSummary = WormEvaluationSummary
    { evaluatedWormId :: Int
    , wormEvaluatedEpisodes :: Int
    , wormAvgFoodEaten :: Double
    , wormFoodPer100Ticks :: Double
    , wormAvgAge :: Double
    , wormMedianAge :: Double
    , wormP90Age :: Int
    , wormMaxAge :: Int
    , wormAvgKills :: Double
    , wormAvgFinalLength :: Double
    , wormMaxFinalLength :: Int
    , wormSurvivalRate :: Double
    , wormLastStandingRate :: Double
    , wormWallDeathRate :: Double
    , wormPoisonDeathRate :: Double
    , wormOwnBodyDeathRate :: Double
    , wormOtherBodyDeathRate :: Double
    , wormHeadToHeadDeathRate :: Double
    }
    deriving (Show, Eq)


-- | Evaluation result of one named agent.
data AgentEvaluation = AgentEvaluation
    { evaluatedAgentName :: String
    , evaluatedAgentId :: Int
    , evaluationSummary :: EvaluationSummary
    , wormSummary :: WormEvaluationSummary
    }
    deriving (Show, Eq)


-- -----------------------------------------------------------------------------
-- Last-standing tracking
-- -----------------------------------------------------------------------------

-- | Returns the ID of the only living worm when exactly one remains.
lastStandingWormId :: GameState -> Maybe Int
lastStandingWormId state =
    case filter wormAlive (gameWorms state) of
        [worm] -> Just (wormId worm)
        _ -> Nothing


-- | Records a worm when it becomes the only living worm.
--
-- Each worm ID is stored at most once during an episode.
recordLastStandingWorm :: GameState -> [Int] -> [Int]
recordLastStandingWorm state recordedIds =
    case lastStandingWormId state of
        Nothing ->
            recordedIds

        Just targetId
            | targetId `elem` recordedIds -> recordedIds
            | otherwise -> targetId : recordedIds


-- -----------------------------------------------------------------------------
-- Episode simulation
-- -----------------------------------------------------------------------------

-- | Returns whether an evaluation episode should continue.
--
-- An episode runs while at least one worm remains alive and the configured tick
-- limit has not been reached.
episodeRunning :: Int -> GameState -> Bool
episodeRunning maxTicks state =
    gameTick state < maxTicks && any wormAlive (gameWorms state)


-- | Runs one evaluation episode until all worms die or the tick limit is reached.
--
-- Controller actions are processed through the normal simultaneous game engine.
-- Death reasons are taken from 'stepGameDetailed', while food is replenished
-- after every simulated tick using the normal food-spawning logic.
runEpisode :: Int -> [(Int, Controller)] -> GameState -> IO EpisodeResult
runEpisode maxTicks assignedControllers initialState = do
    stateWithFood <- maintainFoodCount maxFoodCount initialState
    (finalState, lastStandingIds, deaths) <- runLoop stateWithFood [] []

    pure
        EpisodeResult
            { episodeTicks = gameTick finalState
            , episodeFinalWorms = gameWorms finalState
            , episodeLastStandingWormIds = lastStandingIds
            , episodeDeaths = deaths
            }
  where
    -- | Advances evaluation ticks while recording last-standing events and deaths.
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
            runLoop stateWithFood updatedLastStandingIds updatedDeaths
      where
        updatedLastStandingIds =
            recordLastStandingWorm state lastStandingIds


-- | Runs multiple independent episodes from the same initial game state.
runEpisodes :: Int -> Int -> [(Int, Controller)] -> GameState -> IO [EpisodeResult]
runEpisodes episodeCount maxTicks assignedControllers initialState =
    mapM (\_ -> runEpisode maxTicks assignedControllers initialState) [1 .. episodeCount]


-- -----------------------------------------------------------------------------
-- Game-wide metrics
-- -----------------------------------------------------------------------------

-- | Returns the total amount of food eaten by all worms in one episode.
totalFoodEaten :: EpisodeResult -> Int
totalFoodEaten result =
    sum (map (foodEaten . wormStats) (episodeFinalWorms result))


-- | Returns the average total amount of food eaten per episode.
averageFoodEaten :: [EpisodeResult] -> Double
averageFoodEaten results =
    average (map totalFoodEaten results)


-- | Returns the total number of kills made by all worms in one episode.
totalKills :: EpisodeResult -> Int
totalKills result =
    sum (map (kills . wormStats) (episodeFinalWorms result))


-- | Returns the average total number of kills per episode.
averageKills :: [EpisodeResult] -> Double
averageKills results =
    average (map totalKills results)


-- | Returns the average final game tick across episodes.
averageEpisodeLength :: [EpisodeResult] -> Double
averageEpisodeLength results =
    average (map episodeTicks results)


-- | Returns whether at least one worm is alive at the end of an episode.
episodeHasSurvivor :: EpisodeResult -> Bool
episodeHasSurvivor result =
    any wormAlive (episodeFinalWorms result)


-- | Returns the proportion of episodes ending with at least one living worm.
--
-- Since evaluation continues while any worm is alive, this normally represents
-- the proportion of episodes that reached the configured tick limit instead of
-- ending because all worms died.
averageSurvivalRate :: [EpisodeResult] -> Double
averageSurvivalRate results =
    average
        [ if episodeHasSurvivor result then 1 else 0
        | result <- results
        ]


-- -----------------------------------------------------------------------------
-- Worm-specific metric helpers
-- -----------------------------------------------------------------------------

-- | Sums one integer-valued worm property for the selected worm ID.
--
-- Worm IDs are expected to be unique, but summing preserves the behaviour of
-- the original metric implementations even for malformed episode results.
wormMetric :: (Worm -> Int) -> Int -> EpisodeResult -> Int
wormMetric selector targetId result =
    sum
        [ selector worm
        | worm <- episodeFinalWorms result
        , wormId worm == targetId
        ]


-- | Returns the amount of food eaten by the selected worm in one episode.
foodEatenByWorm :: Int -> EpisodeResult -> Int
foodEatenByWorm =
    wormMetric (foodEaten . wormStats)


-- | Returns the age reached by the selected worm in one episode.
ageByWorm :: Int -> EpisodeResult -> Int
ageByWorm =
    wormMetric (age . wormStats)


-- | Returns the number of kills made by the selected worm in one episode.
killsByWorm :: Int -> EpisodeResult -> Int
killsByWorm =
    wormMetric (kills . wormStats)


-- | Returns the selected worm's final body length in one episode.
finalLengthByWorm :: Int -> EpisodeResult -> Int
finalLengthByWorm =
    wormMetric (length . wormBody)


-- | Returns whether the selected worm is alive in the final episode state.
wormSurvivedEpisode :: Int -> EpisodeResult -> Bool
wormSurvivedEpisode targetId result =
    any
        (\worm -> wormId worm == targetId && wormAlive worm)
        (episodeFinalWorms result)


-- | Returns whether the selected worm became the sole living worm at any point.
wormWasLastStanding :: Int -> EpisodeResult -> Bool
wormWasLastStanding targetId result =
    targetId `elem` episodeLastStandingWormIds result


-- -----------------------------------------------------------------------------
-- Worm-specific metrics
-- -----------------------------------------------------------------------------

-- | Returns the average amount of food eaten by the selected worm.
averageFoodEatenByWorm :: Int -> [EpisodeResult] -> Double
averageFoodEatenByWorm targetId results =
    average (map (foodEatenByWorm targetId) results)


-- | Returns food eaten per 100 ticks lived by the selected worm.
--
-- The denominator uses worm age rather than total episode length, so ticks
-- after the selected worm dies do not reduce this rate.
foodPer100TicksByWorm :: Int -> [EpisodeResult] -> Double
foodPer100TicksByWorm targetId results
    | totalTicks == 0 = 0
    | otherwise = 100 * fromIntegral totalFood / fromIntegral totalTicks
  where
    totalFood = sum (map (foodEatenByWorm targetId) results)
    totalTicks = sum (map (ageByWorm targetId) results)


-- | Returns the average age reached by the selected worm.
averageAgeByWorm :: Int -> [EpisodeResult] -> Double
averageAgeByWorm targetId results =
    average (map (ageByWorm targetId) results)


-- | Returns the median age reached by the selected worm.
medianAgeByWorm :: Int -> [EpisodeResult] -> Double
medianAgeByWorm targetId results =
    median (map (ageByWorm targetId) results)


-- | Returns the approximate 90th percentile of ages reached by the selected worm.
p90AgeByWorm :: Int -> [EpisodeResult] -> Int
p90AgeByWorm targetId results =
    percentile 0.9 (map (ageByWorm targetId) results)


-- | Returns the maximum age reached by the selected worm.
maxAgeByWorm :: Int -> [EpisodeResult] -> Int
maxAgeByWorm targetId results =
    maximumOrZero (map (ageByWorm targetId) results)


-- | Returns the average number of kills made by the selected worm.
averageKillsByWorm :: Int -> [EpisodeResult] -> Double
averageKillsByWorm targetId results =
    average (map (killsByWorm targetId) results)


-- | Returns the average final body length of the selected worm.
averageFinalLengthByWorm :: Int -> [EpisodeResult] -> Double
averageFinalLengthByWorm targetId results =
    average (map (finalLengthByWorm targetId) results)


-- | Returns the maximum final body length of the selected worm.
maxFinalLengthByWorm :: Int -> [EpisodeResult] -> Int
maxFinalLengthByWorm targetId results =
    maximumOrZero (map (finalLengthByWorm targetId) results)


-- | Returns the proportion of episodes in which the selected worm survived
-- until the episode ended.
survivalRateByWorm :: Int -> [EpisodeResult] -> Double
survivalRateByWorm targetId results =
    average
        [ if wormSurvivedEpisode targetId result then 1 else 0
        | result <- results
        ]


-- | Returns the proportion of episodes in which the selected worm became the
-- only living worm at any point.
--
-- This differs from training's final-state last-standing metric: an evaluated
-- worm remains counted even if it later dies.
lastStandingRateByWorm :: Int -> [EpisodeResult] -> Double
lastStandingRateByWorm targetId results =
    average
        [ if wormWasLastStanding targetId result then 1 else 0
        | result <- results
        ]


-- -----------------------------------------------------------------------------
-- General statistics helpers
-- -----------------------------------------------------------------------------

-- | Computes the arithmetic mean of integer values.
average :: [Int] -> Double
average [] = 0
average values =
    fromIntegral (sum values) / fromIntegral (length values)


-- | Computes the median of integer observations.
median :: [Int] -> Double
median [] = 0
median values
    | odd valueCount = fromIntegral (sortedValues !! middleIndex)
    | otherwise =
        fromIntegral (sortedValues !! (middleIndex - 1) + sortedValues !! middleIndex) / 2
  where
    sortedValues = sort values
    valueCount = length sortedValues
    middleIndex = valueCount `div` 2


-- | Returns an observed value at the requested percentile.
--
-- The percentile is clamped to @[0,1]@ and no interpolation is performed.
percentile :: Double -> [Int] -> Int
percentile _ [] = 0
percentile requestedPercentile values =
    sortedValues !! percentileIndex
  where
    sortedValues = sort values
    boundedPercentile = max 0 (min 1 requestedPercentile)
    percentileIndex = floor (boundedPercentile * fromIntegral (length sortedValues - 1))


-- | Returns the maximum value, or zero for an empty list.
maximumOrZero :: [Int] -> Int
maximumOrZero [] = 0
maximumOrZero values =
    maximum values


-- -----------------------------------------------------------------------------
-- Death reason metrics
-- -----------------------------------------------------------------------------

-- | Returns the recorded death reason of the selected worm, if it died.
deathReasonByWorm :: Int -> EpisodeResult -> Maybe DeathReason
deathReasonByWorm targetId result =
    lookup targetId (episodeDeaths result)


-- | Returns the fraction of all episodes in which the selected worm died for a
-- reason matching the supplied predicate.
--
-- The denominator is every evaluated episode, not only episodes in which the
-- worm died.
deathReasonRateByWorm :: (DeathReason -> Bool) -> Int -> [EpisodeResult] -> Double
deathReasonRateByWorm matchesReason targetId results =
    average
        [ if maybe False matchesReason (deathReasonByWorm targetId result) then 1 else 0
        | result <- results
        ]


-- | Returns whether a death was caused by a wall collision.
isWallDeath :: DeathReason -> Bool
isWallDeath HitWall = True
isWallDeath _ = False


-- | Returns whether a death was caused by poison.
isPoisonDeath :: DeathReason -> Bool
isPoisonDeath HitPoison = True
isPoisonDeath _ = False


-- | Returns whether a death was caused by collision with the worm's own body.
isOwnBodyDeath :: DeathReason -> Bool
isOwnBodyDeath HitOwnBody = True
isOwnBodyDeath _ = False


-- | Returns whether a death was caused by collision with another worm's body.
isOtherBodyDeath :: DeathReason -> Bool
isOtherBodyDeath (HitOtherBody _) = True
isOtherBodyDeath _ = False


-- | Returns whether a death was caused by a head-to-head collision.
isHeadToHeadDeath :: DeathReason -> Bool
isHeadToHeadDeath (HeadToHead _) = True
isHeadToHeadDeath _ = False


-- | Returns the fraction of episodes in which the selected worm hit a wall.
wallDeathRateByWorm :: Int -> [EpisodeResult] -> Double
wallDeathRateByWorm =
    deathReasonRateByWorm isWallDeath


-- | Returns the fraction of episodes in which the selected worm hit poison.
poisonDeathRateByWorm :: Int -> [EpisodeResult] -> Double
poisonDeathRateByWorm =
    deathReasonRateByWorm isPoisonDeath


-- | Returns the fraction of episodes in which the selected worm hit its own body.
ownBodyDeathRateByWorm :: Int -> [EpisodeResult] -> Double
ownBodyDeathRateByWorm =
    deathReasonRateByWorm isOwnBodyDeath


-- | Returns the fraction of episodes in which the selected worm hit another
-- worm's body.
otherBodyDeathRateByWorm :: Int -> [EpisodeResult] -> Double
otherBodyDeathRateByWorm =
    deathReasonRateByWorm isOtherBodyDeath


-- | Returns the fraction of episodes in which the selected worm died in a
-- head-to-head collision.
headToHeadDeathRateByWorm :: Int -> [EpisodeResult] -> Double
headToHeadDeathRateByWorm =
    deathReasonRateByWorm isHeadToHeadDeath


-- -----------------------------------------------------------------------------
-- Summary construction
-- -----------------------------------------------------------------------------

-- | Computes game-wide summary statistics for a collection of episodes.
summarizeResults :: [EpisodeResult] -> EvaluationSummary
summarizeResults results =
    EvaluationSummary
        { evaluatedEpisodes = length results
        , avgFoodEaten = averageFoodEaten results
        , avgEpisodeLength = averageEpisodeLength results
        , avgKills = averageKills results
        , survivalRate = averageSurvivalRate results
        }


-- | Computes detailed summary statistics for one selected worm.
summarizeWormResults :: Int -> [EpisodeResult] -> WormEvaluationSummary
summarizeWormResults targetId results =
    WormEvaluationSummary
        { evaluatedWormId = targetId
        , wormEvaluatedEpisodes = length results
        , wormAvgFoodEaten = averageFoodEatenByWorm targetId results
        , wormFoodPer100Ticks = foodPer100TicksByWorm targetId results
        , wormAvgAge = averageAgeByWorm targetId results
        , wormMedianAge = medianAgeByWorm targetId results
        , wormP90Age = p90AgeByWorm targetId results
        , wormMaxAge = maxAgeByWorm targetId results
        , wormAvgKills = averageKillsByWorm targetId results
        , wormAvgFinalLength = averageFinalLengthByWorm targetId results
        , wormMaxFinalLength = maxFinalLengthByWorm targetId results
        , wormSurvivalRate = survivalRateByWorm targetId results
        , wormLastStandingRate = lastStandingRateByWorm targetId results
        , wormWallDeathRate = wallDeathRateByWorm targetId results
        , wormPoisonDeathRate = poisonDeathRateByWorm targetId results
        , wormOwnBodyDeathRate = ownBodyDeathRateByWorm targetId results
        , wormOtherBodyDeathRate = otherBodyDeathRateByWorm targetId results
        , wormHeadToHeadDeathRate = headToHeadDeathRateByWorm targetId results
        }


-- -----------------------------------------------------------------------------
-- Convenience agent evaluation
-- -----------------------------------------------------------------------------

-- | Tick limit used by the default convenience evaluation.
defaultEvaluationMaxTicks :: Int
defaultEvaluationMaxTicks = 500


-- | Evaluates one controller against Safe Greedy Food opponents.
--
-- Every worm other than 'agentId' receives the Safe Greedy Food controller.
-- This keeps the function valid for either starting side and for scenarios with
-- more than two worms.
evaluateAgent :: String -> Int -> Controller -> Int -> GameState -> IO AgentEvaluation
evaluateAgent name agentId controller episodes initialState = do
    results <-
        runEpisodes
            episodes
            defaultEvaluationMaxTicks
            assignedControllers
            initialState

    pure
        AgentEvaluation
            { evaluatedAgentName = name
            , evaluatedAgentId = agentId
            , evaluationSummary = summarizeResults results
            , wormSummary = summarizeWormResults agentId results
            }
  where
    opponentIds =
        [ wormId worm
        | worm <- gameWorms initialState
        , wormId worm /= agentId
        ]

    assignedControllers =
        (agentId, controller)
            : [ (opponentId, AI safeGreedyFoodAgent)
              | opponentId <- opponentIds
              ]