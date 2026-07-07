module Evaluation where

import Config
import Controller
import Game
import Types



-- -----------------------------------------------------------------------------
-- Episode results
-- -----------------------------------------------------------------------------

-- | Summary of one completed episode.
data EpisodeResult = EpisodeResult
    {
        episodeTicks :: Int,
        episodeFinalWorms :: [Worm]
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
        wormAvgAge :: Double,
        wormAvgKills :: Double,
        wormSurvivalRate :: Double
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
            wormAvgAge = averageAgeByWorm targetId results,
            wormAvgKills = averageKillsByWorm targetId results,
            wormSurvivalRate = survivalRateByWorm targetId results
        }




-- -----------------------------------------------------------------------------
-- Episode simulation
-- -----------------------------------------------------------------------------

-- | Returns True if the episode should continue running

episodeRunning :: Int -> GameState -> Bool
episodeRunning maxTicks state =
    gameTick state < maxTicks && any wormAlive (gameWorms state)



-- | Runs a single episode until all worms are dead or the tick limit is reached
runEpisode :: Int -> [(Int, Controller)] -> GameState -> IO EpisodeResult
runEpisode maxTicks assignedControllers initialState = do
    stateWithFood <- maintainFoodCount maxFoodCount initialState
    finalState <- runLoop stateWithFood 
    pure
        EpisodeResult
            {
                episodeTicks = gameTick finalState,
                episodeFinalWorms = gameWorms finalState
            }
  where
    runLoop :: GameState -> IO GameState
    runLoop state
        | not (episodeRunning maxTicks state) = pure state
        | otherwise = do
                actions <- collectActions assignedControllers state
                let steppedState = stepGame actions state
                stateWithFood <- maintainFoodCount maxFoodCount steppedState 
                runLoop stateWithFood


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

