module Util where
-- For data manipulation utilities; things without game-related imports

import Data.List (transpose, sortBy)
import Data.Function (on)
import Safe (headDef)
import System.Random (StdGen, randomR)

count :: Eq a => a -> [a] -> Int
count x = length . filter (x ==)

interleave :: [[a]] -> [a]
interleave = concat . transpose

-- The defaultValue should never trigger _if_ your probabilities add up to 100%.
-- Otherwise its there to fill in the remainder.
-- Theres also no error checking if you give over 100%
-- Examples:
-- randomChoice' 20  "def" [(25, "a"), (25, "b"), (25, "c"), (25 "d")]            -> "a"
-- randomChoice' 50  "def" [(25, "a"), (25, "b"), (25, "c"), (25 "d")]            -> "b"
-- randomChoice' 100 "def" [(25, "a"), (25, "b"), (25, "c")          ]            -> "def"
-- randomChoice' 100 "def" [(25, "a"), (25, "b"), (25, "c"), (25 "d"), (25, "e")] -> "d"
-- randomChoice' :: Int -> a -> [(Int, a)] -> a
-- randomChoice' :: Integer -> a -> [(Integer, (Integer, a))] -> a
-- randomChoice' :: Integer -> b -> [(Integer, b)] -> b
randomChoice' :: (Ord a, Num a) => b -> [(a, b)] -> a -> b
randomChoice' defaultValue probabilities rand =
  let sorted = sortBy (compare `on` fst) probabilities
      probs = drop 1 (scanl (\(s, _) (a, b) -> (s - a, b)) (rand, undefined) sorted)
      choice' = headDef (undefined, defaultValue) (filter (\(a, _) -> a <= 0) probs)
  in snd choice'

randomChoices :: StdGen -> a -> [(Int, a)] -> [a]
randomChoices randomGen defaultValue probabilities =
  let percents = listOfRandomPercentages randomGen
  in map (randomChoice' defaultValue probabilities) percents

randomChoice :: StdGen -> a -> [(Int, a)] -> a
randomChoice r d p = head (randomChoices r d p)

choice :: StdGen -> [a] -> a
choice rnd xs = xs !! rand where
  n = length xs
  (rand, _) = randomR (0, n-1) rnd

listOfRandomPercentages :: StdGen -> [Int]
listOfRandomPercentages randomGen =
  let (percentage, gen) = randomR (1, 100) randomGen
  in percentage : listOfRandomPercentages gen
