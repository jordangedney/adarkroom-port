module Util where
-- For data manipulation utilities; things without game-related imports

import Data.List (transpose, sortBy)
import Data.Function (on)
import Safe (headDef)

count :: Eq a => a -> [a] -> Int
count x = length . filter (x ==)

interleave :: [[a]] -> [a]
interleave = concat . transpose

justifyLeft16 :: String -> String
justifyLeft16 str = take 16 $ str ++ repeat ' '

justifyCenter15 :: String -> String
justifyCenter15 str =
  let whitespace = replicate ((15 - length str) `div` 2) ' '
      newStr = whitespace ++ str ++ whitespace
  in if length newStr == 15 then newStr else newStr ++ " "

-- Theres no error checking if you give over 100%
-- Examples:
-- randomChoice' 20  "def" [(25, "a"), (25, "b"), (25, "c"), (25 "d")]            -> "a"
-- randomChoice' 50  "def" [(25, "a"), (25, "b"), (25, "c"), (25 "d")]            -> "b"
-- randomChoice' 100 "def" [(25, "a"), (25, "b"), (25, "c")          ]            -> "def"
-- randomChoice' 100 "def" [(25, "a"), (25, "b"), (25, "c"), (25 "d"), (25, "e")] -> "d"
randomChoice' :: Int -> a -> [(Int, a)] -> a
randomChoice' rand defaultValue probabilities =
  let sorted = sortBy (compare `on` fst) probabilities
      probs = drop 1 (scanl (\(s, _) (a, b) -> (s - a, b)) (rand, undefined) sorted)
      choice = headDef (undefined, defaultValue) (filter (\(a, b) -> a <= 0) probs)
  in snd choice

randomChoice randomGen defautValue probabilities = undefined
