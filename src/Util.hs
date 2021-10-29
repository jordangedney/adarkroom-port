{-# LANGUAGE TupleSections #-}

module Util where

import Shared.Game
import Shared.GameEvent

import Data.List (sortBy)
import Data.Function (on)
import Safe (headDef)
import System.Random (StdGen, randomR)

import Control.Lens (over, set, view, (&), (%=), use, (.=))
import Shared.Item (Item(..))

count :: Eq a => a -> [a] -> Int
count x = length . filter (x ==)

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

-- Game Utils ------------------------------------------------------------------

addEvent :: String -> Game -> Game
addEvent message game =
  game & over events ((message, 0):)

addEvent' :: String -> DarkRoom
addEvent' message = do
  events %= ((message, 0):)

notifyRoom :: String -> Game -> Game
notifyRoom message game =
  if view location game == Room
  then game & addEvent message
  else game & over roomEventBacklog ((:) message)

notifyRoom' :: String -> DarkRoom
notifyRoom' message = do
  inRoom <- (== Room) <$> use location
  if inRoom then addEvent' message
  else roomEventBacklog  %= (:) message

clearRoomBacklog :: Game -> Game
clearRoomBacklog game =
  game & over events (\es ->  map (, 0) (view roomEventBacklog game) ++ es)
       & set roomEventBacklog []

updateEvents :: GameEvent -> Int -> Game -> Game
updateEvents event time = over upcomingEvents (set (eventGetter event) (event, time))

updateEvent :: GameEvent -> Int -> DarkRoom
updateEvent event time = do
  upcomingEvents.eventGetter event .= (event, time)

itemToStr :: Item -> String
itemToStr Fur     = "fur"
itemToStr Cloth   = "cloth"
itemToStr Scale   = "scales"
itemToStr Teeth   = "teeth"
itemToStr Bait    = "bait"
itemToStr Compass = "compass"
itemToStr Wood = "wood"
itemToStr Hut = "hut"

costMsg :: (Eq a, Num a, Show a) => [(Item, a)] -> Game -> Game
costMsg [] g = g
-- Handle the hidden requirement allowing players to only buy one compass
costMsg ((Compass, 0):xs) g = costMsg xs g
costMsg ((i, cost'):xs) g =
  notifyRoom ("not enough " <> itemToStr i <> " (" <> show cost' <> ").") g
  & costMsg xs
