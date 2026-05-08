{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Shared.Util where

import Shared.Item (Item(..))
import Shared.Game

import Control.Lens
import qualified Data.Map as Map
import Control.Monad (when)
import Control.Monad.State (State, get)

canAfford :: [(Item, Int)] -> Game -> Bool
canAfford items game = all afford items
  where afford (Compass, 0) = getItem Compass game == 0
        afford (i, amnt) = getItem i game >= amnt

getItem :: Item -> Game -> Int
getItem i g = Map.findWithDefault 0 i $ g ^. stored

getStored :: Item -> State Game Int
getStored i = do
  g <- get
  return $ Map.findWithDefault 0 i $ g ^. stored

playerHasSeen :: Item -> Game -> Bool
playerHasSeen i g = Map.member i (g ^. stored)

maxPopulation :: Game -> Int
maxPopulation game = getItem Hut game * 4
-- maxPopulation = (* 4) . getItem Hut

overStored :: Item -> (Int -> Int) -> DarkRoom
overStored i fn = do
  stored %= Map.insertWith (+) i 0
  stored %= Map.insertWith (\a b-> fn a + b) i 0

  -- Keeps the number of huts, people, and workers in aligment
  -- [Yes, it would be better if the types kept these from getting out of sync,
  -- but keeping the People and Huts as part of the Item type is very convenient.]
  when (i `elem` [Hut, People]) $ do
    -- Cap people at maxPopulation (huts may have been removed)
    numPeople <- (\g -> min (maxPopulation g) (getItem People g)) <$> get
    stored %= Map.insert People numPeople

    delta <- unaccountedWorkers
    case compare delta 0 of
      GT -> do
        -- More workers than people: peel some off, starting with whichever
        -- worker types currently have headcount.
        w <- Map.toList <$> use workers
        equalizeWorkers w delta
      LT -> do
        -- New villagers default to Gatherer (matches the original web game).
        workers %= Map.insertWith (+) Gatherer (negate delta)
      EQ -> pure ()


 where equalizeWorkers :: [(Worker, Int)] -> Int -> DarkRoom
       equalizeWorkers _ 0 = pure ()
       equalizeWorkers [] _ = pure ()
       equalizeWorkers ((workerType, numWorker):xs) delta = do
         let amt = min numWorker delta
         workers %= Map.insertWith subtract workerType amt
         equalizeWorkers xs (delta - amt)

       unaccountedWorkers = do
        numPeople <- getStored People
        numWorkers <- sum <$> use workers
        pure $ numWorkers - numPeople
