{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Shared.Util where

import Shared.Item (Item(..))
import Shared.Game

import Control.Lens
import qualified Data.Map as Map
import Control.Monad.State (State, get, when)

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
    -- Handle when Huts are removed
    numPeople <- (\g -> min (maxPopulation g) (getItem People g)) <$> get
    stored %= Map.insert People numPeople

    w <- Map.toList <$> use workers
    unaccountedWorkers >>= equalizeWorkers w

    -- Handle when people are subtracted
    -- let toRemove = (Map.foldr (+) 0 w) - numPeople'
    -- toRemove <- unaccountedWorkers
    -- equalizeWorkers (Map.toList w) toRemove

    -- Handle when people are added


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
