{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Shared.Util where

import Shared.Item (Item(..))
import Shared.Game

import Control.Lens
import qualified Data.Map as Map
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

overItem :: Item -> (Int -> Int) -> Game -> Game
overItem i fn g =
  g & over stored (Map.insertWith (+) i 0)
    & over stored (Map.insertWith (\a b-> fn a + b) i 0)

playerHasSeen :: Item -> Game -> Bool
playerHasSeen i g = Map.member i (g ^. stored)

overStored :: Item -> (Int -> Int) -> DarkRoom
overStored i fn = do
  stored %= Map.insertWith (+) i 0
  stored %= Map.insertWith (\a b-> fn a + b) i 0
