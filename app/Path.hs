module Path
  ( arrival
  , allocatePathSupply
  , deallocatePathSupply
  ) where

import Control.Lens
import Control.Monad (when)
import Control.Monad.State (get)
import qualified Data.Map as Map

import Shared.Game
import Shared.Item (Item)
import Shared.Util (getItem, overStored)

import Util (clearRoomBacklog)

arrival :: DarkRoom
arrival = do
  location .= Path
  clearRoomBacklog

-- Move one of `i` from stores into the expedition rucksack, but only
-- if there is at least one in stores AND the rucksack still has room.
allocatePathSupply :: Item -> DarkRoom
allocatePathSupply i = do
  g <- get
  let inStores = getItem i g
      cap      = view (playerStats . inventoryCapacity) g
      used     = sum (Map.elems (view expedition g))
      free     = cap - used
  when (inStores > 0 && free > 0) $ do
    overStored i (subtract 1)
    expedition %= Map.insertWith (+) i 1

-- Move one of `i` from the expedition rucksack back to stores, but only
-- if at least one is currently allocated.
deallocatePathSupply :: Item -> DarkRoom
deallocatePathSupply i = do
  inExp <- uses expedition (Map.findWithDefault 0 i)
  when (inExp > 0) $ do
    expedition %= Map.adjust (subtract 1) i
    overStored i (+ 1)
