module Rewards
  ( open
  , takeOne
  , takeAll
  , dropOne
  , eatMeat
  , leave
  , continue
  , inventoryUsed
  , inventoryFree
  ) where

import Control.Lens
import Control.Monad (when)
import Control.Monad.State (gets)
import qualified Data.Map.Strict as Map

import Shared.Game
  ( DarkRoom
  , Game
  , expeditionInventory
  , inRewards
  , playerStats
  , inventoryCapacity
  , hp
  , maxHp
  )
import Shared.Item (Item(CuredMeat))
import Shared.Rewards
  ( Rewards(..)
  , RewardsContext
  , rewardsAvailable
  )

-- A single cured meat heals this many HP, capped at maxHp.
curedMeatHeal :: Int
curedMeatHeal = 8

open :: String -> Map.Map Item Int -> RewardsContext -> DarkRoom
open txt items ctx = inRewards .= Just (Rewards txt items ctx)

inventoryUsed :: Game -> Int
inventoryUsed g = sum (Map.elems (g ^. expeditionInventory))

inventoryFree :: Game -> Int
inventoryFree g = (g ^. playerStats . inventoryCapacity) - inventoryUsed g

availableInRewards :: Item -> Game -> Int
availableInRewards i g =
  case g ^. inRewards of
    Nothing -> 0
    Just r  -> Map.findWithDefault 0 i (_rewardsAvailable r)

takeOne :: Item -> DarkRoom
takeOne i = do
  avail <- gets (availableInRewards i)
  free <- gets inventoryFree
  when (avail > 0 && free > 0) $ do
    expeditionInventory %= Map.insertWith (+) i 1
    inRewards . _Just . rewardsAvailable %= Map.adjust (subtract 1) i

-- Move one of the player's expedition-inventory items off the rucksack
-- entirely; dropped items are gone, matching the original web game.
dropOne :: Item -> DarkRoom
dropOne i = do
  cur <- gets (Map.findWithDefault 0 i . view expeditionInventory)
  when (cur > 0) $
    expeditionInventory %= Map.adjust (subtract 1) i

takeAll :: DarkRoom
takeAll = do
  mr <- use inRewards
  case mr of
    Nothing -> pure ()
    Just r  -> takeUntilFull (Map.toList (_rewardsAvailable r))

-- Drains items from the rewards pool into the rucksack until either the
-- rewards pool is empty or the rucksack is full.
takeUntilFull :: [(Item, Int)] -> DarkRoom
takeUntilFull [] = pure ()
takeUntilFull ((i, n):xs) = do
  free <- gets inventoryFree
  let toTake = min n free
  when (toTake > 0) $ do
    expeditionInventory %= Map.insertWith (+) i toTake
    inRewards . _Just . rewardsAvailable
      %= Map.adjust (subtract toTake) i
  if free - toTake <= 0 then pure () else takeUntilFull xs

eatMeat :: DarkRoom
eatMeat = do
  have <- gets (Map.findWithDefault 0 CuredMeat . view expeditionInventory)
  when (have > 0) $ do
    expeditionInventory %= Map.adjust (subtract 1) CuredMeat
    cap <- use (playerStats . maxHp)
    playerStats . hp %= \h -> min cap (h + curedMeatHeal)

leave :: DarkRoom
leave = inRewards .= Nothing

continue :: DarkRoom
continue = inRewards .= Nothing
