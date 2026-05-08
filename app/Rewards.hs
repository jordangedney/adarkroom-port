module Rewards
  ( showRewards
  , dismiss
  , takeItem
  , takeAll
  , takeAllYouCan
  , dropItem
  , eatMeat
  , beastVictory
  , sampleChest
  , curedMeatHealAmount
  ) where

import Control.Lens
import Control.Monad (forM_, when)
import Control.Monad.State (gets)
import qualified Data.Map.Strict as Map

import Shared.Game
import Shared.Item (Item(..))
import Shared.Rewards
import qualified Path

-- A cured meat heals this much HP when eaten (A Dark Room uses 8).
curedMeatHealAmount :: Int
curedMeatHealAmount = 8

showRewards :: RewardsScreen -> DarkRoom
showRewards r = rewards .= Just r

dismiss :: DarkRoom
dismiss = rewards .= Nothing

availableInRewards :: Item -> Game -> Int
availableInRewards i g = case g ^. rewards of
  Nothing -> 0
  Just r  -> Map.findWithDefault 0 i (r ^. rewardsItems)

-- Move one of `i` from the rewards pile into the expedition inventory.
-- Capped by both the rewards count and the rucksack's free space.
takeItem :: Item -> DarkRoom
takeItem i = do
  avail <- gets (availableInRewards i)
  free <- gets Path.inventoryFree
  when (avail > 0 && free > 0) $ do
    rewards . _Just . rewardsItems %= Map.adjust (subtract 1) i
    expeditionInventory %= Map.insertWith (+) i 1

-- Move `n` of `i` at once. Caller is responsible for not exceeding capacity.
takeNOf :: Item -> Int -> DarkRoom
takeNOf i n = when (n > 0) $ do
  rewards . _Just . rewardsItems %= Map.adjust (subtract n) i
  expeditionInventory %= Map.insertWith (+) i n

-- "Take all" — transactional. If everything fits, take it all; if not,
-- take nothing. The intent: the player commits to taking everything,
-- and a half-take would be surprising.
takeAll :: DarkRoom
takeAll = do
  rwds <- use rewards
  free <- gets Path.inventoryFree
  case rwds of
    Nothing -> pure ()
    Just r  -> do
      let entries = filter ((> 0) . snd) (Map.toList (r ^. rewardsItems))
          total   = sum (map snd entries)
      when (total > 0 && total <= free) $
        forM_ entries $ \(i, n) -> takeNOf i n

-- "Take all you can" — greedy. Walk the items in order, take as many of
-- each as fits, stop when the rucksack is full.
takeAllYouCan :: DarkRoom
takeAllYouCan = do
  rwds <- use rewards
  case rwds of
    Nothing -> pure ()
    Just r  -> do
      let entries = filter ((> 0) . snd) (Map.toList (r ^. rewardsItems))
      forM_ entries $ \(i, n) -> do
        free <- gets Path.inventoryFree
        let take' = min n free
        takeNOf i take'

-- Discard one of `i` from the expedition inventory. The dropped item is
-- gone — there's no on-path "ground" to pick it back up from. Used to
-- free rucksack space when the rewards pile holds something the player
-- wants more than what they're already carrying.
dropItem :: Item -> DarkRoom
dropItem i = do
  cur <- gets (Map.findWithDefault 0 i . view expeditionInventory)
  when (cur > 0) $
    expeditionInventory %= Map.adjust (subtract 1) i

-- Eat one cured meat from the expedition inventory to heal HP.
eatMeat :: DarkRoom
eatMeat = do
  meatCount <- gets (Map.findWithDefault 0 CuredMeat . view expeditionInventory)
  curHp <- use (playerStats . hp)
  maxHpVal <- use (playerStats . maxHp)
  when (meatCount > 0 && curHp < maxHpVal) $ do
    expeditionInventory %= Map.adjust (subtract 1) CuredMeat
    playerStats . hp .= min maxHpVal (curHp + curedMeatHealAmount)

-- Combat will call `showRewards (beastVictory drops)` when a beast dies.
-- The beast's species, drop table, and flavor text live in combat code;
-- this just packages the rewards-screen payload.
beastVictory :: [String] -> Map.Map Item Int -> RewardsScreen
beastVictory bodyText drops = RewardsScreen
  { _rewardsTitle  = "victory"
  , _rewardsText   = bodyText
  , _rewardsItems  = drops
  , _rewardsCanEat = True
  , _rewardsExit   = ContinueExit
  }

-- A sample payload for the debug "rew." trigger so the screen can be
-- exercised before combat / place exploration are wired up. Mixes in
-- enough items + a meat to make every button reachable.
sampleChest :: RewardsScreen
sampleChest = RewardsScreen
  { _rewardsTitle  = "you killed the beast"
  , _rewardsText   = [ "the body lies still in the dust."
                     , "blood pools on the path."
                     ]
  , _rewardsItems  = Map.fromList
      [ (Fur,       4)
      , (Meat,      3)
      , (Teeth,     1)
      , (CuredMeat, 1)
      ]
  , _rewardsCanEat = True
  , _rewardsExit   = ContinueExit
  }
