{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Shared.Item where

import GHC.Generics
import Data.Yaml

data Item
  = Fur
  | Cloth
  | Scale
  | Teeth
  | Bait
  | Compass
  | Wood
  | Meat
  | Coal
  | Leather
  | Iron
  | Steel
  | Sulphur
  -- XXX Special items to work with random events:
  | HutItem
  deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)

data Craftable
  = Trap
  | Cart
  | Hut
  | Lodge
  | TradingPost
  | Tannery
  | Smokehouse
  | Workshop
  | Steelworks
  | Armory
  | Torch
  | Waterskin
  | Cask
  | WaterTank
  | BoneSpear
  | Rucksack
  | Wagon
  | Convoy
  | LeatherArmor
  | IronArmor
  | SteelArmor
  | IronSword
  | SteelSword
  | Rifle
  deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)
