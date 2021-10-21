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
  -- Craftable:
  | Hut
  | Lodge
  | TradingPost
  | Tannery
  | Smokehouse
  | Workshop
  | Steelworks
  | Armoury
  | Torch
  | Waterskin
  | Cask
  | WaterTank
  | BoneSpear
  | Rucksack
  | Wagon
  | Convoy
  | LeatherArmour
  | IronArmour
  | SteelArmour
  | IronSword
  | SteelSword
  | Rifle
  deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)
