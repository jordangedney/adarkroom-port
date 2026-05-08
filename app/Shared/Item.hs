{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Shared.Item where

import GHC.Generics
import Data.Yaml
import Data.Aeson.Types (ToJSONKey, FromJSONKey)
import Data.Char (isUpper, toLower)

data Item
  = Fur
  | Cloth
  | Charm
  | Scale
  | Teeth
  | Bait
  | Compass
  | Wood
  | Meat
  | CuredMeat
  | Coal
  | Leather
  | Iron
  | Steel
  | Sulphur
  -- Craftables:
  | Trap
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
  | Bullets
  -- blatant objectification:
  | People
  deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Enum, Bounded)

itemToStr :: Item -> String
itemToStr = tail . foldr go [] . show
  where go l r = if isUpper l then ' ' : toLower l : r else l : r

buildings :: [Item]
buildings = [Trap, Cart, Hut, Lodge, TradingPost, Tannery, Smokehouse,
  Workshop, Steelworks, Armory]

craftableItems :: [Item]
craftableItems = [Trap, Cart, Hut, Lodge, TradingPost, Tannery, Smokehouse,
  Workshop, Steelworks, Armory, Torch, Waterskin, Cask, WaterTank, BoneSpear,
  Rucksack, Wagon, Convoy, LeatherArmor, IronArmor, SteelArmor, IronSword,
  SteelSword, Rifle]

-- Weapons category — gated on first bone-spear craft. BoneSpear itself sits in
-- craftables so the player can reach it before the category exists.
weapons :: [Item]
weapons = [IronSword, SteelSword, Rifle]

-- Items the workshop unlocks immediately on completion (per bead PrePath).
workshopTools :: [Item]
workshopTools = [Torch, Waterskin, Rucksack, LeatherArmor]

-- All non-building, non-weapon-category craftables. These show up in the craft
-- menu as the player gathers the resources they need.
craftables :: [Item]
craftables = [Torch, Waterskin, Cask, WaterTank, BoneSpear, Rucksack, Wagon,
              Convoy, LeatherArmor, IronArmor, SteelArmor]

goods :: [Item]
goods  = [Fur, Cloth, Charm, Scale, Teeth, Bait, Compass, Wood, Meat, Coal,
          Leather, Iron, Steel, Sulphur, Torch]
