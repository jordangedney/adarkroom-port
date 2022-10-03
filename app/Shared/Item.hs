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

weapons :: [Item]
weapons = [BoneSpear, IronSword, SteelSword, Rifle]

goods :: [Item]
goods  = [Fur, Cloth, Charm, Scale, Teeth, Bait, Compass, Wood, Meat, Coal,
          Leather, Iron, Steel, Sulphur, Torch]
