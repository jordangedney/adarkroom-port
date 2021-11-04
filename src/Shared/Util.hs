{-# LANGUAGE LambdaCase #-}
module Shared.Util where

import Shared.Item (Item(..), Craftable(..))
import Shared.Game

import Control.Lens (view)

item :: Functor f => Item -> (Int -> f Int) -> Stored -> f Stored
item = \case
  Fur   -> fur
  Cloth -> cloth
  Scale -> scales
  Teeth -> teeth
  Bait -> bait
  Compass -> compass
  Wood -> wood
  Meat -> meat
  Coal -> coal
  Leather -> leather
  Iron -> iron
  Steel -> steel
  Sulphur -> sulphur
  -- Also craftables
  HutItem -> hut

craftable :: Functor f => Craftable -> (Int -> f Int) -> Stored -> f Stored
craftable = \case
  Trap -> trap
  Cart -> cart
  Hut -> hut
  Lodge -> lodge
  TradingPost -> tradingPost
  Tannery -> tannery
  Smokehouse -> smokehouse
  Workshop -> workshop
  Steelworks -> steelworks
  Armory -> armory
  Torch -> torch
  Waterskin -> waterskin
  Cask -> cask
  WaterTank -> waterTank
  BoneSpear -> boneSpear
  Rucksack -> rucksack
  Wagon -> wagon
  Convoy -> convoy
  LeatherArmor -> leatherArmor
  IronArmor -> ironArmor
  SteelArmor -> steelArmor
  IronSword -> ironSword
  SteelSword -> steelSword
  Rifle -> rifle

canAfford :: [(Item, Int)] -> Game -> Bool
canAfford items game = all afford items
  where afford (Compass, 0) = view (stored . compass) game == 0
        afford (i, amnt) = view (stored . item i) game >= amnt

getItem :: Functor f => Item -> (Int -> f Int) -> Game -> f Game
getItem i = stored . item i

getCraftable :: Functor f => Craftable -> (Int -> f Int) -> Game -> f Game
getCraftable i = stored . craftable i
