{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Shared.Util where

import Shared.UI
import Shared.Item (Item(..))
import Shared.Game

import Control.Lens

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
  -- Craftables:
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

itemShowBtn :: Functor f =>
  Item -> (Bool -> f Bool) -> UIState  -> f UIState
itemShowBtn = \case
  Trap -> showTrapBtn
  Cart -> showCartBtn
  Hut -> showHutBtn
  Lodge -> showLodgeBtn
  TradingPost -> showTradingPostBtn
  Tannery -> showTanneryBtn
  Smokehouse -> showSmokehouseBtn
  Workshop -> showWorkshopBtn
  Steelworks -> showSteelworksBtn
  Armory -> showArmoryBtn
  Torch -> showTorchBtn
  Waterskin -> showWaterskinBtn
  Cask -> showCaskBtn
  WaterTank -> showWaterTankBtn
  BoneSpear -> showBoneSpearBtn
  Rucksack -> showRucksackBtn
  Wagon -> showWagonBtn
  Convoy -> showConvoyBtn
  LeatherArmor -> showLeatherArmorBtn
  IronArmor -> showIronArmorBtn
  SteelArmor -> showSteelArmorBtn
  IronSword -> showIronSwordBtn
  SteelSword -> showSteelSwordBtn
  Rifle -> showRifleBtn
  _ -> error "you done fucked"

-- craftableReady :: Functor f => Item -> (Int -> f Int) -> Game -> f Game
craftableReady i = uiState . itemShowBtn i
