{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Shared.Util where

import Shared.UI
import Shared.Item (Item(..))
import Shared.Game

import Control.Lens
import qualified Data.Map as Map

canAfford :: [(Item, Int)] -> Game -> Bool
canAfford items game = all afford items
  where afford (Compass, 0) = getItem Compass game == 0
        afford (i, amnt) = getItem i game >= amnt

getItem :: Item -> Game -> Int
getItem i g = Map.findWithDefault 0 i $ g ^. stored

overItem :: Item -> (Int -> Int) -> Game -> Game
overItem i fn g =
  g & over stored (Map.insertWith (+) i 0)
    & over stored (Map.insertWith (\a b-> fn a + b) i 0)

playerHas :: Item -> Game -> Bool
playerHas i g = Map.member i (g ^. stored)

overStored :: Item -> (Int -> Int) -> DarkRoom
overStored i fn = do
  stored %= Map.insertWith (+) i 0
  stored %= Map.insertWith (\a b-> fn a + b) i 0

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
