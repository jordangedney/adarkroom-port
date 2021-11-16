{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Shared.Util where

import Shared.UI
import Shared.Item (Item(..), Craftable(..))
import Shared.Game

import Control.Lens (view, (^.))

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

canAfford :: [(Item, Int)] -> Game -> Bool
canAfford items game = all afford items
  where afford (Compass, 0) = view (stored . compass) game == 0
        afford (i, amnt) = view (stored . item i) game >= amnt

getItem :: Functor f => Item -> (Int -> f Int) -> Game -> f Game
getItem i = stored . item i

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

-- I'm not sure why, but the types don't work out when I try to combine this
-- with the craftable function above.
displayCraftables = \case
  Trap -> ("trap", showTrap, showTrapBtn)
  Cart -> ("cart", showCart, showCartBtn)
  Hut -> ("hut", showHut, showHutBtn)
  Lodge -> ("lodge", showLodge, showLodgeBtn)
  TradingPost -> ("trading post", showTradingPost, showTradingPostBtn)
  Tannery -> ("tannery", showTannery, showTanneryBtn)
  Smokehouse -> ("smokehouse", showSmokehouse, showSmokehouseBtn)
  Workshop -> ("workshop", showWorkshop, showWorkshopBtn)
  Steelworks -> ("steelworks", showSteelworks, showSteelworksBtn)
  Armory -> ("armory", showArmory, showArmoryBtn)
  Torch -> ("torch", showTorch, showTorchBtn)
  Waterskin -> ("waterskin", showWaterskin, showWaterskinBtn)
  Cask -> ("cask", showCask, showCaskBtn)
  WaterTank -> ("watertank", showWaterTank, showWaterTankBtn)
  BoneSpear -> ("bonespear", showBoneSpear, showBoneSpearBtn)
  Rucksack -> ("rucksack", showRucksack, showRucksackBtn)
  Wagon -> ("wagon", showWagon, showWagonBtn)
  Convoy -> ("convoy", showConvoy, showConvoyBtn)
  LeatherArmor -> ("leather armor", showLeatherArmor, showLeatherArmorBtn)
  IronArmor -> ("iron armor", showIronArmor, showIronArmorBtn)
  SteelArmor -> ("steel armor", showSteelArmor, showSteelArmorBtn)
  IronSword -> ("iron sword", showIronSword, showIronSwordBtn)
  SteelSword -> ("steel sword", showSteelSword, showSteelSwordBtn)
  Rifle -> ("rifle", showRifle, showRifleBtn)

getCraftable :: Functor f => Craftable -> (Int -> f Int) -> Game -> f Game
getCraftable i = stored . craftable i

craftableName :: Craftable -> String
craftableName = (\(x, _, _) -> x) . displayCraftables

craftableShowStored :: Game -> Craftable -> Bool
craftableShowStored g c = g ^. (uiState . go c)
  where go = (\(_, x, _) -> x) . displayCraftables

craftableShowBtn :: Game -> Craftable -> Bool
craftableShowBtn g c = g ^. (uiState . go c)
  where go = (\(_, _, x) -> x) . displayCraftables
