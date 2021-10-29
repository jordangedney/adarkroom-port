module Shared.Util where

import Shared.Item (Item(..))
import Shared.Game

import Control.Lens (view)

item :: Functor f => Item -> (Int -> f Int) -> Stored -> f Stored
item Fur   = fur
item Cloth = cloth
item Scale = scales
item Teeth = teeth
item Bait = bait
item Compass = compass
item Wood = wood
item Meat = meat
item Coal = coal
item Leather = leather
item Iron = iron
item Steel = steel
item Sulphur = sulphur
-- Craftables
item Trap = trap
item Cart = cart
item Hut = hut
item Lodge = lodge
item TradingPost = tradingPost
item Tannery = tannery
item Smokehouse = smokehouse
item Workshop = workshop
item Steelworks = steelworks
item Armory = armory
item Torch = torch
item Waterskin = waterskin
item Cask = cask
item WaterTank = waterTank
item BoneSpear = boneSpear
item Rucksack = rucksack
item Wagon = wagon
item Convoy = convoy
item LeatherArmor = leatherArmor
item IronArmor = ironArmor
item SteelArmor = steelArmor
item IronSword = ironSword
item SteelSword = steelSword
item Rifle = rifle

canAfford :: [(Item, Int)] -> Game -> Bool
canAfford items game = all afford items
  where afford (Compass, 0) = view (stored . compass) game == 0
        afford (i, amnt) = view (stored . item i) game >= amnt

getItem :: Functor f => Item -> (Int -> f Int) -> Game -> f Game
getItem i = stored . item i
