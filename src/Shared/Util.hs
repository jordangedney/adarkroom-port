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
item Hut = huts

itemToStr :: Item -> String
itemToStr Fur     = "fur"
itemToStr Cloth   = "cloth"
itemToStr Scale   = "scales"
itemToStr Teeth   = "teeth"
itemToStr Bait    = "bait"
itemToStr Compass = "compass"
itemToStr Wood = "wood"
itemToStr Hut = "hut"

canAfford :: [(Item, Int)] -> Game -> Bool
canAfford items game = all afford items
  where afford (Compass, 0) = view (stored . compass) game == 0
        afford (i, amnt) = view (stored . item i) game >= amnt
