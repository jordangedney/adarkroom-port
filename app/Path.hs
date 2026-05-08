module Path
  ( arrival
  , embark
  , increaseSupply
  , decreaseSupply
  , pathSupplies
  , allocated
  , available
  , inventoryUsed
  , inventoryFree
  ) where

import Control.Lens
import Control.Monad (when)
import Control.Monad.State (gets)
import qualified Data.Map.Strict as Map

import Shared.Game
  ( DarkRoom
  , Game
  , Location(Path)
  , embarked
  , expeditionInventory
  , location
  , playerStats
  , inventoryCapacity
  )
import Shared.Item (Item(CuredMeat, Torch))
import Shared.Util (getItem)

import Util (clearRoomBacklog)

-- Items the player can allocate to the expedition before embarking.
-- Order here drives display order on the supply screen.
pathSupplies :: [Item]
pathSupplies = [CuredMeat, Torch]

allocated :: Item -> Game -> Int
allocated i g = Map.findWithDefault 0 i (g ^. expeditionInventory)

-- Stored amount minus what's already been allocated for this expedition.
available :: Item -> Game -> Int
available i g = getItem i g - allocated i g

inventoryUsed :: Game -> Int
inventoryUsed g = sum (Map.elems (g ^. expeditionInventory))

inventoryFree :: Game -> Int
inventoryFree g = (g ^. playerStats . inventoryCapacity) - inventoryUsed g

arrival :: DarkRoom
arrival = do
  location .= Path
  -- Re-allocate from scratch each visit until the player embarks.
  embarked .= False
  expeditionInventory .= Map.empty
  clearRoomBacklog

embark :: DarkRoom
embark = embarked .= True

increaseSupply :: Item -> DarkRoom
increaseSupply i = do
  avail <- gets (available i)
  free <- gets inventoryFree
  when (avail > 0 && free > 0) $
    expeditionInventory %= Map.insertWith (+) i 1

decreaseSupply :: Item -> DarkRoom
decreaseSupply i = do
  cur <- gets (allocated i)
  when (cur > 0) $
    expeditionInventory %= Map.adjust (subtract 1) i
