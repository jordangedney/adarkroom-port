module Path
  ( arrival
  ) where

import Control.Lens

import Shared.Game (DarkRoom, Location(Path), location)

import Util (clearRoomBacklog)

arrival :: DarkRoom
arrival = do
  location .= Path
  clearRoomBacklog
