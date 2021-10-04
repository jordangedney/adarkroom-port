module Path
  ( arrival
  ) where

import Control.Lens (set, view, (&))

import Shared.Game (Game, Location(Path), location)
import Shared.GameEvent (GameEvent(RoomChanged))
import Shared.Constants (roomWarmDelay)

import Util (notifyRoom, clearRoomBacklog, updateEvents)

arrival :: Game -> Game
arrival game =
  game & set location Path
       & clearRoomBacklog
