module Path
  (
  arrival
  ) where

import Control.Lens (set, view, (&))

import GameTypes (FireState, RoomTemperature(..), Game, Location(Path),
                  fireValue, roomTemperature, location)
import GameEvent (GameEvent(RoomChanged))
import Constants (roomWarmDelay)

import GameUtil (notifyRoom, clearRoomBacklog, updateEvents)

arrival :: Game -> Game
arrival game =
  game & set location Path
       & clearRoomBacklog
