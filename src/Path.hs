module Path
  ( arrival
  ) where

import Control.Lens (set, (&))

import Shared.Game (Game, Location(Path), location)

import Util (clearRoomBacklog)

arrival :: Game -> Game
arrival game =
  game & set location Path
       & clearRoomBacklog
