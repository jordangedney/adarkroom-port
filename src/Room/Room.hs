module Room.Room
  ( update
  , arrival
  ) where

import Control.Lens

import Shared.Game
import Shared.GameEvent (GameEvent(RoomChanged))
import Shared.Constants (roomWarmDelay)

import Util (notifyRoom, clearRoomBacklog, updateEvent)

import qualified Room.Builder as Builder
import Control.Monad (unless)

roomState :: RoomTemperature -> String
roomState Freezing = "the room is freezing."
roomState Cold     = "the room is cold."
roomState Mild     = "the room is mild."
roomState Warm     = "the room is warm."
roomState Hot      = "the room is hot."

roomPred :: RoomTemperature -> RoomTemperature
roomPred Freezing = Freezing
roomPred x = pred x

roomSucc :: RoomTemperature -> RoomTemperature
roomSucc Hot = Hot
roomSucc x = succ x

update :: DarkRoom
update = do
  rT <- use roomTemperature
  newTemp <- newTemperature rT <$> use fireState

  unless (rT == newTemp) $ do
    roomTemperature .= newTemp
    notifyRoom (roomState newTemp)

  updateEvent RoomChanged roomWarmDelay

  where newTemperature room fire =
          case compare (fromEnum fire) (fromEnum room) of
            LT -> roomPred room
            EQ -> room
            GT -> roomSucc room

arrival :: DarkRoom
arrival = do
  location .= Room
  clearRoomBacklog
  Builder.canHelp
  Builder.updateBuildables
