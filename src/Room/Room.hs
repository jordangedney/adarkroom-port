{-# LANGUAGE LambdaCase #-}
module Room.Room
  ( update
  , arrival
  ) where

import Control.Lens
import Control.Monad (unless)

import Shared.Game
import Shared.GameEvent (GameEvent(RoomChanged))
import Shared.Constants (roomWarmDelay)
import Util (notifyRoom, clearRoomBacklog, updateEvent)

import qualified Room.Builder as Builder

roomState :: RoomTemperature -> String
roomState = \case
  Freezing -> "the room is freezing."
  Cold     -> "the room is cold."
  Mild     -> "the room is mild."
  Warm     -> "the room is warm."
  Hot      -> "the room is hot."

roomPred :: RoomTemperature -> RoomTemperature
roomPred = \case { Freezing -> Freezing; x -> pred x }

roomSucc :: RoomTemperature -> RoomTemperature
roomSucc = \case { Hot -> Hot; x -> succ x }

update :: DarkRoom
update = do
  -- compare the room's temp with the fire's
  rT <- use roomTemperature
  newTemp <- newTemperature rT <$> use fireState

  -- and bring the room's closer to the fire's
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
  -- home sweet home
  location .= Room
  clearRoomBacklog
  Builder.canHelp
  Builder.updateBuildables
