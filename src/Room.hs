module Room (update) where

import Control.Lens (over, set, view, (&))

import GameTypes (FireState, RoomTemperature(..), Game, events, upcomingEvents,
                  fireValue, roomTemperature)
import GameEvent (GameEvent(RoomChanged), updateEvents)
import Constants (roomWarmDelay)
import Util (addEvent)

-- Defined in GameTypes to avoid an import cycle:
-- data RoomTemperature
--   = Freezing
--   | Cold
--   | Mild
--   | Warm
--   | Hot
--   deriving (Eq, Show, Enum, Ord)

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

newTemperature :: FireState -> RoomTemperature -> RoomTemperature
newTemperature fire room =
  case compare (fromEnum fire) (fromEnum room) of
    LT -> roomPred room
    EQ -> room
    GT -> roomSucc room

update :: Game -> Game
update game =
  let currentTemp = newTemperature (view fireValue game) (view roomTemperature game)

      alwaysChanging =
        game & over upcomingEvents (updateEvents (RoomChanged roomWarmDelay))
             & set roomTemperature currentTemp

      withNotification =
        alwaysChanging
        & over events (addEvent (roomState (view roomTemperature alwaysChanging)))

      temperatureChanged = view roomTemperature game /= currentTemp
  in if temperatureChanged then withNotification else alwaysChanging
