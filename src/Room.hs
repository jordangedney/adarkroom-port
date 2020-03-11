{-# LANGUAGE TupleSections #-}

module Room
  ( update
  , arrival
  , notify
  ) where

import Control.Lens (over, set, view, (&))

import GameTypes (FireState, RoomTemperature(..), Game, Location(Room),
                  events, upcomingEvents, fireValue, roomTemperature, location,
                  roomEventBacklog)
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

notify :: String -> Game -> Game
notify message game =
  if view location game == Room
  then game & over events (addEvent message)
  else game & over roomEventBacklog ((:) message)

update :: Game -> Game
update game =
  let currentTemp = newTemperature (view fireValue game) (view roomTemperature game)

      alwaysChanging =
        game & over upcomingEvents (updateEvents (RoomChanged roomWarmDelay))
             & set roomTemperature currentTemp

      withNotification =
        alwaysChanging & notify (roomState (view roomTemperature alwaysChanging))

      temperatureChanged = view roomTemperature game /= currentTemp
  in if temperatureChanged then withNotification else alwaysChanging

arrival :: Game -> Game
arrival game =
  game & set location Room
       & over events (\es ->  map (, 0) (view roomEventBacklog game) ++ es)
