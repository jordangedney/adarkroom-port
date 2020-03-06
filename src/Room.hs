module Room
where

import Control.Lens (over, set, view, (&))

import GameTypes (FireState, RoomTemperature(..), Game, events, upcomingEvents,
                  roomTemperature)
import GameEvent (GameEvent(BuilderUpdate), updateEvents)
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

roomTemperature :: RoomTemperature -> String
roomTemperature Freezing = "the room is freezing."
roomTemperature Cold     = "the room is cold."
roomTemperature Mild     = "the room is mild."
roomTemperature Warm     = "the room is warm."
roomTemperature Hot      = "the room is hot."

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

-- roomChanged :: Game -> Game
-- roomChanged game =
--   let showRoom = game & over events (addEvent (roomState (view roomTemperature game)))
--       roomIsBurning = view roomTemperature game /= Dead
--       roomContinuesBurning =
--         showRoom & over upcomingEvents (updateEvents (RoomShrinking roomCoolDelay))
--   in if roomIsBurning then roomContinuesBurning else showRoom
--
--
-- shrinking :: Game -> Game
-- shrinking game =
--   game & over roomTemperature roomPred
--        & roomChanged
