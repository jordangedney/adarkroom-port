{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Room.Room
  ( update
  , arrival
  , maxPopulation
  , increasePopulation
  ) where

import Control.Lens
import Control.Monad (unless)
import System.Random (StdGen, randomR)
import Control.Monad.State (gets)

import Shared.Game
import Shared.Item
import Shared.Util
import Shared.GameEvent (GameEvent(RoomChanged, PopulationIncrease))
import Shared.Constants (roomWarmDelay, minutes)
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

maxPopulation :: Game -> Int
maxPopulation game = getItem Hut game * 4

increasePopulation :: StdGen -> DarkRoom
increasePopulation stdGen = do
  let (nextIncrease :: Int, stdGen') = randomR (minutes 1, minutes 3) stdGen
  updateEvent PopulationIncrease nextIncrease
  space <- gets (\g -> maxPopulation g - (view numPeople g))
  let halfSpace = space `quot` 2
      newPeople'= fst (randomR (1, halfSpace) stdGen') + halfSpace
      newPeople = if newPeople' > 0 then newPeople' else 1
      arrivalMessage | newPeople <  1  = error "learn to math, dickhead."
                     | newPeople == 1  = "a stranger arrives in the night."
                     | newPeople <  5  = "a weathered family takes up in one of the huts."
                     | newPeople <  10 = "a small group arrives, all dust and bones."
                     | newPeople <  30 = "a convoy lurches in, equal parts worry and hope."
                     | otherwise       = "the town's booming. word does get around."

  unless (space == 0) $ do
    notifyRoom arrivalMessage
    numPeople %= (+ newPeople)
