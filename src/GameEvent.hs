{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module GameEvent where

import GHC.Generics
import Data.Yaml
import Control.Lens (makeLenses, over, view, (&))

data GameEvent
  = UnlockForest
  | GatherWood
  | FireStoked
  | FireShrinking
  | BuilderUpdate
  | RoomChanged
  | BuilderGathersWood
  | UnlockTraps
  deriving (Eq, Enum, Show, Ord, Generic, ToJSON, FromJSON)

-- Hacky, but >0 means active, 0 triggers, and <0 means inactive
data GameEvents = GameEvents
  { _unlockForest       :: (GameEvent, Int)
  , _gatherWood         :: (GameEvent, Int)
  , _fireStoked         :: (GameEvent, Int)
  , _fireShrinking      :: (GameEvent, Int)
  , _builderUpdate      :: (GameEvent, Int)
  , _roomChanged        :: (GameEvent, Int)
  , _builderGathersWood :: (GameEvent, Int)
  , _unlockTraps        :: (GameEvent, Int)
  } deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)

makeLenses ''GameEvents

gameEventsInit :: GameEvents
gameEventsInit = GameEvents
  { _unlockForest       = (UnlockForest,       -1)
  , _gatherWood         = (GatherWood,         -1)
  , _fireStoked         = (FireStoked,         -1)
  , _fireShrinking      = (FireShrinking,       1)
  , _builderUpdate      = (BuilderUpdate,      -1)
  , _roomChanged        = (RoomChanged,         1)
  , _builderGathersWood = (BuilderGathersWood, -1)
  , _unlockTraps        = (UnlockTraps,        -1)
  }

eventGetter :: Functor f
  => GameEvent -> ((GameEvent, Int) -> f (GameEvent, Int)) -> GameEvents
  -> f GameEvents
eventGetter UnlockForest       = unlockForest
eventGetter GatherWood         = gatherWood
eventGetter FireStoked         = fireStoked
eventGetter FireShrinking      = fireShrinking
eventGetter BuilderUpdate      = builderUpdate
eventGetter RoomChanged        = roomChanged
eventGetter BuilderGathersWood = builderGathersWood
eventGetter UnlockTraps        = unlockTraps

-- Helper Functions ------------------------------------------------------------
toList :: GameEvents -> [(GameEvent, Int)]
toList gameEvents = allEvents & map eventGetter & map (`view` gameEvents)

tickEvents :: GameEvents -> GameEvents
tickEvents gameEvents =
  let allEventsDec = map (\fn -> over (eventGetter fn) eventDec) allEvents
      eventDec (a, b) = (a, b - 1)
  in foldl (\gameEvents' tickEvent -> tickEvent gameEvents') gameEvents allEventsDec

isActive :: (GameEvent, Int) -> Bool
isActive (_, x) = x > 0

allEvents :: [GameEvent]
allEvents = enumFrom (toEnum 0)
