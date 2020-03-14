{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module GameEvent where

import GHC.Generics
import Data.Yaml
import Control.Lens (makeLenses, over, (&), set)

data GameEvent
  = UnlockForest
  | GatherWood
  | FireStoked
  | FireShrinking
  | BuilderUpdate
  | RoomChanged
  deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)

-- Hacky, but >0 means active, 0 triggers, and <0 means inactive
data GameEvents = GameEvents
  { _unlockForest  :: (GameEvent, Int)
  , _gatherWood    :: (GameEvent, Int)
  , _fireStoked    :: (GameEvent, Int)
  , _fireShrinking :: (GameEvent, Int)
  , _builderUpdate :: (GameEvent, Int)
  , _roomChanged   :: (GameEvent, Int)
  } deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)

makeLenses ''GameEvents

gameEventsInit :: GameEvents
gameEventsInit = GameEvents
  { _unlockForest  = (UnlockForest,  -1)
  , _gatherWood    = (GatherWood,    -1)
  , _fireStoked    = (FireStoked,    -1)
  , _fireShrinking = (FireShrinking,  1)
  , _builderUpdate = (BuilderUpdate, -1)
  , _roomChanged   = (RoomChanged,    1)
  }

toList :: GameEvents -> [(GameEvent, Int)]
toList gameEvent  =
  map ($ gameEvent)
  [ _unlockForest
  , _gatherWood
  , _fireStoked
  , _fireShrinking
  , _builderUpdate
  , _roomChanged
  ]

tickEvents :: GameEvents -> GameEvents
tickEvents gameEvent =
  gameEvent & over unlockForest  eventDec
            & over gatherWood    eventDec
            & over fireStoked    eventDec
            & over fireShrinking eventDec
            & over builderUpdate eventDec
            & over roomChanged   eventDec

eventGetter UnlockForest  = unlockForest
eventGetter GatherWood    = gatherWood
eventGetter FireStoked    = fireStoked
eventGetter FireShrinking = fireShrinking
eventGetter BuilderUpdate = builderUpdate
eventGetter RoomChanged   = roomChanged

eventDec (a, b) = (a, b - 1)

getTime = snd

isActive (_, x) = x > 0
