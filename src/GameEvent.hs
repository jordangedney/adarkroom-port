{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module GameEvent where

import GHC.Generics
import Data.Yaml
import Control.Lens (makeLenses, over, (&), set)

data GameEvent
  = UnlockForest Int
  | FireStoked Int
  | FireShrinking Int
  | BuilderUpdate Int
  deriving (Eq, Show, Ord, Generic, ToJSON)

instance FromJSON GameEvent

-- Hacky, but >0 means active, 0 triggers, and <0 means inactive
data GameEvents = GameEvents
  { _unlockForest  :: GameEvent
  , _fireStoked    :: GameEvent
  , _fireShrinking :: GameEvent
  , _builderUpdate :: GameEvent
  } deriving (Eq, Show, Ord, Generic, ToJSON)

instance FromJSON GameEvents
makeLenses ''GameEvents

gameEventsInit :: GameEvents
gameEventsInit = GameEvents
  { _unlockForest  = UnlockForest  (-1)
  , _fireStoked    = FireStoked    (-1)
  , _fireShrinking = FireShrinking (-1)
  , _builderUpdate = BuilderUpdate (-1)
  }

toList :: GameEvents -> [GameEvent]
toList gameEvent  =
  map ($ gameEvent)
  [ _unlockForest
  , _fireStoked
  , _fireShrinking
  , _builderUpdate
  ]

tickEvents :: GameEvents -> GameEvents
tickEvents gameEvent =
  gameEvent & over unlockForest  eventDec
            & over fireStoked    eventDec
            & over fireShrinking eventDec
            & over builderUpdate eventDec

updateEvents :: GameEvent -> GameEvents -> GameEvents
updateEvents event gameEvents = gameEvents & set (eventGetter event) event

eventDec :: GameEvent -> GameEvent
eventDec (UnlockForest    x) = UnlockForest    (x - 1)
eventDec (FireStoked      x) = FireStoked      (x - 1)
eventDec (FireShrinking   x) = FireShrinking   (x - 1)
eventDec (BuilderUpdate   x) = BuilderUpdate   (x - 1)

getTime :: GameEvent -> Int
getTime  (UnlockForest    x) = x
getTime  (FireStoked      x) = x
getTime  (FireShrinking   x) = x
getTime  (BuilderUpdate   x) = x

isActive :: GameEvent -> Bool
isActive (UnlockForest    x) = x > 0
isActive (FireStoked      x) = x > 0
isActive (FireShrinking   x) = x > 0
isActive (BuilderUpdate   x) = x > 0

eventGetter
  :: Functor f
  => GameEvent
  -> ((GameEvent -> f GameEvent) -> GameEvents -> f GameEvents)
eventGetter (UnlockForest  _) = unlockForest
eventGetter (FireStoked    _) = fireStoked
eventGetter (FireShrinking _) = fireShrinking
eventGetter (BuilderUpdate _) = builderUpdate
