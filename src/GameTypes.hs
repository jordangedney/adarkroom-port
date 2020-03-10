{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module GameTypes where

import GHC.Generics
import Data.Yaml

import Control.Lens (makeLenses)

import UIState (UIState, uiStateInit)
import GameEvent (GameEvents, gameEventsInit)

data Tick = Tick deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- FireState and RoomState are comparable
data FireState
  = Dead
  | Smouldering
  | Flickering
  | Burning
  | Roaring
  deriving (Eq, Show, Enum, Ord, Generic, ToJSON, FromJSON)

data RoomTemperature
  = Freezing
  | Cold
  | Mild
  | Warm
  | Hot
  deriving (Eq, Show, Enum, Ord, Generic, ToJSON, FromJSON)

data Milestones = Milestones
  { _fireLit        :: Bool
  , _builderArrived :: Bool
  , _seenForest     :: Bool
  } deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)

makeLenses ''Milestones

milestonesInit :: Milestones
milestonesInit = Milestones
  { _fireLit        = False
  , _builderArrived = False
  , _seenForest     = False
  }

data Stored = Stored
  { _wood :: Int
  , _scales :: Int
  } deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)

makeLenses ''Stored

storedInit :: Stored
storedInit = Stored
  { _wood   = 100
  , _scales = 0
  }

data Location = Room | Outside | Path | Ship
  deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)


data Game = Game
  { _location         :: Location
  , _stored           :: Stored
  , _upcomingEvents   :: GameEvents
  , _events           :: [(String, Int)]
  , _tickCount        :: Int
  , _uiState          :: UIState
  , _fireValue        :: FireState
  , _roomTemperature  :: RoomTemperature
  , _builderLevel     :: Int
  , _progressAmount   :: Float
  , _milestones       :: Milestones
  , _hyper            :: Bool
  , _debug            :: Bool
  , _previousStates   :: [Game]
  , _paused           :: Bool
  } deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)

makeLenses ''Game

initGame :: Game
initGame              = Game
  { _location         = Room
  , _stored           = storedInit
  , _upcomingEvents   = gameEventsInit
  , _events           = [ ("the fire is dead.", 0)
                        , ("the room is freezing.", 0)
                        ]
  , _tickCount        = 0
  , _uiState          = uiStateInit
  , _fireValue        = Dead
  , _roomTemperature  = Freezing
  , _builderLevel     = 0
  , _progressAmount   = 0.5
  , _milestones       = milestonesInit
  , _hyper            = True
  , _debug            = True
  , _previousStates   = []
  , _paused           = False
  }
