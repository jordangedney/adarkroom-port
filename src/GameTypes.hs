{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module GameTypes where

import GHC.Generics
import Data.Yaml

import Control.Lens (makeLenses)

import UI.State (UIState, uiStateInit)
import GameEvent (GameEvents, gameEventsInit)
import RandomEvent.Event (Scene)

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

data BuilderState
  = Approaching
  | Collapsed
  | Shivering
  | Sleeping
  | Helping
  deriving (Eq, Show, Enum, Ord, Generic, ToJSON, FromJSON)

data Milestones = Milestones
  { _fireLit             :: Bool
  , _builderIsHelping    :: Bool
  , _seenForest          :: Bool
  , _trapsUnlocked       :: Bool
  , _craftUnlocked       :: Bool
  , _buyUnlocked         :: Bool
  , _preCartsUnlocked    :: Bool
  , _cartsUnlocked       :: Bool
  } deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)

makeLenses ''Milestones

milestonesInit :: Milestones
milestonesInit = Milestones
  { _fireLit          = False
  , _builderIsHelping = False
  , _seenForest       = False
  , _trapsUnlocked    = False
  , _craftUnlocked    = False
  , _buyUnlocked      = False
  , _preCartsUnlocked = False
  , _cartsUnlocked    = False
  }

data Stored = Stored
  { _wood   :: Int
  , _traps  :: Int
  , _carts  :: Int
  , _huts   :: Int
  , _people :: Int
  , _bait   :: Int
  , _fur    :: Int
  , _meat   :: Int
  , _scales :: Int
  , _teeth  :: Int
  , _cloth  :: Int
  , _charm  :: Int
  } deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)

makeLenses ''Stored

storedInit :: Stored
storedInit = Stored
  { _wood    = 100
  , _traps   = 0
  , _carts   = 0
  , _huts    = 0
  , _people  = 0
  , _bait    = 0
  , _fur     = 0
  , _meat    = 0
  , _scales  = 0
  , _teeth   = 0
  , _cloth   = 0
  , _charm   = 0
  }

data Location = Room | Outside | Path | Ship
  deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)


data Game = Game
  { _location           :: Location
  , _stored             :: Stored
  , _upcomingEvents     :: GameEvents
  , _events             :: [(String, Int)]
  , _roomEventBacklog   :: [String]
  , _forestEventBacklog :: [String]
  , _tickCount          :: Int
  , _nextRandomAt       :: Int
  , _uiState            :: UIState
  , _fireValue          :: FireState
  , _roomTemperature    :: RoomTemperature
  , _builderState       :: BuilderState
  , _progressAmount     :: Float
  , _milestones         :: Milestones
  , _hyper              :: Bool
  , _debug              :: Bool
  , _previousStates     :: [Game]
  , _paused             :: Bool
  , _inEvent            :: Maybe Scene
  } deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)

makeLenses ''Game

initGame :: Game
initGame                = Game
  { _location           = Room
  , _stored             = storedInit
  , _upcomingEvents     = gameEventsInit
  , _events             = []
  , _roomEventBacklog   = []
  , _forestEventBacklog = []
  , _tickCount          = 0
  , _nextRandomAt       = 1
  , _uiState            = uiStateInit
  , _fireValue          = Dead
  , _roomTemperature    = Cold -- instead of Freezing so initial status is displayed
  , _builderState       = Approaching
  , _progressAmount     = 0.5
  , _milestones         = milestonesInit
  , _hyper              = True
  , _debug              = True
  , _previousStates     = []
  , _paused             = False
  , _inEvent            = Nothing
  }
