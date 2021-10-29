{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Shared.Game where

import GHC.Generics
import Data.Yaml

import Control.Lens (makeLenses)
import Control.Monad.State (State)

import Shared.UI (UIState, uiStateInit)
import Shared.GameEvent (GameEvents, gameEventsInit)
import Shared.RandomEvent (Scene)

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
  { _wood         :: Int
  , _people       :: Int
  , _bait         :: Int
  , _fur          :: Int
  , _meat         :: Int
  , _scales       :: Int
  , _teeth        :: Int
  , _cloth        :: Int
  , _charm        :: Int
  , _compass      :: Int
  , _coal         :: Int
  , _leather      :: Int
  , _iron         :: Int
  , _steel        :: Int
  , _sulphur      :: Int
  --  Craftables
  , _trap         :: Int
  , _cart         :: Int
  , _hut          :: Int
  , _lodge        :: Int
  , _tradingPost  :: Int
  , _tannery      :: Int
  , _smokehouse   :: Int
  , _workshop     :: Int
  , _steelworks   :: Int
  , _armory       :: Int
  , _torch        :: Int
  , _waterskin    :: Int
  , _cask         :: Int
  , _waterTank    :: Int
  , _boneSpear    :: Int
  , _rucksack     :: Int
  , _wagon        :: Int
  , _convoy       :: Int
  , _leatherArmor :: Int
  , _ironArmor    :: Int
  , _steelArmor   :: Int
  , _ironSword    :: Int
  , _steelSword   :: Int
  , _rifle        :: Int
  } deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)

makeLenses ''Stored

storedInit :: Stored
storedInit = Stored
  { _wood         = 15
  , _people       = 0
  , _bait         = 0
  , _fur          = 0
  , _meat         = 0
  , _scales       = 0
  , _teeth        = 0
  , _cloth        = 0
  , _charm        = 0
  , _compass      = 0
  , _coal         = 0
  , _leather      = 0
  , _iron         = 0
  , _steel        = 0
  , _sulphur      = 0
  , _trap         = 0
  , _cart         = 0
  , _hut          = 0
  , _lodge        = 0
  , _tradingPost  = 0
  , _tannery      = 0
  , _smokehouse   = 0
  , _workshop     = 0
  , _steelworks   = 0
  , _armory       = 0
  , _torch        = 0
  , _waterskin    = 0
  , _cask         = 0
  , _waterTank    = 0
  , _boneSpear    = 0
  , _rucksack     = 0
  , _wagon        = 0
  , _convoy       = 0
  , _leatherArmor = 0
  , _ironArmor    = 0
  , _steelArmor   = 0
  , _ironSword    = 0
  , _steelSword   = 0
  , _rifle        = 0
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
  , _hyperspeedAmt      :: Int
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

type DarkRoom = State Game ()

hyperspeedAmt' :: Int
hyperspeedAmt' = 4

initGame :: Game
initGame                = Game
  { _location           = Room
  , _stored             = storedInit
  , _upcomingEvents     = gameEventsInit
  , _events             = []
  , _roomEventBacklog   = []
  , _forestEventBacklog = []
  , _tickCount          = 0
  , _hyperspeedAmt      = hyperspeedAmt'
  , _nextRandomAt       = hyperspeedAmt'
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
