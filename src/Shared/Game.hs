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
import Shared.Item
import qualified Data.Map as Map

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

data Location = Room | Outside | Path | Ship
  deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)

data Game = Game
  { _location           :: Location
  , _stored             :: Map.Map Item Int
  , _numPeople          :: Int
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
  , _stored             = Map.fromList [(Wood, 15)]
  , _numPeople          = 0
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
