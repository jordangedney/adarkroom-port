{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Shared.Game where


import GHC.Generics
import Data.Yaml
import Data.Aeson.Types (ToJSONKey, FromJSONKey)

import Control.Lens (makeLenses)
import Control.Monad.State (State)

import Shared.UI (UIState, uiStateInit)
import Shared.GameEvent (gameEventsInit, GameEvent)
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

data Worker
  = Gatherer
  | Hunter
  | Trapper
  | Tanner
  | Charcutier
  | IronMiner
  | CoalMiner
  | SulphurMiner
  | Steelworker
  | Armourer
  deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

data Milestones = Milestones
  { _fireLit             :: Bool
  , _seenForest          :: Bool
  , _buildUnlocked       :: Bool
  , _craftUnlocked       :: Bool
  , _buyUnlocked         :: Bool
  , _weaponsUnlocked     :: Bool
  } deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)

makeLenses ''Milestones

milestonesInit :: Milestones
milestonesInit = Milestones
  { _fireLit          = False
  , _seenForest       = False
  , _buildUnlocked    = False
  , _craftUnlocked    = False
  , _buyUnlocked      = False
  , _weaponsUnlocked  = False
  }

data PlayerStats = PlayerStats
  { _hp                :: Int
  , _maxHp             :: Int
  , _waterCapacity     :: Int
  , _inventoryCapacity :: Int
  , _armor             :: Int
  } deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)

makeLenses ''PlayerStats

playerStatsInit :: PlayerStats
playerStatsInit = PlayerStats
  { _hp                = 10
  , _maxHp             = 10
  , _waterCapacity     = 0
  , _inventoryCapacity = 10
  , _armor             = 0
  }

data Location = Room | Outside | Path | Ship
  deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)

data Game = Game
  { _location           :: Location
  , _stored             :: Map.Map Item Int
  , _upcomingEvents     :: Map.Map GameEvent Int
  , _workers            :: Map.Map Worker Int
  , _notifications      :: [(String, Int)]
  , _roomEventBacklog   :: [String]
  , _forestEventBacklog :: [String]
  , _tickCount          :: Int
  , _uiState            :: UIState
  , _fireState          :: FireState
  , _roomTemperature    :: RoomTemperature
  , _builderState       :: BuilderState
  , _progressAmount     :: Float
  , _milestones         :: Milestones
  , _playerStats        :: PlayerStats
  , _expedition         :: Map.Map Item Int
  , _embarked           :: Bool
  , _hyper              :: Bool
  , _debug              :: Bool
  , _previousStates     :: [Game]
  , _paused             :: Bool
  , _inEvent            :: Maybe Scene
  } deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)

makeLenses ''Game

type DarkRoom = State Game ()

initGame :: Game
initGame                = Game
  { _location           = Room
  , _stored             = Map.fromList [(Wood, 15)]
  , _upcomingEvents     = gameEventsInit
  , _workers            = Map.fromList []
  , _notifications      = []
  , _roomEventBacklog   = []
  , _forestEventBacklog = []
  , _tickCount          = 0
  , _uiState            = uiStateInit
  , _fireState          = Dead
  , _roomTemperature    = Cold -- instead of Freezing so initial status is displayed
  , _builderState       = Approaching
  , _progressAmount     = 0.5
  , _milestones         = milestonesInit
  , _playerStats        = playerStatsInit
  , _expedition         = Map.empty
  , _embarked           = False
  , _hyper              = True
  , _debug              = True
  , _previousStates     = []
  , _paused             = False
  , _inEvent            = Nothing
  }
