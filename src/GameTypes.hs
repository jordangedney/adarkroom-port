{-# LANGUAGE TemplateHaskell #-}

module GameTypes where

import Control.Lens (makeLenses)

import UIState
import GameEvent

data Tick = Tick deriving (Show, Eq, Ord)

data FireState
  = Dead
  | Smouldering
  | Flickering
  | Burning
  | Roaring
  deriving (Eq, Show, Enum, Ord)

newtype Milestones = Milestones
  { _fireLit :: Bool
  } deriving (Show, Eq, Ord)

makeLenses ''Milestones

milestonesInit :: Milestones
milestonesInit = Milestones
  { _fireLit = False }

data Stored = Stored
  { _wood :: Int
  , _scales :: Int
  } deriving (Show, Eq, Ord)

makeLenses ''Stored

storedInit :: Stored
storedInit = Stored
  { _wood   = 100
  , _scales = 0
  }

data Location = Room | Outside | Path | Ship deriving (Eq, Show, Ord)

data Game = Game
  { _location         :: Location
  , _stored           :: Stored
  , _upcomingEvents   :: GameEvents
  , _events           :: [(String, Int)]
  , _tickCount        :: Int
  , _uiState          :: UIState
  , _fireValue        :: FireState
  , _temperatureValue :: Int
  , _builderLevel     :: Int
  , _progressAmount   :: Float
  , _milestones       :: Milestones
  } deriving (Eq, Show, Ord)

makeLenses ''Game

initGame :: IO Game
initGame              = return $ Game
  { _location         = Room
  , _stored           = storedInit
  , _upcomingEvents   = gameEventsInit
  , _events           = [ ("the fire is dead.", 0)
                        , ("the room is freezing.", 0)
                        ]
  , _tickCount        = 0
  , _uiState          = uiStateInit
  , _fireValue        = Dead
  , _temperatureValue = 0
  , _builderLevel     = 0
  , _progressAmount   = 0.5
  , _milestones       = milestonesInit
  }
