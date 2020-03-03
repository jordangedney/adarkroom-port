{-# LANGUAGE TemplateHaskell #-}

module GameTypes where

import Control.Lens (makeLenses)

import UIState

data Tick = Tick deriving (Show, Eq, Ord)

newtype Milestones = Milestones
  { _fireLit :: Bool
  } deriving (Show, Eq, Ord)

data Stored = Stored
  { _wood :: Int
  , _scales :: Int
  } deriving (Show, Eq, Ord)

data GameEvent
  = UnlockForest Int
  | FireStoked Int
  | FireShrinking Int
  | BuilderUpdate Int
  deriving (Show, Eq, Ord)

-- Hacky, but >0 means active, 0 triggers, and <0 means inactive
data GameEvents = GameEvents
  { _unlockForest  :: GameEvent
  , _fireStoked    :: GameEvent
  , _fireShrinking :: GameEvent
  , _builderUpdate :: GameEvent
  } deriving (Show, Eq, Ord)

data Location = Room | Outside | Path | Ship deriving (Eq, Show, Ord)

data Game = Game
  { _location :: Location
  , _stored :: Stored
  , _upcomingEvents :: GameEvents
  , _events :: [(String, Int)]
  , _tickCount :: Int
  , _uiState :: UIState
  , _fireValue :: FireState
  , _temperatureValue :: Int
  , _builderLevel :: Int
  , _progressAmount :: Float
  , _milestones :: Milestones
  } deriving (Eq, Show, Ord)

data FireState
  = Dead
  | Smouldering
  | Flickering
  | Burning
  | Roaring
  deriving (Eq, Show, Enum, Ord)

makeLenses ''Milestones
makeLenses ''Stored
makeLenses ''GameEvents
makeLenses ''Game
