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

data Stored = Stored
  { _wood :: Int
  , _scales :: Int
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

makeLenses ''Milestones
makeLenses ''Stored
makeLenses ''Game
