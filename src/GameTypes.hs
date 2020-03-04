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

initGame :: IO Game
initGame = return $ Game
  { _location = Room
  , _stored = Stored { _wood = 100
                     , _scales = 0
                     }
  , _upcomingEvents = GameEvents { _unlockForest  = UnlockForest  (-1)
                                 , _fireStoked    = FireStoked    (-1)
                                 , _fireShrinking = FireShrinking (-1)
                                 , _builderUpdate = BuilderUpdate (-1)
                                }
  , _events = [ ("the fire is dead.", 0)
              , ("the room is freezing.", 0)
              ]
  , _tickCount = 0
  , _uiState = UIState { _lastReportedClick = Nothing
                       , _showStores = showStoresInit
                       , _showOutside = False
                       , _showPath = False
                       , _showShip = False
                       }
  , _fireValue = Dead
  , _temperatureValue = 0
  , _builderLevel = 0
  , _progressAmount = 0.5
  , _milestones = Milestones {_fireLit = False}
  }
