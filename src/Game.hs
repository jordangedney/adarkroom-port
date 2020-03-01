{-# LANGUAGE TemplateHaskell #-}

module Game where

-- import Control.Lens hiding (element)
import Control.Lens (makeLenses, set)

import UIState

data Tick = Tick

initGame :: IO Game
initGame = return $ Game
  { _location = "A Dark Room"
  , _stored = [("wood", 10)
              ,("scales", 150)
              ]
  , _upcomingEvents = []
  , _events = [ "the fire is dead."
              , "the room is freezing."
              ]
  , _tickCount = 0
  , _uiState = UIState { _lastReportedClick = Nothing
                       , _showStores = showStoresInit
                       , _showOutside = False
                       }
  , _fireValue = 0
  , _temperatureValue = 0
  , _builderLevel = 0
  , _progressAmount = 0.5
  , _milestones = Milestones {_fireLit = False}
  }

data Milestones = Milestones
  { _fireLit :: Bool }

data Game = Game
  { _location :: String
  , _stored :: [(String, Int)]
  , _upcomingEvents :: [(Int, GameEvent, Game -> Game)]
  , _events :: [String]
  , _tickCount :: Int
  , _uiState :: UIState
  , _fireValue :: Int
  , _temperatureValue :: Int
  , _builderLevel :: Int
  , _progressAmount :: Float
  , _milestones :: Milestones
  }

data GameEvent =
    AllowedOutside
  | FireStoked
  deriving (Show, Eq)

makeLenses ''Milestones
makeLenses ''Game

handleGameEvents :: GameEvent -> Game -> Game
handleGameEvents AllowedOutside = set (uiState . showOutside) True
