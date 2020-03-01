module Game where

import UIState

data Tick = Tick

initGame :: IO Game
initGame = return $ Game
  { location = "A Dark Room"
  , stored = [("wood", 10)
             ,("scales", 150)
             ]
  , upcomingEvents = []
  , events = [ "the fire is dead."
             , "the room is freezing."
             ]
  , tickCount = 0
  , uiState = UIState { lastReportedClick = Nothing
                      , showStores = showStoresInit
                      , showOutside = False
                      }
  , fireValue = 0
  , temperatureValue = 0
  , builderLevel = 0
  , progressAmount = 0.5
  , milestones = Milestones {fireLit = False}
  }

data Milestones = Milestones
  { fireLit :: Bool

  }

data Game = Game
  { location :: String
  , stored :: [(String, Int)]
  , upcomingEvents :: [(Int, GameEvent, Game -> Game)]
  , events :: [String]
  , tickCount :: Int
  , uiState :: UIState
  , fireValue :: Int
  , temperatureValue :: Int
  , builderLevel :: Int
  , progressAmount :: Float
  , milestones :: Milestones
  }

data GameEvent =
    AllowedOutside
  | FireStoked
  deriving (Show, Eq)

handleGameEvents g AllowedOutside = g {uiState = (uiState g) {showOutside = True}}
