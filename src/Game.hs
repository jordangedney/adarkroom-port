module Game where

import UIState

data GameEvent = Tick | UnlockOutside

data Game = Game
  { location :: String
  , stored :: [(String, Int)]
  , events :: [String]
  , tickCount :: Int
  , uiState :: UIState
  , fireValue :: Int
  , temperatureValue :: Int
  } deriving (Show)

initGame :: IO Game
initGame = return $ Game "A Dark Room"

                         [("wood", 10)
                         ,("scales", 150)
                         ]

                         [ "the fire is dead."
                         , "the room is freezing."
                         ]

                         0

                         (UIState Nothing showStoresInit)

                         0

                         0
