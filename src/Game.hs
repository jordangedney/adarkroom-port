module Game where

data Tick = Tick

data Game = Game
  { _location :: String
  , _stored :: [(String, Int)]
  , _events :: [String]
  , _tickCount :: Int
  } deriving (Show)

initGame :: IO Game
initGame = return $ Game " A Dark Room "

                         [("wood", 10)
                         ,("scales", 150)
                         ]

                         ["the stranger is standing by the fire. she says she can help. says she builds things"]

                         0
