{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module RandomEvent.Event where

import GHC.Generics
import Data.Yaml

-- This should be in GameTypes, but dependency cycles are a bitch
data Item = Fur | Cloth | Scale | Teeth | Bait | Compass | Wood
  deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)

data Scene = Scene
  { title :: String
  -- Used to hold the window position to a set size
  -- (so it doesn't change size between scenes)
  , windowSize :: Int
  , currentScene :: SceneEvent
  } deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)

-- GiveSome: (Item1, Percentage, Item2, Percentage2)
-- Item1 gets reduced by percentage, Item2 is gained by Percentage2 of Percentage
data Reward
  = Give [(Item, Int)]
  | GiveSome [(Item, Int, Item, Int)]
  | None
  deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)

data SceneEvent = SceneEvent
  { text :: [String]
  , notification :: Maybe String
--   , blink :: Bool
  , reward :: Reward
  , choices :: [SceneChoice]
  } deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)

data StayOrGo = Stay (Maybe String) (Item, Int) | Go ([(Int, SceneEvent)], SceneEvent)
  deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)

data SceneChoice = SceneChoice
  { choiceTxt :: String
  , cost :: [(Item, Int)]
  , nextScene :: Maybe StayOrGo
  } deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)
