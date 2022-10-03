{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Shared.RandomEvent where

import Shared.Item

import GHC.Generics
import Data.Yaml

data Scene = Scene
  { title :: String
  -- Used to hold the window position to a set size
  -- (so it doesn't change size between scenes)
  , windowSize :: Int
  , currentScene :: SceneEvent
  } deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)

-- The reward system is (probably overly) complicated
data Reward
  -- Give: an item directly
  = Give Item Int
  -- GiveSome: (Item1, Percentage, Item2, Percentage2)
  -- Item1 gets reduced by percentage, Item2 is gained by Percentage2 of Percentage
  | EquivalentExchange Item Int Item Int
  -- Gives a random amount of the item in the range
  | GiveRange Item (Int, Int)
  -- Combine multiple reward types
  deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)

data StayOrGo = Stay (Maybe String) (Item, Int) | Go ([(Int, SceneEvent)], SceneEvent)
  deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)

data SceneEvent = SceneEvent
  { text :: [String]
  , notification :: Maybe String
  , reward :: [Reward]
  , choices :: [SceneChoice]
  } deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)

data SceneChoice = SceneChoice
  { choiceTxt :: String
  , cost :: [(Item, Int)]
  , nextScene :: Maybe StayOrGo
  , choiceNotification :: Maybe String
  } deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)
