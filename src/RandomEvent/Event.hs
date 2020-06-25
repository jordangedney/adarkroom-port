{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module RandomEvent.Event where

import GHC.Generics
import Data.Yaml

-- import RandomEvent.EventType (RandomEvent(..))

-- This should be in GameTypes, but dependency cycles are a bitch
data Item = Fur | Cloth | Scale | Teeth
  deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)

data Scene = Scene
  { title :: String
  -- , isAvailable :: Game -> Bool
  , currentScene :: SceneEvent
  } deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)

data SceneEvent = SceneEvent
  { text :: [String]
  , notification :: Maybe String
--   , blink :: Bool
  , reward :: Maybe (Item, Int)
  , choices :: [SceneChoice]
  } deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)

data SceneChoice = SceneChoice
  { choiceTxt :: String
  , cost :: Maybe (Item, Int)
  , nextScene :: Maybe [(Float, SceneEvent)]
  } deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)

theBeggar :: Scene
theBeggar = Scene
  { title = "The Beggar"
  -- , isAvailable = (\g -> view location g == Room && view (stored . fur) g > 0)
  , currentScene = start
  }
  where start = SceneEvent
          { text = [ "a beggar arrives."
                   , "asks for any spare furs to keep him warm at night."]
          , notification = Just "a beggar arrives"
          -- , blink = True
          , reward = Nothing
          , choices =
            [ SceneChoice { choiceTxt = "give 50"
                          , cost = Just (Fur, 50)
                          , nextScene = Just [
                              (0.5, scales'), (0.8, teeth'), (1.0, cloth')]
                          }

            , SceneChoice { choiceTxt = "give 100"
                          , cost = Just (Fur, 100)
                          , nextScene = Just [
                              (0.5, teeth'), (0.8, scales'), (1.0, cloth')]
                          }

            , SceneChoice { choiceTxt = "turn him away"
                          , cost = Nothing
                          , nextScene = Nothing
                          }
            ]
          }
        scales' = SceneEvent
          { text = [ "the beggar expresses his thanks.",
                     "leaves a pile of small scales behind." ]
          , notification = Nothing
          , reward = Just (Scale, 20)
          , choices = [ SceneChoice { choiceTxt = "say goodbye"
                                    , cost = Nothing
                                    , nextScene = Nothing
                                    }
                      ]
          }
        teeth' = SceneEvent
          { text = [ "the beggar expresses his thanks.",
                     "leaves a pile of small teeth behind." ]
          , notification = Nothing
          , reward = Just (Teeth, 20)
          , choices = [ SceneChoice { choiceTxt = "say goodbye"
                                    , cost = Nothing
                                    , nextScene = Nothing
                                    }
                      ]
          }
        cloth' = SceneEvent
          { text = [ "the beggar expresses his thanks.",
                     "leaves some scraps of cloth behind."]
          , notification = Nothing
          , reward = Just (Cloth, 20)
          , choices = [ SceneChoice { choiceTxt = "say goodbye"
                                    , cost = Nothing
                                    , nextScene = Nothing
                                    }
                      ]
          }
