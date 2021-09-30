{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module RandomEvent.Event where

import GHC.Generics
import Data.Yaml

-- import RandomEvent.EventType (RandomEvent(..))

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

data SceneEvent = SceneEvent
  { text :: [String]
  , notification :: Maybe String
--   , blink :: Bool
  , reward :: Maybe [(Item, Int)]
  , choices :: [SceneChoice]
  } deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)

data StayOrGo = Stay (Maybe String) (Item, Int) | Go ([(Int, SceneEvent)], SceneEvent)
  deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)

data SceneChoice = SceneChoice
  { choiceTxt :: String
  , cost :: Maybe [(Item, Int)]
  , nextScene :: Maybe StayOrGo
  } deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)

theBeggar :: Scene
theBeggar = Scene
  { title = "The Beggar"
  , windowSize = 52
  , currentScene = start
  }
  where start = SceneEvent
          { text = [ "a beggar arrives."
                   , "\n"
                   , "asks for any spare furs to keep him warm at night."]
          , notification = Just "a beggar arrives"
          -- , blink = True
          , reward = Nothing
          , choices =
            [ SceneChoice { choiceTxt = "give 50"
                          , cost = Just [(Fur, 50)]
                          , nextScene = Just $ Go ([
                              (50, scales'), (30, teeth')], cloth')
                          }

            , SceneChoice { choiceTxt = "give 100"
                          , cost = Just [(Fur, 100)]
                          , nextScene = Just $ Go ([
                              (50, teeth'), (30, scales')],  cloth')
                          }

            , SceneChoice { choiceTxt = "turn him away"
                          , cost = Nothing
                          , nextScene = Nothing
                          }
            ]
          }
        scales' = SceneEvent
          { text = [ "the beggar expresses his thanks."
                   , "\n"
                   , "leaves a pile of small scales behind." ]
          , notification = Nothing
          , reward = Just [(Scale, 20)]
          , choices = [ SceneChoice { choiceTxt = "say goodbye"
                                    , cost = Nothing
                                    , nextScene = Nothing
                                    }
                      ]
          }
        teeth' = SceneEvent
          { text = [ "the beggar expresses his thanks."
                   , "\n"
                   , "leaves a pile of small teeth behind." ]
          , notification = Nothing
          , reward = Just [(Teeth, 20)]
          , choices = [ SceneChoice { choiceTxt = "say goodbye"
                                    , cost = Nothing
                                    , nextScene = Nothing
                                    }
                      ]
          }
        cloth' = SceneEvent
          { text = [ "the beggar expresses his thanks."
                   , "\n"
                   , "leaves some scraps of cloth behind."]
          , notification = Nothing
          , reward = Just [(Cloth, 20)]
          , choices = [ SceneChoice { choiceTxt = "say goodbye"
                                    , cost = Nothing
                                    , nextScene = Nothing
                                    }
                      ]
          }

theNomad :: Scene
theNomad = Scene
  { title = "The Nomad"
  , windowSize = 51
  , currentScene = start
  }
  where start = SceneEvent
          { text = [ "a nomad shuffles into view, laden with makeshift"
                   , "bags bound with rough twine."
                   , "\n"
                   , "won't say from where he came, but it's clear that"
                   , "he's not staying."]
          , notification = Just "a nomad arrives, looking to trade"
          -- , blink = True
          , reward = Nothing
          , choices =
            [ SceneChoice { choiceTxt = "buy scales"
                          , cost = Just [(Fur, 100)]
                          , nextScene = Just (Stay Nothing (Scale, 1))
                          }
            , SceneChoice { choiceTxt = "buy teeth"
                          , cost = Just [(Fur, 200)]
                          , nextScene = Just (Stay Nothing (Teeth, 1))
                          }
            , SceneChoice { choiceTxt = "buy bait"
                          , cost = Just [(Fur, 5)]
                          , nextScene = Just (Stay
                                        (Just "traps are more effective with bait.")
                                        (Bait, 1))
                          }
            , SceneChoice { choiceTxt = "buy compass"
                          , cost = Just
                            [(Fur, 300), (Scale, 15), (Teeth, 5), (Compass, 0)]
                          , nextScene =
                              Just (Stay
                              (Just "the old compass is dented and dusty, \
                                    \but it looks to work.")
                              (Compass, 1))
                          }
            , SceneChoice { choiceTxt = "turn him away"
                          , cost = Nothing
                          , nextScene = Nothing
                          }
            ]
          }

noisesOutside :: Scene
noisesOutside = Scene
  { title = "Noises"
  , windowSize = 52
  , currentScene = start
  }
  where start = SceneEvent
          { text = [ "through the walls, shuffling noises can be heard."
                   , "\n"
                   , "can't tell what they're up to."
                   , "\n"
                   , "\n"
                   ]
          , notification = Just "strange noises can be heard through the walls"
          -- , blink = True
          , reward = Nothing
          , choices =
            [ SceneChoice { choiceTxt = "investigate"
                          , cost = Nothing
                          , nextScene = Just $ Go ([(30, stuff)], nothing)
                          }
            , SceneChoice { choiceTxt = "ignore them"
                          , cost = Nothing
                          , nextScene = Nothing
                          }
            ]
          }
        stuff = SceneEvent
          { text = [ "a bundle of sticks lies just beyond the threshold, "
                   , "wrapped in coarse furs."
                   , "\n"
                   , "the night is silent."
                   , "\n"
                   ]
          , notification = Nothing
          , reward = Just [(Wood, 100), (Fur, 10)]
          , choices = [ SceneChoice { choiceTxt = "go back inside"
                                    , cost = Nothing
                                    , nextScene = Nothing
                                    }
                      ]
          }
        nothing = SceneEvent
          { text = [ "vague shapes move, just out of sight."
                   , "\n"
                   , "the sounds stop."
                   , "\n"
                   , "\n"
                   ]
          , notification = Nothing
          , reward = Nothing
          , choices = [ SceneChoice { choiceTxt = "go back inside"
                                    , cost = Nothing
                                    , nextScene = Nothing
                                    }
                      ]
          }
