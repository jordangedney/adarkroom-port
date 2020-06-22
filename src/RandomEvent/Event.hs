module RandomEvent.Event where

import RandomEvent.EventType (RandomEvent(..))
import GameTypes (Item(..))

getEvent :: RandomEvent -> Scene
getEvent BeggarEvent = theBeggar

data Scene = Scene
  { title :: String
  -- , isAvailable :: Game -> Bool
  , startingScene :: SceneEvent
  }

data SceneEvent = SceneEvent
  { text :: [String]
  , notification :: Maybe String
--   , blink :: Bool
  , reward :: Maybe (Item, Int)
  , buttons :: [SceneChoice]
  }

data SceneChoice = SceneChoice
  { txt :: String
  , cost :: Maybe (Item, Int)
  , nextScene :: Maybe [(Float, SceneEvent)]
  }


theBeggar :: Scene
theBeggar = Scene
  { title = "The Beggar"
  -- , isAvailable = (\g -> view location g == Room && view (stored . fur) g > 0)
  , startingScene = start
  }
  where start = SceneEvent
          { text = [ "a beggar arrives."
                   , "asks for any spare furs to keep him warm at night."]
          , notification = Just "a beggar arrives"
          -- , blink = True
          , reward = Nothing
          , buttons =
            [ SceneChoice { txt = "give 50"
                          , cost = Just (Fur, 50)
                          , nextScene = Just [
                              (0.5, scales'), (0.8, teeth'), (1.0, cloth')]
                          }

            , SceneChoice { txt = "give 100"
                          , cost = Just (Fur, 100)
                          , nextScene = Just [
                              (0.5, teeth'), (0.8, scales'), (1.0, cloth')]
                          }

            , SceneChoice { txt = "turn him away"
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
          , buttons = [ SceneChoice { txt = "say goodbye"
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
          , buttons = [ SceneChoice { txt = "say goodbye"
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
          , buttons = [ SceneChoice { txt = "say goodbye"
                                    , cost = Nothing
                                    , nextScene = Nothing
                                    }
                      ]
          }
