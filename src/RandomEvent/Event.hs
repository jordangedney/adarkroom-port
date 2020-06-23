module RandomEvent.Event where

import RandomEvent.EventType (RandomEvent(..), RandomEventChoice(..))
import GameTypes (Item(..))

getEvent :: RandomEvent -> Scene
getEvent TheBeggar = theBeggar

data Scene = Scene
  { title :: String
  -- , isAvailable :: Game -> Bool
  , eventType :: RandomEvent
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
  -- Brick requires that every UI component have a unique type association.
  , uiID :: RandomEventChoice
  , nextScene :: Maybe [(Float, SceneEvent)]
  }


theBeggar :: Scene
theBeggar = Scene
  { title = "The Beggar"
  -- , isAvailable = (\g -> view location g == Room && view (stored . fur) g > 0)
  , eventType = TheBeggar
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
                          , uiID = FurBeggarFifty
                          , nextScene = Just [
                              (0.5, scales'), (0.8, teeth'), (1.0, cloth')]
                          }

            , SceneChoice { txt = "give 100"
                          , cost = Just (Fur, 100)
                          , uiID = FurBeggarHundred
                          , nextScene = Just [
                              (0.5, teeth'), (0.8, scales'), (1.0, cloth')]
                          }

            , SceneChoice { txt = "turn him away"
                          , cost = Nothing
                          , uiID = End
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
                                    , uiID = End
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
                                    , uiID = End
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
                                    , uiID = End
                                    , nextScene = Nothing
                                    }
                      ]
          }
