module Room.Event where

import Control.Lens (view)

import Shared.Game
import Shared.RandomEvent
import Shared.Item

events :: Game -> [(Scene, Bool)]
events g =
  [ (theBeggar,        view location g == Room && view (stored . fur) g > 0)
  , (theNomad,         view location g == Room && view (stored . fur) g > 0)
  , (noisesOutside,    view location g == Room && view (stored . wood) g > 15)
  , (noisesInside,     view location g == Room && view (stored . wood) g > 15)
  , (theShadyBuilder,  view location g == Room && view (stored . huts) g > 4
                                              && view (stored . huts) g < 20)
  ]

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
          , reward = None
          , choices =
            [ SceneChoice { choiceTxt = "give 50"
                          , cost = [(Fur, 50)]
                          , nextScene = Just $ Go ([
                              (50, scales'), (30, teeth')], cloth')
                          }

            , SceneChoice { choiceTxt = "give 100"
                          , cost = [(Fur, 100)]
                          , nextScene = Just $ Go ([
                              (50, teeth'), (30, scales')],  cloth')
                          }

            , SceneChoice { choiceTxt = "turn him away"
                          , cost = []
                          , nextScene = Nothing
                          }
            ]
          }
        scales' = SceneEvent
          { text = [ "the beggar expresses his thanks."
                   , "\n"
                   , "leaves a pile of small scales behind." ]
          , notification = Nothing
          , reward = Give [(Scale, 20)]
          , choices = [ SceneChoice { choiceTxt = "say goodbye"
                                    , cost = []
                                    , nextScene = Nothing
                                    }
                      ]
          }
        teeth' = SceneEvent
          { text = [ "the beggar expresses his thanks."
                   , "\n"
                   , "leaves a pile of small teeth behind." ]
          , notification = Nothing
          , reward = Give [(Teeth, 20)]
          , choices = [ SceneChoice { choiceTxt = "say goodbye"
                                    , cost = []
                                    , nextScene = Nothing
                                    }
                      ]
          }
        cloth' = SceneEvent
          { text = [ "the beggar expresses his thanks."
                   , "\n"
                   , "leaves some scraps of cloth behind."]
          , notification = Nothing
          , reward = Give [(Cloth, 20)]
          , choices = [ SceneChoice { choiceTxt = "say goodbye"
                                    , cost = []
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
          , reward = None
          , choices =
            [ SceneChoice { choiceTxt = "buy scales"
                          , cost = [(Fur, 100)]
                          , nextScene = Just (Stay Nothing (Scale, 1))
                          }
            , SceneChoice { choiceTxt = "buy teeth"
                          , cost = [(Fur, 200)]
                          , nextScene = Just (Stay Nothing (Teeth, 1))
                          }
            , SceneChoice { choiceTxt = "buy bait"
                          , cost = [(Fur, 5)]
                          , nextScene = Just (Stay
                                        (Just "traps are more effective with bait.")
                                        (Bait, 1))
                          }
            , SceneChoice { choiceTxt = "buy compass"
                          , cost =
                            [(Fur, 300), (Scale, 15), (Teeth, 5), (Compass, 0)]
                          , nextScene =
                              Just (Stay
                              (Just "the old compass is dented and dusty, \
                                    \but it looks to work.")
                              (Compass, 1))
                          }
            , SceneChoice { choiceTxt = "turn him away"
                          , cost = []
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
          , reward = None
          , choices =
            [ SceneChoice { choiceTxt = "investigate"
                          , cost = []
                          , nextScene = Just $ Go ([(30, stuff)], nothing)
                          }
            , SceneChoice { choiceTxt = "ignore them"
                          , cost = []
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
          , reward = Give [(Wood, 100), (Fur, 10)]
          , choices = [ SceneChoice { choiceTxt = "go back inside"
                                    , cost = []
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
          , reward = None
          , choices = [ SceneChoice { choiceTxt = "go back inside"
                                    , cost = []
                                    , nextScene = Nothing
                                    }
                      ]
          }


noisesInside :: Scene
noisesInside = Scene
  { title = "Noises"
  , windowSize = 52
  , currentScene = start
  }
  where start = SceneEvent
          { text = ["scratching noises can be heard from the store room."
                   , "\n"
                   , "something's in there."
                   , "\n"
                   , "\n"
                   ]
          , notification = Just "something's in the store room"
          -- , blink = True
          , reward = None
          , choices =
            [ SceneChoice { choiceTxt = "investigate"
                          , cost = []
                          , nextScene = Just $ Go ([
                              (50, scales'), (30, teeth')], cloth')
                          }
            , SceneChoice { choiceTxt = "ignore them"
                          , cost = []
                          , nextScene = Nothing
                          }
            ]
          }
        scales' = SceneEvent
          { text = [ "some wood is missing."
                   , "\n"
                   , "the ground is littered with small scales"
                   , "\n"
                   ]
          , notification = Nothing
          , reward = GiveSome [(Wood, 10, Scale, 20)]
          , choices = [ SceneChoice { choiceTxt = "go back inside"
                                    , cost = []
                                    , nextScene = Nothing
                                    }
                      ]
          }
        teeth' = SceneEvent
          { text = [ "some wood is missing."
                   , "\n"
                   , "the ground is littered with small teeth"
                   , "\n"
                   ]
          , notification = Nothing
          , reward = GiveSome [(Wood, 10, Teeth, 20)]
          , choices = [ SceneChoice { choiceTxt = "go back inside"
                                    , cost = []
                                    , nextScene = Nothing
                                    }
                      ]
          }
        cloth' = SceneEvent
          { text = [ "some wood is missing."
                   , "\n"
                   , "the ground is littered with small cloth"
                   , "\n"
                   ]
          , notification = Nothing
          , reward = GiveSome [(Wood, 10, Cloth, 20)]
          , choices = [ SceneChoice { choiceTxt = "go back inside"
                                    , cost = []
                                    , nextScene = Nothing
                                    }
                      ]
          }

theShadyBuilder :: Scene
theShadyBuilder = Scene
  { title = "The Shady Builder"
  , windowSize = 52
  , currentScene = start
  }
  where start = SceneEvent
          { text = [ "a shady builder passes through."
                   , "\n"
                   , "says he can build you a hut for less wood."
                   ]
          , notification = Just "a shady builder passes through"
          -- , blink = True
          , reward = None
          , choices =
            [ SceneChoice { choiceTxt = "300 wood"
                          , cost = [(Wood, 300)]
                          , nextScene = Just $ Go ([
                              (60, steal)], build)
                          }
            , SceneChoice { choiceTxt = "say goodbye"
                          , cost = []
                          , nextScene = Nothing
                          }
            ]
          }
        steal = SceneEvent
          { text = ["the shady builder has made off with your wood."
                   , "\n"
                   ]
          , notification = Just "the shady builder has made off with your wood"
          , reward = None
          , choices = [ SceneChoice { choiceTxt = "go home"
                                    , cost = []
                                    , nextScene = Nothing
                                    }
                      ]
          }
        build = SceneEvent
          { text = [ "the shady builder builds a hut."
                   , "\n"
                   ]
          , notification = Nothing
          , reward = Give [(Hut, 1)]
          , choices = [ SceneChoice { choiceTxt = "go home"
                                    , cost = []
                                    , nextScene = Nothing
                                    }
                      ]
          }
