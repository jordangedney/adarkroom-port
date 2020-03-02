{-# LANGUAGE TemplateHaskell #-}

module Game where

-- import Control.Lens hiding (element)
import Control.Lens

import UIState

-- ticks are 1/100 of a second, so adjust all times
minutes :: Int -> Int
minutes t = 10 * 60 * t
seconds :: Int -> Int
seconds t = 10 * t

fireCoolDelay     = minutes 5  -- time after a stoke before the fire cools
roomWarmDelay     = seconds 30 -- time between room temperature updates
builderStateDelay = seconds 30 -- time between builder state updates
stokeCooldown     = seconds 10 -- cooldown to stoke the fire
needWoodDelay     = seconds 15 -- from the stranger arrival, to when you need wood

data Tick = Tick

data Milestones = Milestones
  { _fireLit :: Bool }

data Stored = Stored
  { _wood :: Int
  , _scales :: Int
  } deriving (Show, Eq, Ord)

-- Hacky, but >0 means active, 0 triggers, and <0 means inactive
data GameEvent = GameEvent
  { _allowedOutside :: (Int, Game -> Game)
  , _fireStoked :: (Int, Game -> Game)
  , _fireShrinking :: (Int, Game -> Game)
  , _builderUpdate :: (Int, Game -> Game)
  }

toList :: GameEvent -> [(Int, Game -> Game )]
toList gameEvent  =
  map ($ gameEvent)
  [ _allowedOutside
  , _fireStoked
  , _fireShrinking
  , _builderUpdate
  ]

isTriggered :: (Int, Game -> Game) -> Bool
isTriggered (time, _) = time == 0

isActive :: (Int, Game -> Game) -> Bool
isActive (time, _) = time > 0

data Location = Room | Outside

data Game = Game
  { _location :: Location
  , _stored :: Stored
  -- , _upcomingEvents :: [(Int, GameEvent, Game -> Game)]
  , _upcomingEvents :: GameEvent
  , _events :: [(String, Int)]
  , _tickCount :: Int
  , _uiState :: UIState
  , _fireValue :: FireState
  , _temperatureValue :: Int
  , _builderLevel :: Int
  , _progressAmount :: Float
  , _milestones :: Milestones
  }

data FireState
  = Dead
  | Smouldering
  | Flickering
  | Burning
  | Roaring
  deriving (Eq, Show, Enum, Ord)

fireState Dead = "the fire is dead."
fireState Smouldering = "the fire is smouldering."
fireState Flickering = "the fire is flickering."
fireState Burning = "the fire is burning."
fireState Roaring = "the fire is roaring."

firePred Dead = Dead
firePred x = pred x
fireSucc Roaring = Roaring
fireSucc x = succ x

makeLenses ''Milestones
makeLenses ''GameEvent
makeLenses ''Stored
makeLenses ''Game

tick :: GameEvent -> GameEvent
tick gameEvent =
  gameEvent & allowedOutside . _1 -~ 1
            & fireStoked . _1 -~ 1
            & fireShrinking . _1 -~ 1
            & builderUpdate . _1 -~ 1

addEvent e es = (e, 0) : es

updateBuilder g =
  let first = "a ragged stranger stumbles through the door and collapses in the corner."
  in g & events %~ addEvent first

fireChanged g =
  let showFire = g & events %~ addEvent (fireState $ _fireValue g)
      fire = if _fireValue g == Dead then showFire
             else showFire & upcomingEvents . fireShrinking .~ (fireCoolDelay, fireBurned)

      fstLight = "the light from the fire spills from the windows, out into the dark."
      firstLightInGame =
        fire & (milestones . fireLit) .~ True
             & events %~ addEvent fstLight
             & upcomingEvents %~ builderUpdate .~ (builderStateDelay, updateBuilder)

  in if (_fireLit . _milestones) g then fire else firstLightInGame


-- handleGameEvents :: GameEvent -> Game -> Game
-- handleGameEvents AllowedOutside = set (uiState . showOutside) True

allowedOutsideFn = set (uiState . showOutside) True

fireBurned g = fireChanged $ g & fireValue %~ firePred

initGame :: IO Game
initGame = return $ Game
  { _location = Room
  , _stored = Stored { _wood = 10
                     , _scales = 150
                     }
  , _upcomingEvents = GameEvent { _allowedOutside = (-1, allowedOutsideFn)
                                , _fireStoked = (-1, id)
                                , _fireShrinking = (-1, fireBurned)
                                , _builderUpdate = (-1, updateBuilder)
                                }
  , _events = [ ("the fire is dead.", 0)
              , ("the room is freezing.", 0)
              ]
  , _tickCount = 0
  , _uiState = UIState { _lastReportedClick = Nothing
                       , _showStores = showStoresInit
                       , _showOutside = False
                       }
  , _fireValue = Dead
  , _temperatureValue = 0
  , _builderLevel = 0
  , _progressAmount = 0.5
  , _milestones = Milestones {_fireLit = False}
  }
