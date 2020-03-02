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
  { _unlockForest :: (Int, Game -> Game)
  , _fireStoked :: (Int, Game -> Game)
  , _fireShrinking :: (Int, Game -> Game)
  , _builderUpdate :: (Int, Game -> Game)
  }

toList :: GameEvent -> [(Int, Game -> Game )]
toList gameEvent  =
  map ($ gameEvent)
  [ _unlockForest
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
  gameEvent & unlockForest . _1 -~ 1
            & fireStoked . _1 -~ 1
            & fireShrinking . _1 -~ 1
            & builderUpdate . _1 -~ 1

addEvent e es = (e, 0) : es

needWood g = g & uiState . showStores . showWood .~ True
               & stored . wood .~ 4
               & events %~ addEvent "the wood is running out."

updateBuilder g =
  let fstTxt = "a ragged stranger stumbles through the door and collapses in the corner."
      firstTime = _builderLevel g == 0
      g' = if firstTime
           then g & events %~ addEvent fstTxt
                  & upcomingEvents %~ unlockForest .~ (needWoodDelay, needWood)
                  & builderLevel +~ 1
           else g
  in g' & upcomingEvents %~ builderUpdate .~ (builderStateDelay, updateBuilder)

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

unlockForestFn = set (uiState . showOutside) True

fireBurned g = fireChanged $ g & fireValue %~ firePred

initGame :: IO Game
initGame = return $ Game
  { _location = Room
  , _stored = Stored { _wood = 100
                     , _scales = 0
                     }
  , _upcomingEvents = GameEvent { _unlockForest = (-1, unlockForestFn)
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
