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

data Tick = Tick deriving (Show, Eq, Ord)

newtype Milestones = Milestones
  { _fireLit :: Bool
  } deriving (Show, Eq, Ord)

data Stored = Stored
  { _wood :: Int
  , _scales :: Int
  } deriving (Show, Eq, Ord)

data GameEvent
  = UnlockForest Int
  | FireStoked Int
  | FireShrinking Int
  | BuilderUpdate Int
  deriving (Show, Eq, Ord)

-- Hacky, but >0 means active, 0 triggers, and <0 means inactive
data GameEvents = GameEvents
  { _unlockForest  :: GameEvent
  , _fireStoked    :: GameEvent
  , _fireShrinking :: GameEvent
  , _builderUpdate :: GameEvent
  } deriving (Show, Eq, Ord)

toList :: GameEvents -> [GameEvent]
toList gameEvent  =
  map ($ gameEvent)
  [ _unlockForest
  , _fireStoked
  , _fireShrinking
  , _builderUpdate
  ]

data Location = Room | Outside | Path | Ship deriving (Eq, Show, Ord)

data Game = Game
  { _location :: Location
  , _stored :: Stored
  , _upcomingEvents :: GameEvents
  , _events :: [(String, Int)]
  , _tickCount :: Int
  , _uiState :: UIState
  , _fireValue :: FireState
  , _temperatureValue :: Int
  , _builderLevel :: Int
  , _progressAmount :: Float
  , _milestones :: Milestones
  } deriving (Eq, Show, Ord)

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
makeLenses ''GameEvents
makeLenses ''Stored
makeLenses ''Game

getTime  (UnlockForest    x) = x
getTime  (FireStoked      x) = x
getTime  (FireShrinking   x) = x
getTime  (BuilderUpdate   x) = x

isActive (UnlockForest    x) = x > 0
isActive (FireStoked      x) = x > 0
isActive (FireShrinking   x) = x > 0
isActive (BuilderUpdate   x) = x > 0

eventDec (UnlockForest    x) = UnlockForest    (x - 1)
eventDec (FireStoked      x) = FireStoked      (x - 1)
eventDec (FireShrinking   x) = FireShrinking   (x - 1)
eventDec (BuilderUpdate   x) = BuilderUpdate   (x - 1)

tick :: GameEvents -> GameEvents
tick gameEvent =
  gameEvent & unlockForest  %~ eventDec
            & fireStoked    %~ eventDec
            & fireShrinking %~ eventDec
            & builderUpdate %~ eventDec

addEvent e es = (e, 0) : es

eventGetter (UnlockForest  _) = unlockForest
eventGetter (FireStoked    _) = fireStoked
eventGetter (FireShrinking _) = fireShrinking
eventGetter (BuilderUpdate _) = builderUpdate

updateEvents event events = events & eventGetter event .~ event

fireChanged g =
  let showFire = g & events %~ addEvent (fireState $ _fireValue g)
      fire = if _fireValue g == Dead then showFire
             else showFire & upcomingEvents %~ updateEvents (FireShrinking fireCoolDelay)

      fstLight = "the light from the fire spills from the windows, out into the dark."
      firstLightInGame =
        fire & (milestones . fireLit) .~ True
             & events %~ addEvent fstLight
             & upcomingEvents %~ updateEvents (BuilderUpdate builderStateDelay)

  in if (_fireLit . _milestones) g then fire else firstLightInGame

getGameEvent (UnlockForest  _) g =
  g & uiState . showStores . showWood .~ True
    & uiState . showOutside .~ True
    & stored . wood .~ 4
    & events %~ addEvent "the wind howls outside."
    & events %~ addEvent "the wood is running out."

getGameEvent (FireStoked    _) g = g

getGameEvent (FireShrinking _) g = fireChanged $ g & fireValue %~ firePred

getGameEvent (BuilderUpdate _) g =
  let fstTxt = "a ragged stranger stumbles through the door and collapses in the corner."
      firstTime = _builderLevel g == 0
      g' = if firstTime
           then g & events %~ addEvent fstTxt
                  & upcomingEvents %~ updateEvents (UnlockForest needWoodDelay)
                  & builderLevel +~ 1
           else g
  in g' & upcomingEvents %~ updateEvents (BuilderUpdate builderStateDelay)

initGame :: IO Game
initGame = return $ Game
  { _location = Room
  , _stored = Stored { _wood = 100
                     , _scales = 0
                     }
  , _upcomingEvents = GameEvents { _unlockForest  = UnlockForest  (-1)
                                 , _fireStoked    = FireStoked    (-1)
                                 , _fireShrinking = FireShrinking (-1)
                                 , _builderUpdate = BuilderUpdate (-1)
                                }
  , _events = [ ("the fire is dead.", 0)
              , ("the room is freezing.", 0)
              ]
  , _tickCount = 0
  , _uiState = UIState { _lastReportedClick = Nothing
                       , _showStores = showStoresInit
                       , _showOutside = False
                       , _showPath = False
                       , _showShip = False
                       }
  , _fireValue = Dead
  , _temperatureValue = 0
  , _builderLevel = 0
  , _progressAmount = 0.5
  , _milestones = Milestones {_fireLit = False}
  }
