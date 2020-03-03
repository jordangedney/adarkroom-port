module Game where

import Control.Lens

import UIState
import GameTypes

-- ticks are 1/100 of a second, so adjust all times
minutes :: Int -> Int
minutes t = 10 * 60 * t
seconds :: Int -> Int
seconds t = 10 * t

fireCoolDelay, roomWarmDelay, builderStateDelay, stokeCooldown, needWoodDelay :: Int
fireCoolDelay     = minutes 5  -- time after a stoke before the fire cools
roomWarmDelay     = seconds 30 -- time between room temperature updates
builderStateDelay = seconds 30 -- time between builder state updates
stokeCooldown     = seconds 10 -- cooldown to stoke the fire
needWoodDelay     = seconds 15 -- from the stranger arrival, to when you need wood

toList :: GameEvents -> [GameEvent]
toList gameEvent  =
  map ($ gameEvent)
  [ _unlockForest
  , _fireStoked
  , _fireShrinking
  , _builderUpdate
  ]

fireState :: FireState -> String
fireState Dead = "the fire is dead."
fireState Smouldering = "the fire is smouldering."
fireState Flickering = "the fire is flickering."
fireState Burning = "the fire is burning."
fireState Roaring = "the fire is roaring."

firePred :: FireState -> FireState
firePred Dead = Dead
firePred x = pred x

fireSucc :: FireState -> FireState
fireSucc Roaring = Roaring
fireSucc x = succ x

getTime :: GameEvent -> Int
getTime  (UnlockForest    x) = x
getTime  (FireStoked      x) = x
getTime  (FireShrinking   x) = x
getTime  (BuilderUpdate   x) = x

isActive :: GameEvent -> Bool
isActive (UnlockForest    x) = x > 0
isActive (FireStoked      x) = x > 0
isActive (FireShrinking   x) = x > 0
isActive (BuilderUpdate   x) = x > 0

eventDec :: GameEvent -> GameEvent
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

addEvent :: String -> [(String, Int)] -> [(String, Int)]
addEvent e es = (e, 0) : es

eventGetter
  :: Functor f
  => GameEvent
  -> ((GameEvent -> f GameEvent) -> GameEvents -> f GameEvents)
eventGetter (UnlockForest  _) = unlockForest
eventGetter (FireStoked    _) = fireStoked
eventGetter (FireShrinking _) = fireShrinking
eventGetter (BuilderUpdate _) = builderUpdate

updateEvents :: GameEvent -> GameEvents -> GameEvents
updateEvents event gameEvents = gameEvents & eventGetter event .~ event

fireChanged :: Game -> Game
fireChanged g =
  let showFire = g & events %~ addEvent (fireState $ _fireValue g)

      fire =
        if _fireValue g == Dead then showFire
        else showFire & upcomingEvents %~ updateEvents (FireShrinking fireCoolDelay)

      fstLight = "the light from the fire spills from the windows, out into the dark."
      firstLightInGame =
        fire & (milestones . fireLit) .~ True
             & events %~ addEvent fstLight
             & upcomingEvents %~ updateEvents (BuilderUpdate builderStateDelay)

  in if (_fireLit . _milestones) g then fire else firstLightInGame

getGameEvent :: GameEvent -> Game -> Game
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
