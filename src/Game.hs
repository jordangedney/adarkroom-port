module Game where

import Control.Lens

import UIState
import GameTypes
import GameEvent
import Fire
import Constants

addEvent :: String -> [(String, Int)] -> [(String, Int)]
addEvent e es = (e, 0) : es

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
