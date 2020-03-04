module Game where

import Control.Lens

import UIState
import GameTypes
import GameEvent
import Constants
import Util

import qualified Fire

getGameEvent :: GameEvent -> Game -> Game
getGameEvent (UnlockForest  _) g =
  g & uiState . showStores . showWood .~ True
    & uiState . showOutside .~ True
    & stored . wood .~ 4
    & events %~ addEvent "the wind howls outside."
    & events %~ addEvent "the wood is running out."

getGameEvent (FireStoked    _) g = g

getGameEvent (FireShrinking _) g = Fire.shrinking g

getGameEvent (BuilderUpdate _) g =
  let fstTxt = "a ragged stranger stumbles through the door and collapses in the corner."
      firstTime = _builderLevel g == 0
      g' = if firstTime
           then g & events %~ addEvent fstTxt
                  & upcomingEvents %~ updateEvents (UnlockForest needWoodDelay)
                  & builderLevel +~ 1
           else g
  in g' & upcomingEvents %~ updateEvents (BuilderUpdate builderStateDelay)
