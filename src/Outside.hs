module Outside
  ( unlock
  )
where

import Control.Lens (over, set, (&))

import UIState (showStores, showWood, showOutside)
import GameTypes (Game, events, stored, wood, uiState)
import Util (addEvent)

unlock :: Game -> Game
unlock game =
  game & set (uiState . showStores . showWood) True
       & set (uiState . showOutside) True
       & set (stored . wood) 4
       & over events (addEvent "the wind howls outside.")
       & over events (addEvent "the wood is running out.")
