module Event where

import           Brick
import qualified Graphics.Vty as V

import           Game
import           UI

handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEvent g (AppEvent Tick)                       = continue g {_tickCount = _tickCount g + 1}
handleEvent g (VtyEvent (V.EvKey V.KUp []))         = continue g
handleEvent g (VtyEvent (V.EvKey V.KDown []))       = continue g
handleEvent g (VtyEvent (V.EvKey V.KRight []))      = continue g
handleEvent g (VtyEvent (V.EvKey V.KLeft []))       = continue g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'k') [])) = continue g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'j') [])) = continue g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'l') [])) = continue g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'h') [])) = continue g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'r') [])) = continue g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = continue g
handleEvent g (VtyEvent (V.EvKey V.KEsc []))        = continue g
handleEvent g _                                     = continue g
