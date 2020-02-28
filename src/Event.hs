module Event (handleEvent) where

import           Brick
import qualified Brick.Main as M
import qualified Brick.Widgets.Edit as E
import qualified Graphics.Vty as V

import           Game
import           UIState

handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)

handleEvent g (AppEvent Tick) =
  continue g {_tickCount = _tickCount g + 1}

handleEvent g (MouseDown n _ _ loc) =
  continue $ g { _uiState = (_uiState g) { _lastReportedClick = Just (n, loc)}}
handleEvent g MouseUp {} =
  continue $ g { _uiState = (_uiState g) { _lastReportedClick = Nothing }}

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

handleEvent g _ = continue  g
