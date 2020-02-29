module Event (handleEvent) where

import           Brick
import qualified Brick.Main as M
import qualified Brick.Widgets.Edit as E
import qualified Graphics.Vty as V

import           Game
import           UIState

updateLastClicked g n loc = g { uiState = (uiState g) { _lastReportedClick = Just (n , loc)}}

handleEvent :: Game -> BrickEvent Name GameEvent -> EventM Name (Next Game)
handleEvent g (AppEvent Tick) =
  continue g {tickCount = tickCount g + 1}

handleEvent g (AppEvent UnlockOutside) =
  continue g {tickCount = tickCount g + 1}

handleEvent g (MouseDown LightButton _ _ loc) =
  let lightFire = (updateLastClicked g LightButton loc)
                  { fireValue = 1
                  , events = "the fire is burning." : (events g)}
      fstLight = "the light from the fire spills from the windows, out into the dark."
      firstLightInGame = lightFire {builderLevel = 0, events = fstLight : events lightFire}
  in continue $ if (builderLevel g) == -1 then firstLightInGame else lightFire

handleEvent g (MouseDown n _ _ loc) = continue $ updateLastClicked g n loc
handleEvent g MouseUp {} =
  continue $ g { uiState = (uiState g) { _lastReportedClick = Nothing }}

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
