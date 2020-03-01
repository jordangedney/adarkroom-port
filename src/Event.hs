module Event (handleEvent) where

import           Brick
import qualified Brick.Main as M
import qualified Brick.Widgets.Edit as E
import qualified Graphics.Vty as V
import           Control.Lens

import           Game
import           UIState

handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEvent g (AppEvent Tick) =
  let tickGame = g & tickCount %~ (+1)
               & upcomingEvents %~ tick
      finishedEventTodos = [toDo | (time, toDo) <- toList $ _upcomingEvents tickGame, time == 0]
  in continue $ foldr ($) tickGame finishedEventTodos

handleEvent g (MouseDown LightButton _ _ loc) =
  let lightFire = g & (uiState . lastReportedClick) ?~ (LightButton, loc)
                    & fireValue .~ Burning
                    & stored . wood %~ (+ (-5))
                    & upcomingEvents . fireStoked .~ (100, id)
      fstLight = "the light from the fire spills from the windows, out into the dark."
      firstLightInGame = lightFire
                         & (milestones . fireLit) .~ True
                         & events %~ (fstLight:)
  in continue $ if (_fireLit . _milestones) g then lightFire else firstLightInGame

handleEvent g (MouseDown StokeButton _ _ loc) =
  continue $ g & (uiState . lastReportedClick) ?~ (StokeButton, loc)
               & fireValue %~ fireSucc
               & stored . wood %~ (+ (-1))
               & upcomingEvents . fireStoked .~ (100, id)

handleEvent g (MouseDown n _ _ loc) = continue $ g & (uiState . lastReportedClick) ?~ (n, loc)
handleEvent g MouseUp {} = continue $ set (uiState . lastReportedClick) Nothing g

-- handleEvent g (VtyEvent (V.EvKey V.KUp []))         = continue g
-- handleEvent g (VtyEvent (V.EvKey V.KDown []))       = continue g
-- handleEvent g (VtyEvent (V.EvKey V.KRight []))      = continue g
-- handleEvent g (VtyEvent (V.EvKey V.KLeft []))       = continue g
-- handleEvent g (VtyEvent (V.EvKey (V.KChar 'k') [])) = continue g
-- handleEvent g (VtyEvent (V.EvKey (V.KChar 'j') [])) = continue g
-- handleEvent g (VtyEvent (V.EvKey (V.KChar 'l') [])) = continue g
-- handleEvent g (VtyEvent (V.EvKey (V.KChar 'h') [])) = continue g
-- handleEvent g (VtyEvent (V.EvKey (V.KChar 'r') [])) = continue g
-- handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = continue g
-- handleEvent g (VtyEvent (V.EvKey V.KEsc []))        = continue g

handleEvent g _ = continue g
