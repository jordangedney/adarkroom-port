module Event (handleEvent) where

import           Brick
import qualified Brick.Main as M
import qualified Brick.Widgets.Edit as E
import qualified Graphics.Vty as V
import Control.Lens (makeLenses, (%~), (.~), set, (&))

import           Game
import           UIState

updateLastClicked g n loc = g & (uiState . lastReportedClick) .~ Just (n, loc)

handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEvent g (AppEvent Tick) =
  let tick = g & tickCount %~ (+1)
               & upcomingEvents %~ map (\(a, b, c) -> (a - 1, b, c))
      finishedEventTodos = [toDo | (time, event, toDo) <- _upcomingEvents tick, time == 0]
      unfinishedEvents = [e | e@(time, event, toDo) <- _upcomingEvents tick, time /= 0]
      withoutFinishedEvents = tick { _upcomingEvents = unfinishedEvents }
      doEvents [] game = game
      doEvents (e:es) game = doEvents es $ e game
  in continue $ doEvents finishedEventTodos withoutFinishedEvents

handleEvent g (MouseDown LightButton _ _ loc) =
  let lightFire = (updateLastClicked g StokeButton loc)
                  & fireValue .~ 1
                  & events %~ ("the fire is burning.":)
                  & upcomingEvents %~ ((100, FireStoked, id):)
      fstLight = "the light from the fire spills from the windows, out into the dark."
      firstLightInGame = lightFire
                         & (milestones . fireLit) .~ True
                         & events %~ (fstLight:)
  in continue $ if (_fireLit . _milestones) g then lightFire else firstLightInGame

handleEvent g (MouseDown StokeButton _ _ loc) =
  -- let lightFire = (updateLastClicked g StokeButton loc)
  --                 { fireValue = 1
  --                 , events = "the fire is burning." : events g
  --                 , upcomingEvents = (100, FireStoked, id) : upcomingEvents g
  --                 }
  let lightFire = (updateLastClicked g StokeButton loc)
                  & fireValue .~ 1
                  & upcomingEvents %~ ((100, FireStoked, id):)
  in continue $ lightFire

handleEvent g (MouseDown n _ _ loc) = continue $ updateLastClicked g n loc
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

handleEvent g _ = continue  g
