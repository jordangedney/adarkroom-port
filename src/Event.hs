module Event (handleEvent) where

import           Brick
import qualified Brick.Main as M
import qualified Brick.Widgets.Edit as E
import qualified Graphics.Vty as V

import           Game
import           UIState

updateLastClicked g n loc = g { uiState = (uiState g) { lastReportedClick = Just (n , loc)}}

handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEvent g (AppEvent Tick) =
  let tick = g { tickCount = tickCount g + 1
               , upcomingEvents = map (\(a, b, c) -> (a - 1, b, c)) $ upcomingEvents g
               }
      finishedEventTodos = [toDo | (time, event, toDo) <- upcomingEvents tick, time == 0]
      unfinishedEvents = [e | e@(time, event, toDo) <- upcomingEvents tick, time /= 0]
      withoutFinishedEvents = tick { upcomingEvents = unfinishedEvents }
      doEvents [] game = game
      doEvents (e:es) game = doEvents es $ e game
  in continue $ doEvents finishedEventTodos withoutFinishedEvents

handleEvent g (MouseDown LightButton _ _ loc) =
  let lightFire = (updateLastClicked g LightButton loc)
                  { fireValue = 1
                  , events = "the fire is burning." : events g
                  , upcomingEvents = (100, FireStoked, id) : upcomingEvents g
                  }
      fstLight = "the light from the fire spills from the windows, out into the dark."
      firstLightInGame = lightFire { milestones = (milestones lightFire) {fireLit = True}
                                   , events = fstLight : events lightFire
                                   }
  in continue $ if (fireLit . milestones) g then lightFire else firstLightInGame

handleEvent g (MouseDown StokeButton _ _ loc) =
  let lightFire = (updateLastClicked g StokeButton loc)
                  { fireValue = 1
                  , events = "the fire is burning." : events g
                  , upcomingEvents = (100, FireStoked, id) : upcomingEvents g
                  }
      fstLight = "the light from the fire spills from the windows, out into the dark."
      firstLightInGame = lightFire {builderLevel = 0, events = fstLight : events lightFire}

  in continue $ if builderLevel g == -1 then firstLightInGame else lightFire

handleEvent g (MouseDown n _ _ loc) = continue $ updateLastClicked g n loc
handleEvent g MouseUp {} =
  continue $ g { uiState = (uiState g) { lastReportedClick = Nothing }}

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
