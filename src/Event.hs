module Event (handleEvent) where

import           Brick
import           Control.Lens

import           Game
import           GameTypes
import           GameEvent
import           UIState
import           Fire
import           Constants

handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEvent g (AppEvent Tick) =
  let updatedTickers = g & over tickCount (+1)
                         & over upcomingEvents tickEvents
                         & over events (take 15 . map (\(a, b) -> (a, b + 1)))
                        -- & over events (take 15 . map (over _2 (+1)))

      doEventIfReady e = if getTime e == 0 then getGameEvent e else id
      allEvents = toList (_upcomingEvents updatedTickers)
      withStateAfterIngameEvents = foldr doEventIfReady updatedTickers allEvents
  in continue withStateAfterIngameEvents

handleEvent g' (MouseDown LightButton _ _ mouseLocation) =
  let g = g' & (uiState . lastReportedClick) ?~ (LightButton, mouseLocation)
      lightFire = g & set fireValue Burning
                    & stored . wood -~ 5
                    & upcomingEvents %~ updateEvents (FireStoked stokeCooldown)
  in continue $
     if (_wood . _stored $ g ) > 4 then fireChanged lightFire
     else g & events %~ addEvent "not enough wood to get the fire going."

handleEvent g' (MouseDown StokeButton _ _ mouseLocation) =
  let g = g' & (uiState . lastReportedClick) ?~ (StokeButton, mouseLocation)
  in continue $
     if (_wood . _stored $ g) > 0
     then fireChanged $ g & fireValue %~ fireSucc
                          & stored . wood -~ 1
                          & upcomingEvents %~ updateEvents (FireStoked stokeCooldown)
     else g & events %~ addEvent "the wood has run out."

handleEvent g (MouseDown n _ _ mouseLocation) =
  continue $ g & (uiState . lastReportedClick) ?~ (n, mouseLocation)
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
