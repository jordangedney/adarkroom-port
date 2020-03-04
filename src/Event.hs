module Event (handleEvent) where

import           Brick (BrickEvent(..), EventM, Next, Location, continue)
import           Control.Lens (over, set, _2, (&))

import           Game
import           GameTypes
import           GameEvent
import           UIState
import qualified Fire

handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEvent game (AppEvent Tick) =
  let updatedTickers =
        game & over tickCount (+1)
             & over upcomingEvents tickEvents
             & over events (take 15 . map (over _2 (+1)))
      doEventIfReady e = if getTime e == 0 then getGameEvent e else id
      allEvents = toList (_upcomingEvents updatedTickers)
      withStateAfterIngameEvents = foldr doEventIfReady updatedTickers allEvents
  in continue withStateAfterIngameEvents
handleEvent g (MouseDown e _ _ m) = handleMouseDown g e m
handleEvent g MouseUp {} = continue $ set (uiState . lastReportedClick) Nothing g
handleEvent g _ = continue g

handleMouseDown :: Game -> Name -> Brick.Location -> EventM n (Next Game)
handleMouseDown game buttonPressed mouseLocation =
  game & set (uiState . lastReportedClick) (Just (buttonPressed, mouseLocation))
  & handleButtonEvent buttonPressed
  & continue

handleButtonEvent :: Name -> Game -> Game
handleButtonEvent LightButton = Fire.light
handleButtonEvent StokeButton = Fire.stoke
handleButtonEvent _ = id
