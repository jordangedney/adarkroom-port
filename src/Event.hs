module Event (handleEventWrapper) where

import Control.Monad.IO.Class (liftIO)
import Brick (BrickEvent(..), EventM, Next, Location, continue)
import Control.Lens (over, set, view, _2, (&))

import Game (getGameEvent)
import GameTypes (Game, Tick(..), tickCount, upcomingEvents, events, uiState,
                  debug, hyper, initGame, previousStates, paused)
import GameEvent (tickEvents, getTime, toList)
import UIState (Name(..), lastReportedClick)
import SaveGame (save)
import qualified Fire


gameTick :: Game -> Game
gameTick game =
  let updatedTickers =
        game & over tickCount (+1)
             & over upcomingEvents tickEvents
             & over events (take 15 . map (over _2 (+1)))
      doEventIfReady e = if getTime e == 0 then getGameEvent e else id
      allEvents = toList (view upcomingEvents updatedTickers)
      withStateAfterIngameEvents = foldr doEventIfReady updatedTickers allEvents
  in if view paused game then game else withStateAfterIngameEvents

handleEventWrapper :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEventWrapper game event@(AppEvent Tick) = handleEvent game event
handleEventWrapper game event@(MouseDown PrevButton _ _ _) = handleEvent game event
handleEventWrapper game event@(MouseUp PrevButton _ _) = handleEvent game event
handleEventWrapper game event =
  handleEvent (game & over  previousStates (game:)) event

handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEvent game (AppEvent Tick) =
  -- XXX the gui ticks twice at once
  let doubleSpeedEnabled = view hyper game
      newGame = if doubleSpeedEnabled then gameTick (gameTick game)
                else gameTick game
  in continue newGame

handleEvent g (MouseDown SaveButton _ _ m) = do
  liftIO (save g)
  handleMouseDown g SaveButton m

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
handleButtonEvent RestartButton = const initGame
handleButtonEvent HyperButton = over hyper not
handleButtonEvent DebugButton = over debug not
handleButtonEvent PrevButton = set paused True . head . view previousStates
handleButtonEvent PauseButton = over paused not
handleButtonEvent _ = id
