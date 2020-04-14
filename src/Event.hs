module Event (handleEventWrapper) where

import Control.Monad.IO.Class (liftIO)
import Brick (BrickEvent(..), EventM, Next, Location, continue)
import Control.Lens (over, set, view, _2, (&))

import Game (getGameEvent)
import GameTypes (Game, Tick(..), Location(..), tickCount, upcomingEvents, events, uiState,
                  debug, hyper, initGame, previousStates, paused, location)
import GameEvent (tickEvents, toList)
import UIState (Name(..), lastReportedClick)
import SaveGame (save)
import qualified Fire
import qualified Outside
import qualified Room
import qualified Builder

handleEventWrapper :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEventWrapper game event =
  let saveGame = game & over previousStates (game:)
      stepButDontSave = continue $ handleEvent game event
      saveAndStep     = continue $ handleEvent saveGame event
  in case event of
    -- Don't autosave
    (AppEvent Tick)              -> stepButDontSave
    (MouseDown PrevButton _ _ _) -> stepButDontSave
    (MouseUp PrevButton _ _)     -> stepButDontSave

    -- Events which use IO:
    (MouseDown SaveButton _ _ buttonLocation) -> do
      liftIO (save game)
      continue (handleMouseDown game SaveButton buttonLocation)

    _                            -> saveAndStep

gameTick :: Game -> Game
gameTick game =
  let updatedTickers =
        game & over tickCount (+1)
             & over upcomingEvents tickEvents
             & over events (take 15 . map (over _2 (+1)))
      doEventIfReady e = if snd e == 0 then getGameEvent e else id
      allEvents = toList (view upcomingEvents updatedTickers)
      withStateAfterIngameEvents = foldr doEventIfReady updatedTickers allEvents
  in if view paused game then game else withStateAfterIngameEvents

handleEvent :: Game -> BrickEvent Name Tick -> Game
handleEvent game (AppEvent Tick) =
  -- XXX the gui ticks twice at once
  let doubleSpeedEnabled = view hyper game
      fasterFaster = gameTick . gameTick . gameTick . gameTick $ game
  in if doubleSpeedEnabled then fasterFaster else gameTick game

handleEvent g (MouseDown e _ _ m) = handleMouseDown g e m
handleEvent g MouseUp {} = set (uiState . lastReportedClick) Nothing g
handleEvent g _ = g

handleMouseDown :: Game -> Name -> Brick.Location -> Game
handleMouseDown game buttonPressed mouseLocation =
  game & set (uiState . lastReportedClick) (Just (buttonPressed, mouseLocation))
  & handleButtonEvent buttonPressed

handleButtonEvent :: Name -> Game -> Game
handleButtonEvent NoOpButton = id

handleButtonEvent RoomButton = Room.arrival
handleButtonEvent LightButton = Fire.light
handleButtonEvent StokeButton = Fire.stoke

handleButtonEvent OutsideButton      = Outside.arrival
handleButtonEvent GatherButton       = Outside.gather
handleButtonEvent CheckTrapsButton   = Outside.checkTraps

handleButtonEvent TrapButton  = Builder.buildTrap
handleButtonEvent CartButton  = Builder.buildCart

handleButtonEvent PathButton = set location Path
handleButtonEvent ShipButton = set location Ship

handleButtonEvent RestartButton = const initGame
handleButtonEvent HyperButton = over hyper not
handleButtonEvent DebugButton = over debug not
handleButtonEvent PrevButton = set paused True . head . view previousStates
handleButtonEvent PauseButton = over paused not

handleButtonEvent _ = id
