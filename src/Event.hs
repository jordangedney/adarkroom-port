module Event (handleEventWrapper) where

import System.Random (newStdGen)
import Control.Monad.IO.Class (liftIO)
import Brick (BrickEvent(..), EventM, Next, Location, continue)
import Control.Lens (over, set, view, _2, (&))

import Game (getGameEvent)
import GameTypes (Game, Tick(..), Location(..), tickCount, upcomingEvents, events, uiState, inEvent,
                  debug, hyper, initGame, previousStates, paused, location, inEvent,
                  stored, bait, fur, meat, scales, teeth, cloth, charm)
import GameEvent (tickEvents, toList)
import UI.State (Name(..), lastReportedClick)
import SaveGame (save)
import qualified Fire
import qualified Outside
import qualified Room
import qualified Builder
import qualified RandomEvent.Handler as RandomEvent
import qualified RandomEvent.Event as RandomEvent

handleEventWrapper :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEventWrapper game event =
  let step g = continue $ handleEvent g event
      autosave g = g & over previousStates (g:)
  in case event of
    -- Don't autosave:
    (MouseDown PrevButton _ _ _) -> step game
    (MouseUp PrevButton _ _)     -> step game

    -- Events which use IO:
    (AppEvent Tick)              ->
      if RandomEvent.shouldDoRandomEvent game
      then do
        random <- liftIO newStdGen
        step (RandomEvent.doRandomEvent random game)
      else step game

    (MouseDown SaveButton _ _ _) -> do
      liftIO (save game)
      step game

    (MouseDown (RandomEventButton x) _ _ _) -> do
      random <- liftIO newStdGen
      step (RandomEvent.handleButton random x game)

    (MouseDown e@CheckTrapsButton  _ _ m) -> do
      random <- liftIO newStdGen
      game & setMouseDown e m & Outside.checkTraps random & continue

    -- Normal events:
    _                            -> step (autosave game)


gameTick :: Game -> Game
gameTick game =
  let updatedTickers =
        game & over tickCount (+1)
             & over upcomingEvents tickEvents
             & over events (take 15 . map (over _2 (+1)))
      doEventIfReady e = if snd e == 0 then getGameEvent e else id
      allEvents = toList (view upcomingEvents updatedTickers)
      stateAfterIngameEvents = foldr doEventIfReady updatedTickers allEvents
  in if view paused game then game else stateAfterIngameEvents

handleEvent :: Game -> BrickEvent Name Tick -> Game
handleEvent game (AppEvent Tick) =
  -- XXX the gui ticks twice at once
  let doubleSpeedEnabled = view hyper game
      fasterFaster = gameTick . gameTick . gameTick . gameTick $ game
  in if doubleSpeedEnabled then fasterFaster else gameTick game

handleEvent g (MouseDown e _ _ m) = handleMouseDown g e m
handleEvent g MouseUp {} = set (uiState . lastReportedClick) Nothing g
handleEvent g _ = g

setMouseDown :: Name -> Brick.Location -> Game -> Game
setMouseDown buttonPressed mouseLocation game =
  game & set (uiState . lastReportedClick) (Just (buttonPressed, mouseLocation))

handleMouseDown :: Game -> Name -> Brick.Location -> Game
handleMouseDown game pressed mouseLocation =
  game & setMouseDown pressed mouseLocation & handleButtonEvent pressed

handleButtonEvent :: Name -> Game -> Game
handleButtonEvent NoOpButton = id

handleButtonEvent RoomButton = Room.arrival

handleButtonEvent LightButton = Fire.light
handleButtonEvent StokeButton = Fire.stoke

handleButtonEvent OutsideButton    = Outside.arrival
handleButtonEvent GatherButton     = Outside.gather
handleButtonEvent CheckTrapsButton = error "This should be handled up above"

handleButtonEvent TrapButton = Builder.buildTrap
handleButtonEvent CartButton = Builder.buildCart

handleButtonEvent PathButton = set location Path
handleButtonEvent ShipButton = set location Ship

handleButtonEvent RestartButton = const initGame
handleButtonEvent HyperButton = over hyper not
handleButtonEvent DebugButton = over debug not
handleButtonEvent PrevButton = set paused True . head . view previousStates
handleButtonEvent PauseButton = over paused not
handleButtonEvent DialogButton = over inEvent (\ x ->
                                                 case x of
                                                   Just _ -> Nothing
                                                   Nothing -> Just RandomEvent.theBeggar)
handleButtonEvent CheatButton =
    over (stored . bait)   (+ 50)
  . over (stored . fur)    (+ 50)
  . over (stored . meat)   (+ 50)
  . over (stored . scales) (+ 50)
  . over (stored . teeth)  (+ 50)
  . over (stored . cloth)  (+ 50)
  . over (stored . charm)  (+ 50)

handleButtonEvent _ = id
