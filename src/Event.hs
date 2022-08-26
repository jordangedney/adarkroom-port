{-# LANGUAGE LambdaCase #-}
module Event (handleEventWrapper) where

import System.Random (newStdGen)
import Control.Monad.IO.Class (liftIO)
import Brick (BrickEvent(..), EventM, Next, Location, continue)
import Control.Monad.State (execState, modify)

import Shared.Game
import Shared.GameEvent
import Shared.UI (Name(..), lastReportedClick)
import Shared.Util (overStored)
import Shared.Item
import SaveGame (save)

import qualified Room.Fire as Fire
import qualified Room.Room as Room
import qualified Room.Builder as Builder
import qualified Room.Event as RE
import qualified Outside

import qualified RandomEvent
import Control.Lens
import Control.Monad (forM_, unless)
import qualified Data.Map as Map

    -- EventM { runEventM :: ReaderT (EventRO n) (StateT (EventState n) IO) a
                                                   -- ReaderT (EventRO Name) (StateT (EventState Name) IO) (Next Game)

handleEventWrapper :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEventWrapper game event =
  let step g = continue ((execState $ handleEvent event) g)
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
        step ((execState $ RandomEvent.doRandomEvent random) game)
      else step game

    (MouseDown SaveButton _ _ _) -> do
      liftIO (save game)
      step game

    (MouseDown (RandomEventButton x) _ _ _) -> do
      random <- liftIO newStdGen
      step ((execState $ RandomEvent.handleButton random x) game)

    (MouseDown e@CheckTrapsButton  _ _ m) -> do
      random <- liftIO newStdGen
      game & execState (setMouseDown e m)
           & execState (Outside.checkTraps random)
           & continue

    (MouseDown e@HyperButton   _ _ m) -> do
      -- Change the tick at which the next random event occurs to avoid
      -- accidentally skipping over it.
      random <- liftIO newStdGen
      game & execState (setMouseDown e m)
           & over hyper not
           & execState (RandomEvent.setNextRandomEvent random)
           & continue

    -- Normal events:
    _                            -> step (autosave game)

handleEvent :: BrickEvent Name Tick -> DarkRoom
handleEvent = \case
  (AppEvent Tick) -> do
    -- XXX the gui ticks how ever many times a hyper step is
    hyperEnabled <- use hyper
    -- TODO BUG IS THIS TICKING THE CORRECT AMOUNT, OR DID I JUST OFF BY ONE?
    hsAmt <- (\x -> if hyperEnabled then x - 1 else 0) <$> use hyperspeedAmt

    let fasterFaster 0 = do gameTick
        fasterFaster n = do
          gameTick
          fasterFaster (n - 1)

    fasterFaster hsAmt


  (MouseDown e _ _ m) -> handleMouseDown e m
  MouseUp {} -> (uiState . lastReportedClick) .= Nothing
  _ -> pure ()

setMouseDown :: Name -> Brick.Location -> DarkRoom
setMouseDown buttonPressed mouseLocation = do
  (uiState . lastReportedClick) .= Just (buttonPressed, mouseLocation)

handleMouseDown :: Name -> Brick.Location -> DarkRoom
handleMouseDown pressed mouseLocation = do
  setMouseDown pressed mouseLocation
  handleButtonEvent pressed

handleButtonEvent :: Name -> DarkRoom
handleButtonEvent = \case
  NoOpButton -> pure ()

  RoomButton -> Room.arrival

  LightButton -> Fire.light
  StokeButton -> Fire.stoke

  OutsideButton    -> Outside.arrival
  GatherButton     -> Outside.gather
  CheckTrapsButton -> error "This should be handled in handleEventWrapper"

  (CraftButton x) -> Builder.build x

  PathButton -> do location .= Path
  ShipButton -> do location .= Ship

  RestartButton -> do modify (const initGame)
  HyperButton -> error "This should be handled in handleEventWrapper"
  DebugButton -> do debug %= not
  PrevButton -> do
    prev <- head <$> use previousStates
    modify (const prev)
    paused .= True
  PauseButton -> do paused %= not
  DialogButton -> do
    inEvent %= (\case { Just _ -> Nothing ; _ -> Just RE.theShadyBuilder })
  CheatButton -> do
    overStored Wood  (+ 5000)
    overStored Bait  (+ 5000)
    overStored Fur   (+ 5000)
    overStored Meat  (+ 5000)
    overStored Scale (+ 5000)
    overStored Teeth (+ 5000)
    overStored Cloth (+ 5000)
    overStored Charm (+ 5000)

  _ -> pure ()

getGameEvent :: GameEvent -> DarkRoom
getGameEvent = \case
  UnlockForest       -> Outside.unlock
  FireShrinking      -> Fire.shrinking
  BuilderUpdate      -> Builder.update
  BuilderGathersWood -> Builder.gatherWood
  RoomChanged        -> Room.update

  -- Button Cooldowns
  GatherWood         -> pure ()
  FireStoked         -> pure ()
  CheckTraps         -> pure ()

gameTick :: DarkRoom
gameTick = do
  doNothing <- use paused
  unless doNothing $ do
    tickCount %= (+ 1)
    upcomingEvents %= tickEvents
    -- TODO wtf?
    events %= (take 15 . map (over _2 (+1)))

    allEvs <- filter (\x -> snd x == 0) . Map.toList <$> use upcomingEvents
    forM_ allEvs $ \(e, _) -> do
      getGameEvent e
