{-# LANGUAGE LambdaCase #-}
module Event (handleEventWrapper) where

import System.Random (newStdGen, StdGen, split)
import Control.Monad.IO.Class (liftIO)
import Brick (BrickEvent(..), EventM, Next, continue)
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
import Control.Monad (forM_, unless, when)
import Control.Monad.State (gets, get)
import qualified Data.Map as Map

    -- EventM { runEventM :: ReaderT (EventRO n) (StateT (EventState n) IO) a
                                                   -- ReaderT (EventRO Name) (StateT (EventState Name) IO) (Next Game)

handleEventWrapper :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEventWrapper game event = do
  when (savePressed event) $
    liftIO (save game)

  stdGen <- liftIO newStdGen
  continue ((execState $ handleEvent stdGen event) game)

  where savePressed (MouseDown SaveButton _ _ _) = True
        savePressed _ = False

handleEvent :: StdGen -> BrickEvent Name Tick -> DarkRoom
handleEvent stdGen = \case
  (AppEvent Tick) -> do
    sDRE <- gets RandomEvent.shouldDoRandomEvent
    let (sG, sG1) = split stdGen
    when sDRE $ do
      RandomEvent.doRandomEvent sG1

    gameTickWrapper sG

  (MouseDown e _ _ m) -> do
    unless (e == PrevButton) $ do
      -- autosave
      g <- get
      previousStates %= (g:)

    (uiState . lastReportedClick) .= Just (e, m)
    handleButtonEvent stdGen e

  (MouseUp {}) -> do
    (uiState . lastReportedClick) .= Nothing

  -- Keyboard input does nothing for now:
  VtyEvent _ -> pure ()

gameTickWrapper :: StdGen -> DarkRoom
gameTickWrapper stdGen = do
  -- XXX the gui ticks how ever many times a hyper step is
  hyperEnabled <- use hyper
  -- TODO BUG IS THIS TICKING THE CORRECT AMOUNT, OR DID I JUST OFF BY ONE?
  hsAmt <- (\x -> if hyperEnabled then x - 1 else 0) <$> use hyperspeedAmt

  let fasterFaster 0 gen = do gameTick gen
      fasterFaster n gen = do
        let (sG, sG1) = split gen
        gameTick sG1
        fasterFaster (n - 1) sG

  fasterFaster hsAmt stdGen

gameTick :: StdGen -> DarkRoom
gameTick _ = do
  doNothing <- use paused
  unless doNothing $ do
    tickCount %= (+ 1)
    upcomingEvents %= tickEvents
    -- TODO wtf?
    events %= (take 15 . map (over _2 (+1)))

    allEvs <- filter (\x -> snd x == 0) . Map.toList <$> use upcomingEvents
    forM_ allEvs $ \(e, _) -> do
      getGameEvent e

  where
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

handleButtonEvent :: StdGen -> Name -> DarkRoom
handleButtonEvent stdGen = \case
  (RandomEventButton s) -> RandomEvent.handleButton stdGen s

  RoomButton -> Room.arrival

  LightButton -> Fire.light
  StokeButton -> Fire.stoke

  OutsideButton    -> Outside.arrival
  GatherButton     -> Outside.gather
  CheckTrapsButton -> Outside.checkTraps stdGen

  (CraftButton x) -> Builder.build x

  PathButton -> do location .= Path
  ShipButton -> do location .= Ship

  SaveButton -> pure ()
  RestartButton -> do modify (const initGame)
  HyperButton -> do hyper %= not
                    RandomEvent.setNextRandomEvent stdGen
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

  -- UI Faff:
  NoOpButton -> pure ()
  StoreVP -> pure ()
  ForestVP -> pure ()
  EventsVP -> pure ()
