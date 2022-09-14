{-# LANGUAGE LambdaCase #-}
module Event (handleEventWrapper) where

import System.Random (newStdGen, StdGen)
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
import Control.Monad.State (get)
import Control.Concurrent.STM.TVar (TVar, writeTVar)
import Control.Monad.STM (atomically)
import qualified Data.Map as Map

-- | Perform IO and convert from Brick's EventM into the internal DarkRoom type.
handleEventWrapper :: TVar Bool -> Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEventWrapper enableHyper game event = do
  when (pressed SaveButton) $
    liftIO (save game)

  when (pressed HyperButton) $ do
    liftIO $ atomically $ writeTVar enableHyper (not (_hyper game))

  stdGen <- liftIO newStdGen
  continue ((execState $ handleBrickEvent stdGen event) game)

  where pressed b = case event of MouseDown x _ _ _ -> b == x; _ -> False

-- | Game-engine bookkeeping before dispatching story-driven events.
handleBrickEvent :: StdGen -> BrickEvent Name Tick -> DarkRoom
handleBrickEvent stdGen = \case
  AppEvent Tick -> do
    doNothing <- use paused
    unless doNothing $ do
      tickCount %= (+ 1)
      upcomingEvents %= (fmap (+ (-1)))

      -- Only the last 15 notificaitons are likely to be relevant.
      -- Keep track of the age so that the UI can change the color.
      notifications %= (take 15 . map (over _2 (+1)))

      allEvs <- filter (\x -> snd x == 0) . Map.toList <$> use upcomingEvents
      forM_ allEvs $ \(e, _) -> handleGameEvent stdGen e

  MouseDown e _ _ m -> do
    unless (e == PrevButton) $ do
      -- Autosave for debugging.
      g <- get
      previousStates %= (g:)

    uiState . lastReportedClick .= Just (e, m)
    handleButtonEvent stdGen e

  MouseUp {} -> do uiState . lastReportedClick .= Nothing

  -- Keyboard input does nothing for now.
  VtyEvent _ -> pure ()

-- | Events which occur randomly or over time.
handleGameEvent :: StdGen -> GameEvent -> DarkRoom
handleGameEvent stdGen = \case
  UnlockForest       -> Outside.unlock
  FireShrinking      -> Fire.shrinking
  BuilderUpdate      -> Builder.update
  BuilderGathersWood -> Builder.gatherWood
  RoomChanged        -> Room.update
  Random             -> RandomEvent.start stdGen
  PopulationIncrease -> Room.increasePopulation stdGen

  -- Button Cooldowns:
  GatherWood         -> pure ()
  FireStoked         -> pure ()
  CheckTraps         -> pure ()

-- | User driven events, created through the UI.
handleButtonEvent :: StdGen -> Name -> DarkRoom
handleButtonEvent stdGen = \case
  RandomEventButton s -> RandomEvent.handleButton stdGen s

  RoomButton -> Room.arrival

  LightButton -> Fire.light
  StokeButton -> Fire.stoke

  OutsideButton    -> Outside.arrival
  GatherButton     -> Outside.gather
  CheckTrapsButton -> Outside.checkTraps stdGen

  CraftButton x -> Builder.build x

  PathButton -> do location .= Path
  ShipButton -> do location .= Ship

  RestartButton -> do modify (const initGame)
  DebugButton -> do debug %= not
  PrevButton -> do
    prev <- head <$> use previousStates
    modify (const prev)
    paused .= True
  PauseButton -> do paused %= not
  DialogButton -> do
    inEvent %= (\case { Just _ -> Nothing ; _ -> Just RE.beastAttack })
  CheatButton -> do
    overStored Wood  (+ 5000)
    overStored Bait  (+ 5000)
    overStored Fur   (+ 5000)
    overStored Meat  (+ 5000)
    overStored Scale (+ 5000)
    overStored Teeth (+ 5000)
    overStored Cloth (+ 5000)
    overStored Charm (+ 5000)

  -- Both of these are touched above too, as they need IO.
  SaveButton  -> pure ()
  HyperButton -> do hyper %= not

  -- We don't support scrolling within UI windows.
  StoreVP -> pure ()
  ForestVP -> pure ()
  EventsVP -> pure ()
