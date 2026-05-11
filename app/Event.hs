{-# LANGUAGE LambdaCase #-}
module Event (handleEventWrapper) where

import System.Random (newStdGen, StdGen)
import Control.Monad.IO.Class (liftIO)
import Brick (BrickEvent(..), EventM)
import Control.Monad.State (execState, modify)

import Shared.Game
import Shared.GameEvent
import Shared.UI (Name(..), lastReportedClick)
import Shared.Util (overStored, assignWorker)
import Shared.Item
import Shared.Worker (Worker(Gatherer))
import SaveGame (save)

import qualified Graphics.Vty as V

import qualified Room.Fire as Fire
import qualified Room.Room as Room
import qualified Room.Builder as Builder
-- import qualified Room.Event as RE
import qualified Outside
import qualified Path
import qualified Path.Combat as Combat
import qualified Rewards
import Shared.Rewards (RewardsContext(..))

import qualified RandomEvent
import Control.Lens
import Control.Monad (forM_, unless, when)
import Control.Monad.State (get, put)
import Control.Concurrent.STM.TVar (TVar, writeTVar)
import Control.Monad.STM (atomically)
import qualified Data.Map as Map

-- | Perform IO and convert from Brick's EventM into the internal DarkRoom type.
handleEventWrapper :: TVar Bool -> BrickEvent Name Tick -> EventM Name Game ()
handleEventWrapper enableHyper event = do
  game <- get
  when (pressed SaveButton) $
    liftIO (save game)

  when (pressed HyperButton) $ do
    liftIO $ atomically $ writeTVar enableHyper (not (_hyper game))

  stdGen <- liftIO newStdGen
  put $ ((execState $ handleBrickEvent stdGen event) game)

  where pressed b = case event of MouseDown x _ _ _ -> b == x; _ -> False

-- | Game-engine bookkeeping before dispatching story-driven events.
handleBrickEvent :: StdGen -> BrickEvent Name Tick -> DarkRoom
handleBrickEvent stdGen = \case
  AppEvent Tick -> do
    doNothing <- use paused
    unless doNothing $ do
      tickCount %= (+ 1)
      upcomingEvents %= (fmap (+ (-1)))
      Path.tickBlackout

      -- Only the last 15 notificaitons are likely to be relevant.
      -- Keep track of the age so that the UI can change the color.
      notifications %= (take 15 . map (over _2 (+1)))

      Combat.combatTick

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

  VtyEvent (V.EvKey k _) -> handlePathKey k
  VtyEvent _ -> pure ()

handlePathKey :: V.Key -> DarkRoom
handlePathKey k = case k of
  V.KChar 'h' -> Path.move Path.West
  V.KChar 'j' -> Path.move Path.South
  V.KChar 'k' -> Path.move Path.North
  V.KChar 'l' -> Path.move Path.East
  V.KLeft     -> Path.move Path.West
  V.KDown     -> Path.move Path.South
  V.KUp       -> Path.move Path.North
  V.KRight    -> Path.move Path.East
  V.KChar 'a' -> Path.goHome
  V.KChar 'A' -> Path.goHome
  _           -> pure ()

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
  WorkerIncome       -> Outside.applyWorkerIncome

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
  BuyButton x   -> Builder.buy x

  IncWorker w -> assignWorker Gatherer w 1
  DecWorker w -> assignWorker w Gatherer 1

  PathButton -> Path.arrival
  EmbarkButton -> Path.embark
  IncreaseSupplyButton i -> Path.increaseSupply i
  DecreaseSupplyButton i -> Path.decreaseSupply i

  AttackButton          -> Combat.attackBeast stdGen
  ClaimRewardsButton    -> do
    Combat.claimRewards stdGen
    Path.advanceAfterCombat
  WakeUpButton          -> do
    Combat.wakeUp
    Path.exitPlaceEarly
  StartBeastFightButton -> Path.triggerBeastFight stdGen

  TakeRewardButton i -> Rewards.takeOne i
  DropRewardButton i -> Rewards.dropOne i
  TakeAllRewardsButton -> Rewards.takeAll
  EatMeatButton -> do
    -- During combat, eating meat heals mid-fight; otherwise it's the
    -- rewards screen "eat meat" button (same effect, no combat ticking).
    inC <- use inCombat
    case inC of
      Just _  -> Combat.eatMeat
      Nothing -> Rewards.eatMeat
  LeaveRewardsButton -> do
    Rewards.leave
    Path.exitPlaceEarly
  ContinueRewardsButton -> do
    Rewards.continue
    Path.advanceAfterRewards
  DebugRewardsButton ->
    Rewards.open
      "you killed the beast."
      (Map.fromList [(Fur, 8), (Meat, 5), (Teeth, 2)])
      RewardsCombat

  ShipButton -> do location .= Ship

  RestartButton -> do modify (const initGame)
  DebugButton -> do debug %= not
  PrevButton -> do
    prev <- head <$> use previousStates
    modify (const prev)
    paused .= True
  PauseButton -> do paused %= not
  DialogButton -> RandomEvent.start stdGen
    -- inEvent %= (\case { Just _ -> Nothing ; _ -> Just RE.beastAttack})
  CheatButton -> do
    overStored Wood      (+ 5000)
    overStored Bait      (+ 5000)
    overStored Fur       (+ 5000)
    overStored Meat      (+ 5000)
    overStored Scale     (+ 5000)
    overStored Teeth     (+ 5000)
    overStored Cloth     (+ 5000)
    overStored Charm     (+ 5000)
    overStored CuredMeat (+ 5000)

  -- Both of these are touched above too, as they need IO.
  SaveButton  -> pure ()
  HyperButton -> do hyper %= not

  -- We don't support scrolling within UI windows.
  StoreVP -> pure ()
  ForestVP -> pure ()
  EventsVP -> pure ()
  WorkersVP -> pure ()
