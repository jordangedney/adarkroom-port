module Path
  ( arrival
  , embark
  , increaseSupply
  , decreaseSupply
  , pathSupplies
  , allocated
  , available
  , inventoryUsed
  , inventoryFree
  , Direction(..)
  , move
  , goHome
  , scout
  , scoutAvailable
  , tickBlackout
  , triggerBeastFight
  , advanceAfterCombat
  , advanceAfterRewards
  , exitPlaceEarly
  , renderedTileAt
  ) where

import Data.Maybe (isJust)
import System.Random (StdGen)
import Control.Lens
import Control.Monad (forM_, unless, when)
import Control.Monad.State (get, gets)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Shared.Constants
  ( pathStepsPerWater
  , pathStepsPerFood
  , pathBlackoutTicks
  )
import Shared.Game
  ( DarkRoom
  , Game
  , Location(Path)
  , blackoutCooldown
  , embarked
  , expeditionInventory
  , inventoryCapacity
  , location
  , milestones
  , movesUntilFood
  , movesUntilWater
  , pathExplored
  , pathPlace
  , pathPlayer
  , pathPreferredAllocation
  , pathRoads
  , pathSeen
  , pathWater
  , playerStats
  , scoutUnlocked
  , waterCapacity
  , inCombat
  , inRewards
  , hp
  , maxHp
  , stored
  )
import Shared.Item (Item(CuredMeat, Torch, Medicine, Bullets, Bolas, Grenades))
import Shared.PathMap
  ( pathMapHeight
  , pathMapWidth
  , pathTileAt
  , villageTile
  )
import Shared.Util (getItem, overStored)

import Path.Combat (beginCombat, snarlingBeast)
import qualified Path.Place as Place
import Util (clearRoomBacklog, notify)

-- Items the player can allocate to the expedition before embarking.
-- Order here drives display order on the supply screen.
pathSupplies :: [Item]
pathSupplies = [CuredMeat, Torch, Medicine, Bullets, Bolas, Grenades]

allocated :: Item -> Game -> Int
allocated i g = Map.findWithDefault 0 i (g ^. expeditionInventory)

-- Stored amount minus what's already been allocated for this expedition.
available :: Item -> Game -> Int
available i g = getItem i g - allocated i g

inventoryUsed :: Game -> Int
inventoryUsed g = sum (Map.elems (g ^. expeditionInventory))

inventoryFree :: Game -> Int
inventoryFree g = (g ^. playerStats . inventoryCapacity) - inventoryUsed g

arrival :: DarkRoom
arrival = do
  location .= Path
  embarked .= False
  -- Pre-allocate from whatever the player took on the previous expedition,
  -- capped by what's currently in stores. Items they've run out of just
  -- silently drop off the allocation.
  preferred <- use pathPreferredAllocation
  cap <- use (playerStats . inventoryCapacity)
  storeMap <- use stored
  let (newInv, _) = preallocate cap storeMap preferred
  expeditionInventory .= newInv
  clearRoomBacklog

-- Cap allocation by both the rucksack capacity and the available stock for
-- each item. Walks pathSupplies in order so capacity goes to higher-priority
-- items first when the player is out of room.
preallocate
  :: Int                  -- ^ rucksack capacity
  -> Map.Map Item Int     -- ^ items currently in stores
  -> Map.Map Item Int     -- ^ preferred amounts from last expedition
  -> (Map.Map Item Int, Int)
preallocate cap storeMap preferred = foldl step (Map.empty, cap) pathSupplies
  where
    step (acc, remaining) i =
      let want  = Map.findWithDefault 0 i preferred
          have  = Map.findWithDefault 0 i storeMap
          take' = max 0 (minimum [want, have, remaining])
      in if take' > 0
         then (Map.insert i take' acc, remaining - take')
         else (acc, remaining)

-- Embark moves allocated items out of stores and into the rucksack, then
-- initialises the path expedition state (position, water, consumption
-- counters, fog).
embark :: DarkRoom
embark = do
  bo  <- use blackoutCooldown
  alreadyEmbarked <- use embarked
  cap <- use (playerStats . waterCapacity)
  unless (alreadyEmbarked || bo > 0 || cap <= 0) $ do
    inv <- use expeditionInventory
    forM_ (Map.toList inv) $ \(i, n) ->
      when (n > 0) $ overStored i (subtract n)
    pathWater .= cap
    movesUntilWater .= pathStepsPerWater
    movesUntilFood  .= pathStepsPerFood
    pathPlayer .= startTile
    pathSeen   .= seenAround startTile
    embarked .= True
    -- Heal at the trailhead so each expedition starts at full HP.
    mx <- use (playerStats . maxHp)
    playerStats . hp .= mx

increaseSupply :: Item -> DarkRoom
increaseSupply i = do
  avail <- gets (available i)
  free <- gets inventoryFree
  when (avail > 0 && free > 0) $
    expeditionInventory %= Map.insertWith (+) i 1

decreaseSupply :: Item -> DarkRoom
decreaseSupply i = do
  cur <- gets (allocated i)
  when (cur > 0) $
    expeditionInventory %= Map.adjust (subtract 1) i

-- Path map ---------------------------------------------------------------

data Direction = North | South | East | West deriving (Eq, Show)

-- The starting tile is the village 'A' from the shared map.
startTile :: (Int, Int)
startTile = villageTile

-- How many tiles the player can see in each direction from their position
-- (Chebyshev / king-move distance). 2 gives a 5x5 visible square.
sightRadius :: Int
sightRadius = 2

-- All in-bounds tiles within `sightRadius` of `(col, row)`.
seenAround :: (Int, Int) -> Set.Set (Int, Int)
seenAround (col, row) = Set.fromList
  [ (c, r)
  | dc <- [-sightRadius .. sightRadius]
  , dr <- [-sightRadius .. sightRadius]
  , let c = col + dc
        r = row + dr
  , c >= 0, c < pathMapWidth
  , r >= 0, r < pathMapHeight
  ]

-- | What glyph the player should see at a tile, factoring in exploration
-- conversions and roads. Used by the renderer.
renderedTileAt :: Game -> (Int, Int) -> Maybe Char
renderedTileAt game pos
  | pos `Set.member` view pathExplored game =
      case pathTileAt pos of
        Just 'I' -> Just 'I'                       -- iron mine stays I
        _        -> Just 'P'
  | pos `Set.member` view pathRoads game = Just '#'
  | otherwise = pathTileAt pos

-- | Scout reveals a wide ring of tiles around the player. Available once the
-- wandering scout event has hired on; consumes the scout — single use per
-- expedition's worth of revealed terrain.
scoutAvailable :: Game -> Bool
scoutAvailable g =
     view location g == Path
  && view embarked g
  && view (milestones . scoutUnlocked) g

scoutRadius :: Int
scoutRadius = 5

scout :: DarkRoom
scout = do
  avail <- gets scoutAvailable
  when avail $ do
    (col, row) <- use pathPlayer
    let reveal = Set.fromList
          [ (c, r)
          | dc <- [-scoutRadius .. scoutRadius]
          , dr <- [-scoutRadius .. scoutRadius]
          , let c = col + dc
                r = row + dr
          , c >= 0, c < pathMapWidth
          , r >= 0, r < pathMapHeight
          ]
    pathSeen %= Set.union reveal
    notify "the scout points out landmarks on the horizon."

-- Movement ---------------------------------------------------------------

move :: Direction -> DarkRoom
move dir = do
  loc <- use location
  emb <- use embarked
  bo  <- use blackoutCooldown
  inP <- use pathPlace
  inC <- use inCombat
  inR <- use inRewards
  when (loc == Path && emb && bo == 0
        && not (isJust inP) && not (isJust inC) && not (isJust inR)) $ do
    (col, row) <- use pathPlayer
    let prevTile = pathTileAt (col, row)
        (nc, nr) = case dir of
          North -> (col, row - 1)
          South -> (col, row + 1)
          West  -> (col - 1, row)
          East  -> (col + 1, row)
    case pathTileAt (nc, nr) of
      Nothing -> pure ()
      Just newTile -> do
        pathPlayer .= (nc, nr)
        pathSeen %= Set.union (seenAround (nc, nr))
        when (terrainName prevTile /= terrainName (Just newTile)) $
          forM_ (terrainName (Just newTile)) $ \name ->
            notify ("you enter " <> name <> ".")
        consumeStep
        -- After the step is paid for, see if we walked onto a fresh place
        -- tile and trigger its exploration sequence.
        maybeEnterPlace (nc, nr)

-- | If the player just stepped onto a landmark glyph that hasn't been
-- explored yet, push them into the place's exploration sequence.
maybeEnterPlace :: (Int, Int) -> DarkRoom
maybeEnterPlace pos = do
  alreadyExplored <- gets (Set.member pos . view pathExplored)
  unless alreadyExplored $ do
    case pathTileAt pos >>= Place.placeFromGlyph of
      Nothing -> pure ()
      Just place -> Place.enterPlace place pos

-- | Re-enter the current place's event queue after combat ends or the
-- rewards screen is dismissed. Routes the rewards/wakeUp/leave button
-- handlers in 'Event' so the player can keep pushing through a place.
advanceAfterCombat :: DarkRoom
advanceAfterCombat = do
  inP <- use pathPlace
  when (isJust inP) Place.advanceAfterCombat

advanceAfterRewards :: DarkRoom
advanceAfterRewards = do
  inP <- use pathPlace
  when (isJust inP) Place.advanceAfterRewards

exitPlaceEarly :: DarkRoom
exitPlaceEarly = do
  inP <- use pathPlace
  when (isJust inP) Place.exitPlaceEarly

consumeStep :: DarkRoom
consumeStep = do
  movesUntilWater %= subtract 1
  mw <- use movesUntilWater
  when (mw <= 0) $ do
    pathWater %= subtract 1
    movesUntilWater .= pathStepsPerWater
  movesUntilFood %= subtract 1
  mf <- use movesUntilFood
  when (mf <= 0) $ do
    -- insertWith (+) so the count drops below zero even when the player
    -- never allocated cured meat — that case still represents starvation.
    expeditionInventory %= Map.insertWith (+) CuredMeat (-1)
    movesUntilFood .= pathStepsPerFood
  pw <- use pathWater
  pf <- gets (Map.findWithDefault 0 CuredMeat . view expeditionInventory)
  when (pw < 0) $ do
    notify "your throat is parched. you collapse from thirst."
    blackout
  when (pw >= 0 && pf < 0) $ do
    notify "starving and dizzy, you collapse."
    blackout

-- Wipe the expedition: discard rucksack, reset fog, leave the path, start
-- the recovery counter. Player wakes up at the path screen with embark
-- locked until the cooldown clears.
blackout :: DarkRoom
blackout = do
  blackoutCooldown .= pathBlackoutTicks
  embarked .= False
  expeditionInventory .= Map.empty
  pathSeen .= Set.empty
  pathPlace .= Nothing
  pathPlayer .= startTile
  pathWater .= 0

-- Voluntary return: rucksack contents go back into stores, reset fog,
-- drop back to the supply allocation screen (no cooldown). Items the
-- player took are remembered as the next expedition's preferred allocation.
-- Disabled while a combat or rewards modal is up so the player can't
-- accidentally bail out of a place mid-encounter.
goHome :: DarkRoom
goHome = do
  loc <- use location
  emb <- use embarked
  inC <- use inCombat
  inR <- use inRewards
  when (loc == Path && emb && not (isJust inC) && not (isJust inR)) $ do
    inv <- use expeditionInventory
    -- Remember the takes so the next expedition pre-allocates them.
    pathPreferredAllocation .= Map.filter (> 0) inv
    forM_ (Map.toList inv) $ \(i, n) ->
      when (n > 0) $ overStored i (+ n)
    embarked .= False
    expeditionInventory .= Map.empty
    pathSeen .= Set.empty
    pathPlace .= Nothing
    pathPlayer .= startTile
    pathWater .= 0
    notify "you stagger back to the village."

tickBlackout :: DarkRoom
tickBlackout = do
  bo <- use blackoutCooldown
  when (bo > 0) $ do
    blackoutCooldown %= subtract 1
    g <- get
    when (view blackoutCooldown g == 0) $
      notify "you feel ready to set out again."

-- Terrain naming ---------------------------------------------------------

-- Friendly names for the tiles seen on the map. Tiles that are either the
-- player marker or have no narrative description return Nothing so we don't
-- fire spurious "you enter ..." messages.
terrainName :: Maybe Char -> Maybe String
terrainName Nothing  = Nothing
terrainName (Just c) = case c of
  '.' -> Just "the barrens"
  ',' -> Just "the scrubland"
  ';' -> Just "the forest"
  '#' -> Just "a road"
  _   -> Nothing

-- Manual trigger for the canonical encounter while embarked. Map walking
-- (separate bead) will eventually invoke this automatically.
triggerBeastFight :: StdGen -> DarkRoom
triggerBeastFight _ = do
  onPath <- (== Path) <$> use location
  isEmbarked <- use embarked
  alreadyFighting <- isJust <$> use inCombat
  inP <- use pathPlace
  when (onPath && isEmbarked && not alreadyFighting && not (isJust inP)) $
    beginCombat snarlingBeast
