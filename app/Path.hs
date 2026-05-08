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
  , tickBlackout
  , pathMapData
  , triggerBeastFight
  ) where

import Data.Maybe (isJust)
import System.Random (StdGen)
import Control.Lens
import Control.Monad (forM_, unless, when)
import Control.Monad.State (get, gets)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Maybe (listToMaybe)

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
  , movesUntilFood
  , movesUntilWater
  , pathPlayer
  , pathSeen
  , pathWater
  , playerStats
  , waterCapacity
  , inCombat
  , hp
  , maxHp
  )
import Shared.Item (Item(CuredMeat, Torch))
import Shared.Util (getItem, overStored)

import Path.Combat (beginCombat, snarlingBeast)
import Util (clearRoomBacklog, notify)

-- Items the player can allocate to the expedition before embarking.
-- Order here drives display order on the supply screen.
pathSupplies :: [Item]
pathSupplies = [CuredMeat, Torch]

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
  -- Re-allocate from scratch each visit until the player embarks.
  embarked .= False
  expeditionInventory .= Map.empty
  clearRoomBacklog

-- Embark moves allocated items out of stores and into the rucksack, then
-- initialises the path expedition state (position, water, consumption
-- counters, fog).
embark :: DarkRoom
embark = do
  bo  <- use blackoutCooldown
  alreadyEmbarked <- use embarked
  unless (alreadyEmbarked || bo > 0) $ do
    inv <- use expeditionInventory
    forM_ (Map.toList inv) $ \(i, n) ->
      when (n > 0) $ overStored i (subtract n)
    cap <- use (playerStats . waterCapacity)
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

pathTileAt :: (Int, Int) -> Maybe Char
pathTileAt (col, row)
  | row < 0 || row >= pathMapHeight = Nothing
  | col < 0 || col >= pathMapWidth  = Nothing
  | otherwise = Just ((pathMapData !! row) !! col)

pathMapHeight :: Int
pathMapHeight = length pathMapData

pathMapWidth :: Int
pathMapWidth = case pathMapData of
  []    -> 0
  (r:_) -> length r

-- Look for the village tile ('A') as the start; fall back to map centre.
startTile :: (Int, Int)
startTile = case findChar 'A' of
  Just c  -> c
  Nothing -> (pathMapWidth `div` 2, pathMapHeight `div` 2)
  where
    findChar target = listToMaybe
      [ (col, row)
      | (row, line) <- zip [0..] pathMapData
      , (col, ch)   <- zip [0..] line
      , ch == target
      ]

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

-- Movement ---------------------------------------------------------------

move :: Direction -> DarkRoom
move dir = do
  loc <- use location
  emb <- use embarked
  bo  <- use blackoutCooldown
  when (loc == Path && emb && bo == 0) $ do
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
  pathPlayer .= startTile
  pathWater .= 0

-- Voluntary return: rucksack contents go back into stores, reset fog,
-- drop back to the supply allocation screen (no cooldown).
goHome :: DarkRoom
goHome = do
  loc <- use location
  emb <- use embarked
  when (loc == Path && emb) $ do
    inv <- use expeditionInventory
    forM_ (Map.toList inv) $ \(i, n) ->
      when (n > 0) $ overStored i (+ n)
    embarked .= False
    expeditionInventory .= Map.empty
    pathSeen .= Set.empty
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

-- The path map. 60 rows by 61 columns of terrain characters. Lowercase
-- terrain is wilderness; uppercase letters mark named landmarks (A=village,
-- F=field, H=house, B=battlefield, etc.) which later beads will turn into
-- explorable places.
pathMapData :: [String]
pathMapData =
  [ "....,,,,,,,,,.......;;;;;;;;;;;;;Y;;;;;;;;;;;;;;;;;.........,"
  , ",,,,,,,,,,,,......;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;.........,"
  , ",,,,,,,,,,,,......;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;.........,"
  , ",,,.,,,,,,,.....;;;;;;;;;;;;;;H;;;;;;;;;;;;;;;;;;;..........,"
  , ",,,.,,,,,,....;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;......;...,"
  , ",,,,,,........;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;......;...,"
  , ".,,,,........;;;;;;;;;;;;;;;.;;;;;B;;;;;;;;;;;;;;;......;;;;;"
  , "..,,,........;;;;;;;;;;;.....;;;;;;;;;;;;;;;;;;;;;......;;;;;"
  , "..,,,.......;;;;;;;;;;;......;;;;;;;;;;;;;;;;;;;;;........;;;"
  , "...,,,...,..;;;;;;;,,........;;;;;;;;;;;;;;;;;;;...........;;"
  , "....,,;;;;..;;;;;;,,,........;;;;;;;;;;;;;;;;;;;............."
  , "....,,Y,;;;,;;;;,,,,........;;;;;;;......;;;................."
  , "....,,,,,;;;;;;,,,,......Y;;;;;;;;;.....;;;.....,....F......."
  , "....,,,,,,;;;;;,,,,.....;;;;;;;;;;;....;;;;.................."
  , "....,,,,,,;;;;,,,,.....;;;;;;;;;;;;....;;;..................."
  , "....,,,,,,;;;;,,,,.....;;;;;;;;;.......;;;................F.."
  , "..Y...,,,,,,,;;;......;;;;;;;M;;......;;;;..;................"
  , "......,,,,,,,,;;;;....;;;;;;.........;;;;;..;................"
  , "Y.....,,,,,,,,........;;;H;.O........;;;;;....B.............."
  , "......,,,,,,,,,,,....;;;;;,..........;;;;;..................."
  , "......,,,,,,,,,,,....;;;;;,..........;;;.;..................."
  , ".....Y.,,F,,,,,,,....;;;;,,.........;;;..;..................."
  , ".........,,,,,,,,,...,,O,,,..,,.....;;O;.;..................."
  , ".........,,,,,,,,,,,..,,,,,C.,......;,;;;;..................."
  , ".........,,,,,,,,,,,....,,...,......;;;;;;O;................."
  , "Y........,,,,,,,,,,,,,,......,......;;;;;;;;.B..............."
  , ".........,,,,,,,,,,,,,,,,..,,,.......;;O;;;;................B"
  , "Y..........,,,,,,,,,,,,,,V,,,....,,..;;;;;;;................."
  , "F.......B..,,,,,,,,,,,,,,,,,.......H.....;;;;................"
  , "............,,,,,,,,,,,,,,,,H,H..........;;;;;..............B"
  , "............,,,,,,,,,,,,,,,,.;A;.........;;;;;..............."
  , "F...........,,,,,,,,,,,,,,,,P##,......V..;;;;;...S..........."
  , ".............,,,,,,,,,,,,,,,.,#,V;;...O.....................Y"
  , ".............,,,,,,,,,,,,,,,,,#,..;;........................."
  , ".Y..............,,,,,,,,,,,,,IP.............................."
  , "...Y........Y...,,,,,,,,,,,,,,,.............................."
  , "B..................,,,,H,,,,,,...H..................W........"
  , ",..................,,,,,,,,O,................................"
  , ",,..................,,,,,,,,,................................"
  , ",,..................,,,,,,,.....O............................"
  , ",,,,.................,,,,,..............Y...................."
  , ",,,,,............,,,,,,,,,.............,,...................."
  , ",,,,,............,,,,,,,...............,....................."
  , ",,,,,.Y..........,,,,,,,....................................."
  , ",,,,,............,,,,,,,.....H..O..O........................."
  , ",,,,,,.......,,,,,,,,,,.....................,,..............."
  , ",,,,,,.......,.,,,,,,,.....................,,,,Y,,,,........."
  , ",,,,,,,,,....,,,,,,,,,...................,,,,,,,,,,,........."
  , ",,,,,,,,,....,,,,,,......................,,,,,,,,,,,,,......."
  , ",,,,,,,,,Y,,,,,,Y........................,,,,,,,,,,,,,,,....."
  , ",,,,,,,,.,,,,,,......................,,,,,,,,,,,,,,,,,,,....."
  , ",,,,,,,,.,,,,,,.........H...........,,,,,,,,,,,,,,,,,,,,,,..."
  , ",,,,,,,,.,,,,,......................,,,,,,,,,,,,,,,,,,,,,,,,,"
  , ",,,,,,,,,,,,...............Y......,,,,,,,,,,,,,,,,,,,,,,,,,,,"
  , ",,,,,,,,,,....................,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,"
  , ",,,,,,,,,,...................,,,,,,,,,,,,,,,,Y,,,,,,,,,,,,,,,"
  , ",,,,,,,,,,...................,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,"
  , ",,,,,,,,,,...................,,,,,,,,,,,,,,,,,,,,,,,,,,,,..,,"
  , ",,,,,,,,.....................,,,,,,,,,H,,,,,,,,,,,,,,,,,....."
  , ",,,,,,.......................,B,,,,,,,,,,,,,,,,,,,,,,,,,....."
  , ";,,,,...............Y........,,B,,B,,,,,,,,,,,,,,,,,,,,......"
  ]

-- Manual trigger for the canonical encounter while embarked. Map walking
-- (separate bead) will eventually invoke this automatically.
triggerBeastFight :: StdGen -> DarkRoom
triggerBeastFight _ = do
  onPath <- (== Path) <$> use location
  isEmbarked <- use embarked
  alreadyFighting <- isJust <$> use inCombat
  when (onPath && isEmbarked && not alreadyFighting) $
    beginCombat snarlingBeast
