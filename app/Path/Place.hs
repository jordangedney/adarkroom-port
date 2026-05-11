{-# LANGUAGE LambdaCase #-}

-- | Explorable places on the path. Each place is a multi-room mini-dungeon
-- represented as a static list of 'PlaceEvent's; entering a place pushes the
-- first event (combat or rewards screen), and as each event resolves the
-- exploration state machine fires the next one. When the last event ends the
-- place is marked explored, its tile glyph flips to 'P' (except the iron
-- mine), and a road is drawn back to the village.
module Path.Place
  ( placeFromGlyph
  , placeName
  , placeBecomesOutpost
  , placeTotalEvents
  , enterPlace
  , advanceAfterCombat
  , advanceAfterRewards
  , exitPlaceEarly
  ) where

import Control.Lens
import Control.Monad (when, unless)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Shared.Game
import Shared.Item (Item(..))
import Shared.PathMap (pathTileAt, villageTile)
import Shared.Place
import Shared.Rewards (RewardsContext(..))

import qualified Rewards
import Path.Combat
  ( BeastSpec(..)
  , beginCombat
  , snarlingBeastSpec
  , scavengerSpec
  , meatEaterSpec
  , feralTerrorSpec
  , soldierSpec
  , sniperSpec
  , gauntManSpec
  )
import Util (notify)

-- | Each scripted beat in a place's exploration sequence.
data PlaceEvent
  = PlaceLoot String (Map.Map Item Int)
  | PlaceEnemy BeastSpec

-- | Map an uppercase landmark glyph on the path map to the place it
-- represents. Returns 'Nothing' for terrain, the village, the player, and
-- already-converted outposts.
placeFromGlyph :: Char -> Maybe Place
placeFromGlyph = \case
  'V' -> Just DampCave
  'I' -> Just IronMine
  'O' -> Just DesertedTown
  'H' -> Just OldHouse
  'C' -> Just CoalMine
  'F' -> Just CrashedShip
  'S' -> Just SulfurMine
  'B' -> Just Battlefield
  'Y' -> Just RuinedCity
  'M' -> Just HugeBorehole
  'W' -> Just MurkySwamp
  'P' -> Just Outpost
  _   -> Nothing

placeName :: Place -> String
placeName = \case
  DampCave     -> "a damp cave"
  IronMine     -> "the iron mine"
  Outpost      -> "an outpost"
  OldHouse     -> "an old house"
  CoalMine     -> "the coal mine"
  DesertedTown -> "a deserted town"
  SulfurMine   -> "the sulfur mine"
  Battlefield  -> "a forgotten battlefield"
  RuinedCity   -> "a ruined city"
  HugeBorehole -> "a huge borehole"
  CrashedShip  -> "a crashed ship"
  MurkySwamp   -> "a murky swamp"

-- | Iron mines stay tagged 'I' on the map even after exploration so the
-- player can still see where the iron came from; everything else converts to
-- 'P' once cleared.
placeBecomesOutpost :: Place -> Bool
placeBecomesOutpost IronMine = False
placeBecomesOutpost _        = True

-- | Static scene script for each place. Loot scenes show the rewards screen;
-- enemy scenes trigger combat. Multiple scenes per place satisfy the
-- "multi-room mini-dungeon" requirement on the bead.
placeEvents :: Place -> [PlaceEvent]
placeEvents = \case
  DampCave ->
    [ PlaceLoot "a chill drips from the rocks. nests of cloth lie under a ledge."
        (Map.fromList [(Cloth, 2), (Wood, 2)])
    , PlaceEnemy snarlingBeastSpec
    , PlaceLoot "the beast's lair is strewn with scraps."
        (Map.fromList [(Fur, 3), (Teeth, 2), (Meat, 2)])
    ]
  IronMine ->
    [ PlaceLoot "rusted rails lead deep into the rock."
        (Map.fromList [(Iron, 3), (Wood, 2)])
    , PlaceEnemy scavengerSpec
    , PlaceLoot "an abandoned cart sits half-loaded with ore."
        (Map.fromList [(Iron, 5), (Coal, 1)])
    ]
  Outpost ->
    [ PlaceLoot "an old cache sits unmolested at the outpost."
        (Map.fromList [(CuredMeat, 3), (Wood, 4), (Cloth, 1)])
    ]
  OldHouse ->
    [ PlaceLoot "the door creaks open. dust hangs in the light."
        (Map.fromList [(Cloth, 2), (Leather, 1)])
    , PlaceLoot "a footlocker yields a few keepsakes."
        (Map.fromList [(Charm, 1), (Cloth, 2), (Wood, 1)])
    ]
  CoalMine ->
    [ PlaceLoot "the air is thick with coal dust."
        (Map.fromList [(Coal, 4)])
    , PlaceEnemy gauntManSpec
    , PlaceLoot "a coal seam glistens in the lantern light."
        (Map.fromList [(Coal, 6), (Iron, 1)])
    ]
  DesertedTown ->
    [ PlaceLoot "the streets are silent. a few houses still stand."
        (Map.fromList [(Cloth, 3), (Wood, 4)])
    , PlaceEnemy scavengerSpec
    , PlaceLoot "you sift through what the scavengers missed."
        (Map.fromList [(Iron, 2), (Leather, 2), (CuredMeat, 1)])
    ]
  SulfurMine ->
    [ PlaceLoot "the tunnels reek; the air burns your lungs."
        (Map.fromList [(Sulphur, 3), (Coal, 1)])
    , PlaceLoot "a vein of sulfur glows yellow in the dim."
        (Map.fromList [(Sulphur, 5)])
    ]
  Battlefield ->
    [ PlaceLoot "rusted helmets and bones cover the ground."
        (Map.fromList [(Iron, 2), (Cloth, 3)])
    , PlaceEnemy soldierSpec
    , PlaceLoot "weapons and armor lie scattered among the fallen."
        (Map.fromList [(Iron, 3), (Steel, 1), (Bullets, 2)])
    ]
  RuinedCity ->
    [ PlaceLoot "broken buildings cast long shadows across the rubble."
        (Map.fromList [(Cloth, 4), (Leather, 2)])
    , PlaceEnemy feralTerrorSpec
    , PlaceLoot "treasures linger in the wreckage."
        (Map.fromList [(Steel, 2), (Charm, 1), (Cloth, 2)])
    ]
  HugeBorehole ->
    [ PlaceLoot "the borehole gapes downward, impossibly deep."
        (Map.fromList [(Iron, 3), (Coal, 2)])
    , PlaceLoot "machinery half-buried in slag yields up its parts."
        (Map.fromList [(Steel, 3), (Iron, 2)])
    ]
  CrashedShip ->
    [ PlaceLoot "a ship lies broken across the dunes."
        (Map.fromList [(Steel, 2), (Cloth, 3)])
    , PlaceEnemy sniperSpec
    , PlaceLoot "you salvage what you can from the bridge."
        (Map.fromList [(Steel, 3), (Bullets, 3), (Rifle, 1)])
    ]
  MurkySwamp ->
    [ PlaceLoot "the swamp gurgles around you; mist clings to the reeds."
        (Map.fromList [(Meat, 2), (Cloth, 1)])
    , PlaceEnemy meatEaterSpec
    , PlaceLoot "the bog yields its dead, and what they carried."
        (Map.fromList [(Scale, 2), (Teeth, 2), (Charm, 1)])
    ]

placeTotalEvents :: Place -> Int
placeTotalEvents = length . placeEvents

-- | Player has stepped onto a place tile. Mark the place as in-progress and
-- fire the first event.
enterPlace :: Place -> (Int, Int) -> DarkRoom
enterPlace p tile = do
  pathPlace .= Just (PlaceState p tile 0)
  notify ("you enter " <> placeName p <> ".")
  fireCurrentEvent

-- | Drive the current event in the player's place. When the player has run
-- past the last scripted event, finish the place.
fireCurrentEvent :: DarkRoom
fireCurrentEvent = do
  inP <- use pathPlace
  case inP of
    Nothing -> pure ()
    Just (PlaceState p _ idx) -> do
      let evs = placeEvents p
      if idx >= length evs
        then finishPlace
        else case evs !! idx of
          PlaceLoot txt loot ->
            Rewards.open txt loot RewardsExploration
          PlaceEnemy spec ->
            beginCombat spec

-- | Combat just ended — either a victory (claimRewards) or the player took a
-- non-fatal interrupt. Bump the event index past the enemy and fire whatever
-- comes next (typically the loot scene that follows the fight).
advanceAfterCombat :: DarkRoom
advanceAfterCombat = bumpStep >> fireCurrentEvent

-- | The rewards screen for the current place event closed via "continue" —
-- player wants to push deeper.
advanceAfterRewards :: DarkRoom
advanceAfterRewards = bumpStep >> fireCurrentEvent

bumpStep :: DarkRoom
bumpStep = pathPlace . _Just . psStep %= (+ 1)

-- | Player bailed out of a place mid-exploration (rewards "leave", or
-- combat blackout). Drop the place state without marking the tile cleared so
-- a return visit starts the place over.
exitPlaceEarly :: DarkRoom
exitPlaceEarly = do
  inP <- use pathPlace
  case inP of
    Nothing -> pure ()
    Just (PlaceState p _ _) -> do
      pathPlace .= Nothing
      notify ("you slip out of " <> placeName p <> ".")

-- | All scripted events for the place have fired. Mark the tile as explored
-- (so the renderer flips it to 'P'), draw a road back home, and clear the
-- place state.
finishPlace :: DarkRoom
finishPlace = do
  inP <- use pathPlace
  case inP of
    Nothing -> pure ()
    Just (PlaceState p tile _) -> do
      pathExplored %= Set.insert tile
      when (placeBecomesOutpost p) $ addRoad tile
      pathPlace .= Nothing
      notify ("you have cleared " <> placeName p <> ".")

-- | Trace a simple L-shaped road of '#' tiles from the village to the just-
-- explored place. Skips landmark tiles so we don't paint over other places.
addRoad :: (Int, Int) -> DarkRoom
addRoad target = do
  let (vx, vy) = villageTile
      (tx, ty) = target
      -- Walk horizontally first (along vy), then vertically (along tx).
      hRange = if vx <= tx then [vx .. tx] else [tx .. vx]
      vRange = if vy <= ty then [vy .. ty] else [ty .. vy]
      candidates = Set.fromList $
        [(c, vy) | c <- hRange] <> [(tx, r) | r <- vRange]
      pruned = Set.filter (passable . pathTileAt) candidates
  unless (Set.null pruned) $
    pathRoads %= Set.union pruned
  where
    passable (Just '.') = True
    passable (Just ',') = True
    passable (Just ';') = True
    passable (Just '#') = True
    passable _          = False
