{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Path.Combat
  ( BeastSpec(..)
  , combatTick
  , attackBeast
  , shootBeast
  , tangleBeast
  , lobAtBeast
  , bayonetBeast
  , useMeds
  , eatMeat
  , claimRewards
  , wakeUp
  , beginCombat
  , snarlingBeast
  , snarlingBeastSpec
  , gauntManSpec
  , scavengerSpec
  , meatEaterSpec
  , feralTerrorSpec
  , soldierSpec
  , sniperSpec
  , bestMeleeDamage
  , hasShoot
  , hasTangle
  , hasLob
  , hasBayonet
  ) where

import GHC.Generics (Generic)
import Data.Yaml (FromJSON, ToJSON)
import Control.Lens
import Control.Monad (forM_, when, unless)
import Control.Monad.State (gets)
import qualified Data.Map.Strict as Map
import System.Random (StdGen, randomR)

import Shared.Game
import Shared.Item (Item(..), itemToStr)
import Util (notify)

-- Reusable definition for a wandering encounter. Other wandering events
-- (gaunt man, scavenger, ...) supply their own BeastSpec.
data BeastSpec = BeastSpec
  { _bsName        :: String
  , _bsMaxHp       :: Int
  , _bsAttack      :: (Int, Int)
  , _bsDrops       :: [(Item, (Int, Int))]
  , _bsArt         :: [String]
  , _bsVictoryText :: [String]
  , _bsDefeatText  :: [String]
  } deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)

makeLenses ''BeastSpec

animFrames :: Int
animFrames = 3

attackCooldownMax :: Int
attackCooldownMax = 5

healAmount :: Int
healAmount = 8

beginCombat :: BeastSpec -> DarkRoom
beginCombat spec = inCombat .= Just (fromSpec spec)

fromSpec :: BeastSpec -> Combat
fromSpec spec = Combat
  { _enemyName      = view bsName spec
  , _enemyHp        = view bsMaxHp spec
  , _enemyMaxHp     = view bsMaxHp spec
  , _enemyAttack    = view bsAttack spec
  , _enemyArt       = view bsArt spec
  , _enemyDrops     = view bsDrops spec
  , _victoryText    = view bsVictoryText spec
  , _defeatText     = view bsDefeatText spec
  , _combatState    = CombatActive
  , _playerAtkAnim  = 0
  , _enemyAtkAnim   = 0
  , _attackCooldown = 0
  }

-- Decrement animation timers and cooldowns each tick.
combatTick :: DarkRoom
combatTick = do
  mc <- use inCombat
  forM_ mc $ \_ -> do
    inCombat . _Just . playerAtkAnim  %= max 0 . subtract 1
    inCombat . _Just . enemyAtkAnim   %= max 0 . subtract 1
    inCombat . _Just . attackCooldown %= max 0 . subtract 1

-- | Damage from the player's best melee weapon in stores. A fist (no weapon)
-- does 2; every weapon tier above it adds to that.
bestMeleeDamage :: Game -> Int
bestMeleeDamage g
  | held SteelSword = 7
  | held IronSword  = 5
  | held BoneSpear  = 3
  | otherwise       = 2
  where held i = Map.findWithDefault 0 i (g ^. stored) > 0

-- | Weapons stay in stores (they're equipment, not consumables); ammo and
-- single-use ordnance live in the expedition inventory so the player can
-- choose how much to take along.
hasShoot :: Game -> Bool
hasShoot g = inStores Rifle g > 0 && inExpedition Bullets g > 0

hasTangle :: Game -> Bool
hasTangle g = inExpedition Bolas g > 0

hasLob :: Game -> Bool
hasLob g = inExpedition Grenades g > 0

hasBayonet :: Game -> Bool
hasBayonet g = inStores Bayonet g > 0

inExpedition :: Item -> Game -> Int
inExpedition i g = Map.findWithDefault 0 i (g ^. expeditionInventory)

inStores :: Item -> Game -> Int
inStores i g = Map.findWithDefault 0 i (g ^. stored)

attackBeast :: StdGen -> DarkRoom
attackBeast rng = do
  mc <- use inCombat
  forM_ mc $ \c ->
    when (view combatState c == CombatActive
          && view attackCooldown c <= 0) $ do
      playerDmg <- gets bestMeleeDamage
      bonus <- gets (\g -> if hasBayonet g then 2 else 0)
      let enemyHpRemaining = max 0 (view enemyHp c - (playerDmg + bonus))

      inCombat . _Just . enemyHp .= enemyHpRemaining
      inCombat . _Just . playerAtkAnim .= animFrames
      inCombat . _Just . attackCooldown .= attackCooldownMax

      if enemyHpRemaining <= 0
        then victoryTransition
        else enemyCounter rng c

-- | Rifle attack: ranged, consumes one bullet, does heavy damage without
-- giving the enemy a counter-swing (the rifle button hits and the player
-- backsteps in the same tick).
shootBeast :: StdGen -> DarkRoom
shootBeast _ = do
  mc <- use inCombat
  forM_ mc $ \c -> when (view combatState c == CombatActive
                        && view attackCooldown c <= 0) $ do
    canShoot <- gets hasShoot
    when canShoot $ do
      expeditionInventory %= Map.adjust (subtract 1) Bullets
      let enemyHpRemaining = max 0 (view enemyHp c - 6)
      inCombat . _Just . enemyHp .= enemyHpRemaining
      inCombat . _Just . playerAtkAnim .= animFrames
      inCombat . _Just . attackCooldown .= attackCooldownMax
      notify "the rifle barks once."
      when (enemyHpRemaining <= 0) victoryTransition

-- | Throw bolas: tangle the enemy so they skip a counter and lose their
-- own ramp-up time. Consumes one bolas.
tangleBeast :: StdGen -> DarkRoom
tangleBeast _ = do
  mc <- use inCombat
  forM_ mc $ \c -> when (view combatState c == CombatActive
                        && view attackCooldown c <= 0) $ do
    canTangle <- gets hasTangle
    when canTangle $ do
      expeditionInventory %= Map.adjust (subtract 1) Bolas
      let enemyHpRemaining = max 0 (view enemyHp c - 1)
      inCombat . _Just . enemyHp .= enemyHpRemaining
      inCombat . _Just . playerAtkAnim .= animFrames
      inCombat . _Just . attackCooldown .= attackCooldownMax
      notify "the bolas wraps tight. the beast staggers."
      when (enemyHpRemaining <= 0) victoryTransition

-- | Lob a grenade: heavy damage, consumes one grenade.
lobAtBeast :: StdGen -> DarkRoom
lobAtBeast rng = do
  mc <- use inCombat
  forM_ mc $ \c -> when (view combatState c == CombatActive
                        && view attackCooldown c <= 0) $ do
    canLob <- gets hasLob
    when canLob $ do
      expeditionInventory %= Map.adjust (subtract 1) Grenades
      let (extra, _) = randomR (0 :: Int, 4) rng
          enemyHpRemaining = max 0 (view enemyHp c - (8 + extra))
      inCombat . _Just . enemyHp .= enemyHpRemaining
      inCombat . _Just . playerAtkAnim .= animFrames
      inCombat . _Just . attackCooldown .= attackCooldownMax
      notify "the grenade lands true. dust and limbs."
      if enemyHpRemaining <= 0
        then victoryTransition
        else enemyCounter rng c

-- | Bayonet thrust: alternative melee that always hits for a flat 5 even if
-- the player has no other weapon. Consumes nothing (the bayonet stays on
-- the rifle).
bayonetBeast :: StdGen -> DarkRoom
bayonetBeast rng = do
  mc <- use inCombat
  forM_ mc $ \c -> when (view combatState c == CombatActive
                        && view attackCooldown c <= 0) $ do
    canBayo <- gets hasBayonet
    when canBayo $ do
      let enemyHpRemaining = max 0 (view enemyHp c - 5)
      inCombat . _Just . enemyHp .= enemyHpRemaining
      inCombat . _Just . playerAtkAnim .= animFrames
      inCombat . _Just . attackCooldown .= attackCooldownMax
      notify "the bayonet bites deep."
      if enemyHpRemaining <= 0
        then victoryTransition
        else enemyCounter rng c

-- | Use medicine. Heals the player by a large fixed amount, costs one
-- medicine vial. Works in or out of combat (the rucksack on the path also
-- exposes this button).
useMeds :: DarkRoom
useMeds = do
  have <- gets (Map.findWithDefault 0 Medicine . view expeditionInventory)
  when (have > 0) $ do
    expeditionInventory %= Map.adjust (subtract 1) Medicine
    curHp <- use (playerStats . hp)
    mx    <- use (playerStats . maxHp)
    playerStats . hp .= min mx (curHp + 20)
    notify "the medicine burns going down. the world steadies."

enemyCounter :: StdGen -> Combat -> DarkRoom
enemyCounter rng c = do
  let (lo, hi) = view enemyAttack c
      (raw, _) = randomR (lo, hi) rng
  curHp <- use (playerStats . hp)
  let newHp = max 0 (curHp - raw)
  playerStats . hp .= newHp
  inCombat . _Just . enemyAtkAnim .= animFrames
  when (newHp <= 0) defeatTransition

victoryTransition :: DarkRoom
victoryTransition = do
  inCombat . _Just . combatState .= CombatVictory
  mc <- use inCombat
  forM_ mc $ \c -> notify (view enemyName c ++ " falls.")

defeatTransition :: DarkRoom
defeatTransition = do
  inCombat . _Just . combatState .= CombatDefeat
  mc <- use inCombat
  forM_ mc $ \c -> notify ("the " ++ view enemyName c ++ " overpowers you.")

-- Heal using cured meat from the expedition inventory; no-op if none.
eatMeat :: DarkRoom
eatMeat = do
  mc <- use inCombat
  forM_ mc $ \c -> when (view combatState c == CombatActive) $ do
    have <- gets (Map.findWithDefault 0 CuredMeat . view expeditionInventory)
    when (have > 0) $ do
      expeditionInventory %= Map.adjust (subtract 1) CuredMeat
      curHp <- use (playerStats . hp)
      mx    <- use (playerStats . maxHp)
      playerStats . hp .= min mx (curHp + healAmount)
      notify "the cured meat steadies you."

-- Placeholder for the rewards screen (separate bead). Drops the beast's
-- inventory directly into the expedition rucksack and ends combat.
claimRewards :: StdGen -> DarkRoom
claimRewards rng = do
  mc <- use inCombat
  forM_ mc $ \c -> when (view combatState c == CombatVictory) $ do
    forM_ (view enemyDrops c) $ \(i, (lo, hi)) -> do
      let (n, _) = randomR (lo, hi) rng
      unless (n <= 0) $ do
        expeditionInventory %= Map.insertWith (+) i n
        notify ("you take " ++ show n ++ " " ++ itemToStr i ++ ".")
    inCombat .= Nothing

-- Placeholder blackout (Movement bead will replace with the canonical
-- blackout state). Drop expedition inventory, end the expedition, and
-- restore the player to a sliver of HP so they can keep playing.
wakeUp :: DarkRoom
wakeUp = do
  mc <- use inCombat
  forM_ mc $ \_ -> do
    inCombat .= Nothing
    embarked .= False
    expeditionInventory .= Map.empty
    mx <- use (playerStats . maxHp)
    playerStats . hp .= max 1 (mx `div` 2)
    notify "you stagger back, vision swimming."

-- Canonical encounter for the path. Other wandering events reuse the
-- combat machinery by supplying a different BeastSpec.
snarlingBeast :: BeastSpec
snarlingBeast = BeastSpec
  { _bsName  = "snarling beast"
  , _bsMaxHp = 12
  , _bsAttack = (1, 3)
  , _bsDrops = [ (Fur, (1, 3)), (Meat, (1, 3)), (Teeth, (0, 1)) ]
  , _bsArt =
      [ "    ,_,    "
      , " o.-/ )-,  "
      , "  /; /'^^' "
      , "   `;      "
      ]
  , _bsVictoryText =
      [ "the beast slumps into the dust."
      , "you pick over what's left."
      ]
  , _bsDefeatText =
      [ "the beast tears at you and the world goes dark."
      ]
  }

-- | Place encounters reuse the combat machinery with empty drops — loot is
-- presented by the surrounding place's loot scene, not by the combat itself.
-- Stats are pulled from 'Path.Enemy' so the catalog stays the source of truth.
placeFoeSpec
  :: String -> Int -> (Int, Int) -> [String] -> BeastSpec
placeFoeSpec foeName foeHp foeAttack foeArt = BeastSpec
  { _bsName        = foeName
  , _bsMaxHp       = foeHp
  , _bsAttack      = foeAttack
  , _bsDrops       = []
  , _bsArt         = foeArt
  , _bsVictoryText = [ "the " <> foeName <> " collapses." ]
  , _bsDefeatText  = [ "the " <> foeName <> " overpowers you." ]
  }

snarlingBeastSpec :: BeastSpec
snarlingBeastSpec = snarlingBeast

gauntManSpec :: BeastSpec
gauntManSpec = placeFoeSpec "gaunt man" 8 (1, 2)
  [ "    o      "
  , "   /|\\     "
  , "   / \\     "
  , "           "
  ]

scavengerSpec :: BeastSpec
scavengerSpec = placeFoeSpec "scavenger" 10 (1, 3)
  [ "   <o>     "
  , "   /|\\     "
  , "   / \\     "
  , "           "
  ]

meatEaterSpec :: BeastSpec
meatEaterSpec = placeFoeSpec "meat eater" 14 (2, 4)
  [ "   _.-.    "
  , "  ( o o)   "
  , "  /  >|    "
  , "   `~~     "
  ]

feralTerrorSpec :: BeastSpec
feralTerrorSpec = placeFoeSpec "feral terror" 22 (3, 5)
  [ "  /\\_/\\    "
  , " ( O O)    "
  , "  >v<      "
  , "  /| |\\    "
  ]

soldierSpec :: BeastSpec
soldierSpec = placeFoeSpec "soldier" 24 (4, 6)
  [ "    ()     "
  , "   /||=    "
  , "   /\\      "
  , "           "
  ]

sniperSpec :: BeastSpec
sniperSpec = placeFoeSpec "sniper" 30 (5, 8)
  [ "   ___     "
  , "  ( o)==-  "
  , "   /|      "
  , "   / \\     "
  ]
