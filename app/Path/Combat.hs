{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Path.Combat
  ( BeastSpec(..)
  , combatTick
  , attackBeast
  , eatMeat
  , claimRewards
  , wakeUp
  , beginCombat
  , snarlingBeast
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

attackBeast :: StdGen -> DarkRoom
attackBeast rng = do
  mc <- use inCombat
  forM_ mc $ \c ->
    when (view combatState c == CombatActive
          && view attackCooldown c <= 0) $ do
      let playerDmg = 2  -- TODO: derive from equipped weapon when weapons land on path
          enemyHpRemaining = max 0 (view enemyHp c - playerDmg)

      inCombat . _Just . enemyHp .= enemyHpRemaining
      inCombat . _Just . playerAtkAnim .= animFrames
      inCombat . _Just . attackCooldown .= attackCooldownMax

      if enemyHpRemaining <= 0
        then victoryTransition
        else enemyCounter rng c

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
