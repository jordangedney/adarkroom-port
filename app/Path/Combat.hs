{-# LANGUAGE LambdaCase #-}

module Path.Combat
  ( beastEncounter
  , maybeStartEncounter
  , scheduleEncounter
  , playerAttack
  , eatCuredMeat
  , leaveCombat
  , combatTick
  , playerAttackDamage
  , attackAnimDuration
  , blackoutDuration
  )
where

import Control.Lens
import Control.Monad (when)
import Control.Monad.State (gets)
import Data.Maybe (isJust)
import qualified Data.Map as Map
import System.Random (StdGen, randomR)

import Shared.Constants (seconds)
import Shared.Game
import Shared.GameEvent (GameEvent(PathEncounter))
import Shared.Item (Item(CuredMeat, Fur, Meat, Teeth), itemToStr)
import Util (notify, updateEvent)

-- How long an attack-frame stays on screen (ticks; ~1 tick = 1/10s).
attackAnimDuration :: Int
attackAnimDuration = 5

-- Cooldown after a knockout before the player can embark again.
blackoutDuration :: Int
blackoutDuration = seconds 30

-- Encounter spawn cadence: somewhere in this window each path-tick.
encounterMinDelay, encounterMaxDelay :: Int
encounterMinDelay = seconds 8
encounterMaxDelay = seconds 20

-- Canonical snarling-beast template. All numbers are data so the
-- wandering-events catalog can swap in other enemies without touching
-- the combat loop itself.
beastEncounter :: Combat
beastEncounter = Combat
  { _enemyName        = "a snarling beast"
  , _enemyChar        = 'B'
  , _enemyHp          = 5
  , _enemyMaxHp       = 5
  , _enemyDamage      = 1
  , _enemyHitChance   = 80
  , _enemyAttackTimer = seconds 2
  , _enemyAttackDelay = seconds 2
  , _enemyDrops       = [(Fur, 2), (Meat, 1), (Teeth, 1)]
  , _playerAttackAnim = 0
  , _enemyAttackAnim  = 0
  , _combatStatus     = Fighting
  }

-- Schedule the next encounter check while embarked. Called when arriving
-- on the path (post-embark) and after each finished encounter.
scheduleEncounter :: StdGen -> DarkRoom
scheduleEncounter rnd = do
  let (delay, _) = randomR (encounterMinDelay, encounterMaxDelay) rnd
  updateEvent PathEncounter delay

-- Fired by the GameEvent dispatcher when PathEncounter ticks down to 0.
-- Spawns a beast if the player is actually walking and has no other
-- modal (combat / random event) blocking the screen.
maybeStartEncounter :: StdGen -> DarkRoom
maybeStartEncounter rnd = do
  embarkedNow <- use embarked
  inFight     <- gets (isJust . view combat)
  inAnyEvent  <- gets (isJust . view inEvent)
  here        <- use location
  alive       <- gets ((> 0) . view (playerStats . hp))
  if embarkedNow && here == Path && not inFight && not inAnyEvent && alive
    then do
      combat .= Just beastEncounter
      notify "a snarling beast lopes out of the brush."
    else
      -- Reschedule even when we skipped — avoids a permanently-armed event
      -- staying at 0 and re-firing every tick.
      scheduleEncounter rnd

-- Damage a player swing lands on the enemy.
-- TODO(ad-9iq/weapons): use strongest weapon in expedition inventory.
playerAttackDamage :: Game -> Int
playerAttackDamage _ = 1

-- Player presses Attack: damage the enemy, kick off animation.
playerAttack :: DarkRoom
playerAttack = do
  cmb <- use combat
  case cmb of
    Just c | _combatStatus c == Fighting -> do
      dmg <- gets playerAttackDamage
      let newHp = max 0 (_enemyHp c - dmg)
          c' = c { _enemyHp = newHp
                 , _playerAttackAnim = attackAnimDuration
                 }
      combat .= Just c'
      when (newHp == 0) winCombat
    _ -> pure ()

-- Player presses Eat Meat: spend 1 cured meat for +2 HP (capped at maxHp).
eatCuredMeat :: DarkRoom
eatCuredMeat = do
  cmb <- use combat
  case cmb of
    Just c | _combatStatus c == Fighting -> do
      have <- gets (Map.findWithDefault 0 CuredMeat . view expeditionInventory)
      when (have > 0) $ do
        expeditionInventory %= Map.adjust (subtract 1) CuredMeat
        maxH <- use (playerStats . maxHp)
        playerStats . hp %= \h -> min maxH (h + 2)
        notify "the meat is tough but it staves off the gnawing in your gut."
    _ -> pure ()

-- Close out a finished encounter — apply rewards (placeholder until the
-- rewards-screen bead lands), then resume walking. On a loss, blackout
-- has already been triggered by loseCombat.
leaveCombat :: StdGen -> DarkRoom
leaveCombat rnd = do
  cmb <- use combat
  case cmb of
    Just c | _combatStatus c == Won -> do
      -- Placeholder rewards: drop straight into expedition inventory.
      -- The rewards-screen bead (ad-y2h) will replace this with a
      -- proper take/leave UI.
      mapM_ (\(i, n) -> expeditionInventory %= Map.insertWith (+) i n)
            (_enemyDrops c)
      let summary = "you find: "
            <> formatDrops (_enemyDrops c)
            <> "."
      notify summary
      combat .= Nothing
      scheduleEncounter rnd
    Just c | _combatStatus c == Lost -> do
      combat .= Nothing
      -- blackout cleanup happened in loseCombat; here we just dismiss
      -- the modal so the path screen redraws.
      pure ()
    _ -> pure ()

formatDrops :: [(Item, Int)] -> String
formatDrops [] = "nothing"
formatDrops xs = listJoin [show n <> " " <> itemToStr i | (i, n) <- xs]
  where
    listJoin []        = ""
    listJoin [a]       = a
    listJoin [a, b]    = a <> " and " <> b
    listJoin (a:rest)  = a <> ", " <> listJoin rest

-- Per-tick combat update: cool down animation timers, run the enemy AI.
combatTick :: StdGen -> DarkRoom
combatTick rnd = do
  cmb <- use combat
  case cmb of
    Just c | _combatStatus c == Fighting -> do
      let c1 = c { _playerAttackAnim = max 0 (_playerAttackAnim c - 1)
                 , _enemyAttackAnim  = max 0 (_enemyAttackAnim c - 1)
                 , _enemyAttackTimer = _enemyAttackTimer c - 1
                 }
      if _enemyAttackTimer c1 <= 0
        then do
          combat .= Just (c1 { _enemyAttackTimer = _enemyAttackDelay c1 })
          enemyAttack rnd
        else combat .= Just c1
    _ -> pure ()

-- Enemy swing: roll hit chance, subtract armor, deduct from player HP.
enemyAttack :: StdGen -> DarkRoom
enemyAttack rnd = do
  cmb <- use combat
  case cmb of
    Just c | _combatStatus c == Fighting -> do
      let (roll, _) = randomR (1, 100) rnd :: (Int, StdGen)
          hits = roll <= _enemyHitChance c
      if hits
        then do
          arm <- use (playerStats . armor)
          let raw = max 0 (_enemyDamage c - arm)
          combat .= Just (c { _enemyAttackAnim = attackAnimDuration })
          playerStats . hp %= \h -> max 0 (h - raw)
          curHp <- use (playerStats . hp)
          when (curHp == 0) loseCombat
        else
          -- Missed swing still animates so the player sees activity.
          combat .= Just (c { _enemyAttackAnim = attackAnimDuration })
    _ -> pure ()

winCombat :: DarkRoom
winCombat = do
  cmb <- use combat
  case cmb of
    Just c -> do
      combat .= Just (c { _combatStatus = Won })
      notify (_enemyName c <> " falls.")
    Nothing -> pure ()

-- Player went down. Drop expedition state, send them home, start the
-- blackout cooldown. Movement bead (ad-1es) will own the actual
-- "can't embark while blacked out" guard; we just set the timer here.
loseCombat :: DarkRoom
loseCombat = do
  cmb <- use combat
  case cmb of
    Just c -> do
      combat .= Just (c { _combatStatus = Lost })
      notify "the world goes black."
      embarked .= False
      expeditionInventory .= Map.empty
      blackoutTimer .= blackoutDuration
      maxH <- use (playerStats . maxHp)
      playerStats . hp .= maxH
    Nothing -> pure ()
