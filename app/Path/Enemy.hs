{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}

-- | Wandering-attack catalog for the Path. Pure data: HP, damage, drops,
-- and spawn weights for each enemy type. The combat system (separate
-- module) consumes this; nothing in here touches game state.
module Path.Enemy
  ( Enemy(..)
  , EnemyStats(..)
  , EnemyDrop(..)
  , enemyStats
  , wanderingEnemies
  , wanderingAttackChancePct
  ) where

import GHC.Generics (Generic)
import Data.Yaml (FromJSON, ToJSON)

import Shared.Item (Item(..))

-- | The seven wandering attackers from A Dark Room. The snarling beast is
-- the canonical/early encounter; the rest are reachable as the player ranges
-- further from home.
data Enemy
  = SnarlingBeast
  | GauntMan
  | Scavenger
  | MeatEater
  | FeralTerror
  | Soldier
  | Sniper
  deriving (Eq, Show, Ord, Enum, Bounded, Generic, ToJSON, FromJSON)

-- | A single drop entry. Combat resolves drops by rolling 'dropRolls' times,
-- each succeeding with 'dropChancePct' percent. Total drop = number of
-- successful rolls. Mirrors the rolls/prob shape from the original events.js.
data EnemyDrop = EnemyDrop
  { dropItem      :: Item
  , dropRolls     :: Int
  , dropChancePct :: Int
  } deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)

-- | Per-enemy parameters consumed by the combat system. 'enemyAttackDelay'
-- is in ticks (10 ticks/sec; 20 ≈ 2s, matching the original game).
data EnemyStats = EnemyStats
  { enemyName        :: String
  , enemyMaxHp       :: Int
  , enemyDamage      :: Int
  , enemyHitChance   :: Int
  , enemyAttackDelay :: Int
  , enemyDrops       :: [EnemyDrop]
  , enemySpawnWeight :: Int
  } deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)

-- | Stats for each enemy. Numbers track the original A Dark Room balance.
enemyStats :: Enemy -> EnemyStats
enemyStats = \case
  SnarlingBeast -> EnemyStats
    { enemyName        = "a snarling beast"
    , enemyMaxHp       = 5
    , enemyDamage      = 1
    , enemyHitChance   = 80
    , enemyAttackDelay = 20
    , enemyDrops =
        [ EnemyDrop Meat  5 80
        , EnemyDrop Fur   3 40
        , EnemyDrop Teeth 2 40
        ]
    , enemySpawnWeight = 5
    }
  GauntMan -> EnemyStats
    { enemyName        = "a gaunt man"
    , enemyMaxHp       = 6
    , enemyDamage      = 1
    , enemyHitChance   = 80
    , enemyAttackDelay = 20
    , enemyDrops =
        [ EnemyDrop Meat  1 50
        , EnemyDrop Cloth 2 50
        ]
    , enemySpawnWeight = 5
    }
  Scavenger -> EnemyStats
    { enemyName        = "a scavenger"
    , enemyMaxHp       = 6
    , enemyDamage      = 1
    , enemyHitChance   = 80
    , enemyAttackDelay = 20
    , enemyDrops =
        [ EnemyDrop Meat    3 50
        , EnemyDrop Cloth   3 50
        , EnemyDrop Iron    1 50
        , EnemyDrop Leather 2 50
        ]
    , enemySpawnWeight = 4
    }
  MeatEater -> EnemyStats
    { enemyName        = "a meat eater"
    , enemyMaxHp       = 8
    , enemyDamage      = 2
    , enemyHitChance   = 80
    , enemyAttackDelay = 20
    , enemyDrops =
        [ EnemyDrop Meat  3 80
        , EnemyDrop Cloth 2 50
        , EnemyDrop Fur   2 50
        , EnemyDrop Teeth 2 40
        ]
    , enemySpawnWeight = 4
    }
  FeralTerror -> EnemyStats
    { enemyName        = "a feral terror"
    , enemyMaxHp       = 18
    , enemyDamage      = 4
    , enemyHitChance   = 80
    , enemyAttackDelay = 20
    , enemyDrops =
        [ EnemyDrop Meat  5 70
        , EnemyDrop Fur   3 60
        , EnemyDrop Scale 5 50
        , EnemyDrop Teeth 4 60
        ]
    , enemySpawnWeight = 3
    }
  Soldier -> EnemyStats
    { enemyName        = "a soldier"
    , enemyMaxHp       = 20
    , enemyDamage      = 6
    , enemyHitChance   = 80
    , enemyAttackDelay = 20
    , enemyDrops =
        [ EnemyDrop Meat    2 50
        , EnemyDrop Cloth   3 50
        , EnemyDrop Rifle   1 30
        , EnemyDrop Bullets 5 50
        , EnemyDrop Iron    1 50
        , EnemyDrop Steel   1 30
        ]
    , enemySpawnWeight = 2
    }
  Sniper -> EnemyStats
    { enemyName        = "a sniper"
    , enemyMaxHp       = 30
    , enemyDamage      = 8
    , enemyHitChance   = 80
    , enemyAttackDelay = 20
    , enemyDrops =
        [ EnemyDrop Meat    2 50
        , EnemyDrop Cloth   5 50
        , EnemyDrop Rifle   1 40
        , EnemyDrop Bullets 6 60
        , EnemyDrop Iron    2 60
        , EnemyDrop Steel   2 40
        ]
    , enemySpawnWeight = 2
    }

-- | Weighted spawn table for wandering encounters. Format matches the
-- (weight, value) shape randomChoice expects in Util. Tougher enemies are
-- rarer; the combat bead may further gate by world distance.
wanderingEnemies :: [(Int, Enemy)]
wanderingEnemies =
  [ (enemySpawnWeight (enemyStats e), e) | e <- [minBound .. maxBound] ]

-- | Per-step probability (percent) that walking triggers a wandering attack.
-- ~1-in-7 matches the original game.
wanderingAttackChancePct :: Int
wanderingAttackChancePct = 14
