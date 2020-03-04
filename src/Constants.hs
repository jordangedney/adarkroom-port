module Constants
  ( fireCoolDelay
  , roomWarmDelay
  , builderStateDelay
  , stokeCooldown
  , needWoodDelay
  )
where

-- ticks are 1/100 of a second, so adjust all times
minutes :: Int -> Int
minutes t = 10 * 60 * t
seconds :: Int -> Int
seconds t = 10 * t

fireCoolDelay, roomWarmDelay, builderStateDelay, stokeCooldown, needWoodDelay :: Int
fireCoolDelay     = minutes 5  -- time after a stoke before the fire cools
roomWarmDelay     = seconds 30 -- time between room temperature updates
builderStateDelay = seconds 30 -- time between builder state updates
stokeCooldown     = seconds 10 -- cooldown to stoke the fire
needWoodDelay     = seconds 15 -- from the stranger arrival, to when you need wood