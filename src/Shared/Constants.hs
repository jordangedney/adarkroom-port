module Shared.Constants
  ( fireCoolDelay
  , roomWarmDelay
  , builderStateDelay
  , stokeCooldown
  , needWoodDelay
  , gatherCooldown
  , builderGatherDelay
  , maximumNumberOfTraps
  , maximumNumberOfHuts
  , checkTrapsCooldown
  , minutes
  , seconds
  )
where

-- checkTraps button cooldown
checkTrapsCooldown :: Int
checkTrapsCooldown = seconds 90

maximumNumberOfTraps :: Int
maximumNumberOfTraps = 10

maximumNumberOfHuts :: Int
maximumNumberOfHuts = 20

-- how often builder gathers wood
builderGatherDelay :: Int
builderGatherDelay = seconds 10

-- gatherWood button cooldown
gatherCooldown :: Int
gatherCooldown = seconds 60

-- stranger arrival -> when you need wood
needWoodDelay :: Int
needWoodDelay = seconds 45

-- fireStoke button cooldown
stokeCooldown :: Int
stokeCooldown = seconds 10

-- builder state updates
builderStateDelay :: Int
builderStateDelay = seconds 30

-- room temperature updates
roomWarmDelay :: Int
roomWarmDelay = seconds 30

-- fire stoke -> fire cools
fireCoolDelay :: Int
fireCoolDelay = minutes 5

-- ticks are 1/100 of a second, so adjust all times
minutes :: Int -> Int
minutes t = 10 * 60 * t
seconds :: Int -> Int
seconds t = 10 * t
