{-# LANGUAGE ScopedTypeVariables #-}

module RandomEvents where

import System.Random (StdGen, randomR)
import Control.Lens (view, over, set, (&))

import GameTypes (Game, stored, fur, tickCount, nextRandomAt)


-- The Fur Beggar
give50fur :: Game -> Game
give50fur game =
  game & over (stored . fur) (+ (-50))


give100fur :: Game -> Game
give100fur game =
  game & over (stored . fur) (+ (-100))




shouldDoRandomEvent :: Game -> Bool
shouldDoRandomEvent game = view tickCount game == view nextRandomAt game

doRandomEvent :: StdGen -> Game -> Game
doRandomEvent randomGen game =
  let ticksPerMinute = 10 * 60
      (nextRandom :: Int, gen) =
        randomR (ticksPerMinute * 3, ticksPerMinute * 6) randomGen

  in game & set nextRandomAt (view tickCount game + nextRandom)
