{-# LANGUAGE ScopedTypeVariables #-}

module RandomEvent.Handler where

import System.Random (StdGen, randomR)
import Control.Lens (view, over, set, (&))

import GameTypes (Game, stored, fur, tickCount, nextRandomAt)
import RandomEvent.Event (SceneChoice)


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

      -- availableEvents = [e | e <- allRandomEvents, (isAvailable e game)]

  in game & set nextRandomAt (view tickCount game + nextRandom)

handleButton :: StdGen -> SceneChoice -> Game -> Game
handleButton random x game =
  game & over (stored . fur) (subtract 50)

-- handleButton random FurBeggarHundred g = g & over (stored . fur) (subtract 100)
