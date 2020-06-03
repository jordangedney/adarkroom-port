module RandomEvents where

import System.Random (StdGen)
import Control.Lens (view, over, (&))

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
  game
