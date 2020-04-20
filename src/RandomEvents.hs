module RandomEvents where

import Control.Lens (over, (&))
import GameTypes (Game, stored, fur)

-- The Fur Beggar
give50fur :: Game -> Game
give50fur game =
  game & over (stored . fur) (+ (-50))

give100fur :: Game -> Game
give100fur game =
  game & over (stored . fur) (+ (-100))
