module SaveGame (save, load) where

import Data.Yaml (decodeFileEither, encodeFile)

import Shared.Game (Game, initGame, previousStates)
import Control.Lens (set, (&))

load :: IO Game
load = do
  loadFile <- decodeFileEither "save.data"
  case loadFile of
    Left _     -> return initGame
    Right game -> return game

save :: Game -> IO ()
save game =
  game & set previousStates []
       & encodeFile "save.data"
