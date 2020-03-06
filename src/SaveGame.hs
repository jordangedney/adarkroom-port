module SaveGame (save, load) where

import Data.Yaml (decodeFileEither, encodeFile)

import GameTypes (Game, initGame)

load :: IO Game
load = do
  loadFile <- decodeFileEither "save.data"
  case loadFile of
    Left _     -> return initGame
    Right game -> return game

save :: Game -> IO ()
save = encodeFile "save.data"
