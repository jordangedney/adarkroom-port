module SaveGame where

import Data.Yaml

import GameTypes

load :: IO String
load = do
  inpStr <- readFile "input.txt"
  return inpStr

save :: Game -> IO ()
save game = do
  encodeFile "save.data" game
