{-# LANGUAGE OverloadedStrings #-}

module SaveGame where

import Data.Yaml

import GameTypes

load :: IO Game
load = do
  loadFile <-  decodeFile "save.data"
  case loadFile of
    Just g -> return g
    Nothing -> return initGame

save :: Game -> IO ()
save game = do
  encodeFile "save.data" game
