module SaveGame (save, load) where

import Data.Yaml (decodeFileEither, encodeFile)
import System.Random (newStdGen)

import Control.Lens (set, (&))

import GameTypes (World(..), Game, initGame, previousStates)

load :: IO World
load = do
  loadFile <- decodeFileEither "save.data"
  stdGen <- newStdGen
  case loadFile of
    Left _     -> return $ World initGame stdGen
    Right game -> return $ World game stdGen

save :: Game -> IO ()
save game =
  game & set previousStates []
       & encodeFile "save.data"
