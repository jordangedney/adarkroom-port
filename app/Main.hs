{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
module Main where

import Brick (App (..), appDraw, customMain, neverShowCursor)
import Brick.BChan (newBChan, writeBChan)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, void)
import Event (handleEventWrapper)
import Graphics.Vty (Mode (Mouse), mkVty, outputIface, setMode, standardIOConfig)
import SaveGame (load)
import Shared.Game (Game, Tick (..))
import Shared.UI (Name)
import UI.Components (theMap)
import UI.Display (drawUI)

app :: App Game Tick Name
app = App
  { appDraw         = drawUI
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = handleEventWrapper
  , appStartEvent   = return
  , appAttrMap      = const theMap }

main :: IO ()
main = do
  let buildVty = do
        v <- mkVty =<< standardIOConfig
        setMode (outputIface v) Mouse True
        return v
  initialVty <- buildVty
  g <- load
  chan <- newBChan 10
  _ <- forkIO $
    forever $ do
      writeBChan chan Tick
      threadDelay 100000 -- decides how fast your game moves; 1/10 second
  void $ customMain initialVty buildVty (Just chan) app g
