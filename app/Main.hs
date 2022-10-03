{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
module Main where

import Brick (App (..), appDraw, customMain, neverShowCursor)
import Brick.BChan (newBChan, writeBChan)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM.TVar (newTVar, readTVar)
import Control.Monad (forever, void)
import Control.Monad.STM (atomically)
import Event (handleEventWrapper)
import Graphics.Vty (Mode (Mouse), mkVty, outputIface, setMode, standardIOConfig)
import SaveGame (load)
import Shared.Game (Game(_hyper), Tick (..),)
import Shared.UI (Name)
import UI.Components (theMap)
import UI.Display (drawUI)

app :: App Game Tick Name
app = App
  { appDraw         = drawUI
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = undefined
  , appStartEvent   = return ()
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
  hyperEnabled <- atomically $ newTVar (_hyper g)
  _ <- forkIO $
    forever $ do
      writeBChan chan Tick
      hE <- atomically $ readTVar hyperEnabled
      -- default is 1/10 second
      let gameSpeed = if hE then 25000 else 100000
      threadDelay gameSpeed
  void $ customMain initialVty buildVty (Just chan)
           app {appHandleEvent = handleEventWrapper hyperEnabled} g
