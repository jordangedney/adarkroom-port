module Main where

import Control.Monad (forever, void)
import Control.Concurrent (threadDelay, forkIO)
import Brick (appDraw, App(..), neverShowCursor, customMain)
import Brick.BChan (newBChan, writeBChan)
import Graphics.Vty (mkVty, standardIOConfig, setMode, outputIface, Mode(Mouse))

import           SaveGame (load)
import           UI.Display (drawUI)
import           UI.Components (theMap)
import           Shared.UI (Name)
import           Shared.Game (Tick(..), initGame, Game)
import           Event (handleEventWrapper)


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
  g          <- load
  chan       <- newBChan 10
  forkIO $ forever $ do
          writeBChan chan Tick
          threadDelay 100000 -- decides how fast your game moves; 1/10 second

  void $ customMain initialVty buildVty (Just chan) app g
