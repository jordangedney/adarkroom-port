module Main where

import           Brick
import           Brick.BChan                    ( newBChan
                                                , writeBChan
                                                )
import qualified Graphics.Vty                  as V
import           Control.Monad                  ( forever
                                                , void
                                                )
import           Control.Concurrent             ( threadDelay
                                                , forkIO
                                                )

import           UI
import           UIState
import           Game
import           Event

app :: App Game Tick Name
app = App { appDraw         = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent  = handleEvent
          , appStartEvent   = return
          , appAttrMap      = const theMap
          }

main :: IO ()
main = do
        let buildVty = do
              v <- V.mkVty =<< V.standardIOConfig
              V.setMode (V.outputIface v) V.Mouse True
              return v
        initialVty <- buildVty
        g          <- initGame
        chan       <- newBChan 10
        forkIO $ forever $ do
                writeBChan chan Tick
                threadDelay 100000 -- decides how fast your game moves; 1/10 second

        void $ customMain initialVty buildVty (Just chan) app g
