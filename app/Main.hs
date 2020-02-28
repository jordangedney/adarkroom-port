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
        let buildVty = V.mkVty V.defaultConfig
        initialVty <- buildVty
        g          <- initGame
        chan       <- newBChan 10
        forkIO $ forever $ do
                writeBChan chan Tick
                threadDelay 1000000 -- decides how fast your game moves; one second

        void $ customMain initialVty buildVty (Just chan) app g
