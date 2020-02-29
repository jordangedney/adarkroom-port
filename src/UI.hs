module UI where

import           Brick
import           Brick.Widgets.Center
import           Brick.Widgets.Border
import           Brick.Widgets.Border.Style
import qualified Brick.Widgets.ProgressBar as P
import qualified Graphics.Vty as V
import Data.Bifunctor (second)

import Util
import Game
import UIState

storeWindow :: [(String, Int)] -> Widget Name
storeWindow stockpileItems =
  let (width, height) = (20, length stockpileItems)
      toString = second show
      countWhitespace (a, b) = (a, width - (length a + length b), b)
      withWhitespace (a, b, c) = a ++ replicate b ' ' ++ c
      toDisplay = unlines $ map (withWhitespace . countWhitespace . toString) stockpileItems
  in vLimit (height + 2) $ hLimit (width + 2) $ -- Extra padding for the border
     borderWithLabel (str " stores ") $
     center $
     viewport StoreVP Vertical $ str toDisplay

progress g =
  let bar amountDone = P.progressBar (Just $ "stoke fire") amountDone
      pBar = updateAttrMap
             (mapAttrNames [ (progressBarDone, P.progressCompleteAttr)
                           , (progressBarToDo, P.progressIncompleteAttr)
                           ]
             ) $ bar $ getAmountDone g
      getAmountDone g =
        let t = head [time | (time, event, _) <- upcomingEvents g, FireStoked == event]
        in 0.01 * fromIntegral t
      in if hasEvent FireStoked g
         then clickable StokeButton $
              withDefAttr blueBackground $
              border $
              pBar
         else str "fk"

blueButton attr text =
  clickable attr $
  withDefAttr blueBackground $
  border $
  str (justifyCenter15 text)

lightFireButton = blueButton LightButton "light fire"

hasEvent e g = length [e | (_, event, _) <- upcomingEvents g, e == event] > 0

stokeFireButton :: Game -> Widget Name
stokeFireButton g =
  if (hasEvent FireStoked g)
  then progress g
  else blueButton StokeButton "stoke fire"

stokeButton g = if fireValue g /= 0 then stokeFireButton g else lightFireButton

actionWindow g =
  let lastClicked = fst <$> lastReportedClick (uiState g)
  in padRight (Pad 3) $ vBox [stokeButton g
                             -- , progress g
                             ]

eventsWindow :: [String] -> Widget Name
eventsWindow events =
  hLimit 30 $
  viewport EventsVP Vertical $
  strWrap $ unlines $ interleave [events, replicate (length events) " "]

locationsWindow g =
  padBottom (Pad 1 ) $
  hBox [ str " "
       , withAttr underlined $ str (location g)
       , str $ " | " <> show (tickCount g) <> " "
       ]

drawUI :: Game -> [Widget Name]
drawUI g =
  [
  center $ hLimit 77 $ vLimit 30 $
  withBorderStyle unicodeRounded $
     border $
      hBox [eventsWindow (events g)
           , padLeft (Pad 3)$
             vBox [ locationsWindow g
                  ,  actionWindow g <+> storeWindow (stored g)]
           ]
  ]

blueBackground = attrName "blueBackground"
underlined = attrName "underlined"
progressBarDone = attrName "progressBarDone"
progressBarToDo = attrName "progressBarToDo"

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (blueBackground, V.white `on` V.blue)
  , (underlined, fg V.white `V.withStyle` V.underline)
  , (progressBarDone, V.black `on` V.white)
  , (progressBarToDo, V.black `on` V.blue)
  ]
