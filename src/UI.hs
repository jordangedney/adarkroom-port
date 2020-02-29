module UI where

import           Brick
import           Brick.Widgets.Center
import           Brick.Widgets.Border
import           Brick.Widgets.Border.Style
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

blueButton attr text =
  clickable attr $
  withDefAttr blueBackground $
  border $ str (justifyCenter15 text)

lightFireButton = blueButton LightButton "light fire"
stokeFireButton = blueButton StokeButton "stoke fire"

stokeButton fireLit = if fireLit then stokeFireButton else lightFireButton

actionWindow g =
  let lastClicked = fst <$> _lastReportedClick (uiState g)
  in padRight (Pad 6) $ vBox [stokeButton (fireValue g /= 0)]

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
           , vBox [ locationsWindow g
                  ,  actionWindow g <+> storeWindow (stored g)]
           ]
  ]

blueBackground = attrName "blueBackground"
underlined = attrName "underlined"

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (blueBackground, V.white `on` V.blue)
  , (underlined, fg V.white `V.withStyle` V.underline)
  ]
