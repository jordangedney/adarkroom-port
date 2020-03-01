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

storeWindow :: Game -> Widget Name
storeWindow g =
  let stockpileItems =
        [ ("wood", _wood . _stored $ g)
        , ("scales", _scales . _stored $ g)
        ]
      (width, height) = (20, length stockpileItems)
      toString = second show
      countWhitespace (a, b) = (a, width - (length a + length b), b)

      withWhitespace (a, b, c) = a ++ replicate b ' ' ++ c
      toDisplay = unlines $ map (withWhitespace . countWhitespace . toString) stockpileItems

  in vLimit (height + 2) $ hLimit (width + 2) $ -- Extra padding for the border
     borderWithLabel (str " stores ") $
     center $
     viewport StoreVP Vertical $ str toDisplay

progress g =
  let bar = P.progressBar (Just "stoke fire")
      pBar = updateAttrMap
             (mapAttrNames [ (progressBarDone, P.progressCompleteAttr)
                           , (progressBarToDo, P.progressIncompleteAttr)
                           ]
             ) $ bar $ getAmountDone g
      getAmountDone g =
        let t = fst . _fireStoked . _upcomingEvents $ g
        in 0.01 * fromIntegral t
      in if isActive $ _fireStoked . _upcomingEvents $ g
         then clickable StokeButton $
              withDefAttr blueBackground $
              border pBar
         else str "fk"

blueButton attr text =
  clickable attr $
  withDefAttr blueBackground $
  border $
  str (justifyCenter15 text)

lightFireButton = blueButton LightButton "light fire"

-- hasEvent e g = not . null $ [e | (_, event, _) <- _upcomingEvents g, e == event]

stokeFireButton :: Game -> Widget Name
stokeFireButton g =
  if isActive $ _fireStoked . _upcomingEvents $ g
  then progress g
  else blueButton StokeButton "stoke fire"

stokeButton g = if _fireValue g == Dead then lightFireButton else stokeFireButton g

actionWindow g =
  let lastClicked = fst <$> _lastReportedClick (_uiState g)
  in padRight (Pad 3) $ vBox [stokeButton g
                             -- , progress g
                             ]

eventsWindow :: [String] -> Widget Name
eventsWindow events =
  hLimit 30 $
  viewport EventsVP Vertical $
  strWrap $ unlines $ interleave [events, replicate (length events) " "]

locationsWindow :: Game -> Widget Name
locationsWindow g =
  padBottom (Pad 1 ) $
  hBox [ str " "
       , withAttr underlined $ str (_location g)
       , str $ " | " <> show (_tickCount g) <> " "
       ]

drawUI :: Game -> [Widget Name]
drawUI g =
  [
  center $ hLimit 77 $ vLimit 30 $
  withBorderStyle unicodeRounded $
     border $
      hBox [eventsWindow (_events g)
           , padLeft (Pad 3)$
             vBox [ locationsWindow g
                  , actionWindow g <+> storeWindow g]
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
