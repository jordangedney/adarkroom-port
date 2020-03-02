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
  let stockpileItems = [(a, b)| (a, b, c) <-
        [ ("wood",   _wood   . _stored $ g,  _showWood   . _showStores . _uiState $ g)
        , ("scales", _scales . _stored $ g,  _showScales . _showStores . _uiState $ g)
        ], c]
      showWindow = not . null $ stockpileItems
      (width, height) = (20, length stockpileItems)
      toString = second show
      countWhitespace (a, b) = (a, width - (length a + length b), b)

      withWhitespace (a, b, c) = a ++ replicate b ' ' ++ c
      toDisplay = unlines $ map (withWhitespace . countWhitespace . toString) stockpileItems

  in vLimit (height + 2) $ hLimit (width + 2) -- Extra padding for the border
     (if showWindow then
       borderWithLabel (str " stores ")
       $ center
       $ viewport StoreVP Vertical $ str toDisplay
     else str (replicate (width + 5) ' '))

buttonWithCoolDown g label coolDownGetter =
  withDefAttr blueBackground
  $ border
  $ updateAttrMap (mapAttrNames [ (progressBarDone, P.progressCompleteAttr)
                                , (progressBarToDo, P.progressIncompleteAttr)])
  $ P.progressBar (Just label)
                  (0.01 * fromIntegral (fst . coolDownGetter . _upcomingEvents $ g))

blueButton buttonId label =
  clickable buttonId
  $ withDefAttr blueBackground
  $ border
  $ str $ justifyCenter15 label

lightFireButton = blueButton LightButton "light fire"

stokeFireButton :: Game -> Widget Name
stokeFireButton g =
  if isActive $ _fireStoked . _upcomingEvents $ g
  then buttonWithCoolDown g "stoke fire" _fireStoked
  else blueButton StokeButton "stoke fire"

stokeButton g = if _fireValue g == Dead then lightFireButton else stokeFireButton g

actionWindow g =
  let lastClicked = fst <$> _lastReportedClick (_uiState g)
  in padRight (Pad 3) $ vBox [stokeButton g
                             ]
eventsWindow :: [(String, Int)] -> Widget Name
eventsWindow events =
  let withWhitespace = interleave [events, replicate (length events) (" ", 0)]
      withStyling = [(s, case t of
                           0 -> blackText
                           1 -> blackText
                           2 -> blueText
                           _ -> whiteText)
                    | (s, t) <- withWhitespace]
      topEvents = map (\(s, style) -> withAttr  style (strWrap s)) $ take 9 withStyling
      topEvents' = foldl1 (<=>) topEvents
      bottomEvents = unlines $ drop 9 $ map fst withWhitespace
  in hLimit 30
  $ viewport EventsVP Vertical
  $ topEvents'
  <=> withAttr blueText (strWrap bottomEvents)

locationsWindow :: Game -> Widget Name
locationsWindow g =
  let locationTxt Room = if _fireValue g == Dead then "A Dark Room" else "A Firelit Room"
  in
  padBottom (Pad 1 ) $
  hBox [ str " "
       , withAttr underlined $ str (locationTxt $ _location g)
       , str $ " | " <> show (_tickCount g) <> " "
       ]

drawUI :: Game -> [Widget Name]
drawUI g =
  [ center $ hLimit 77 $ vLimit 30
    $ withBorderStyle unicodeRounded
    $ border
    $ hBox [ eventsWindow (_events g)
           , padLeft (Pad 3)
             $ vBox [ locationsWindow g
                    , actionWindow g <+> storeWindow g]
           ]
  ]

blueBackground = attrName "blueBackground"
underlined = attrName "underlined"
progressBarDone = attrName "progressBarDone"
progressBarToDo = attrName "progressBarToDo"
whiteText = attrName "whiteText"
blueText = attrName "blueText"
blackText = attrName "blackText"

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (blueBackground, V.white `on` V.blue)
  , (underlined, fg V.white `V.withStyle` V.underline)
  , (progressBarDone, V.black `on` V.white)
  , (progressBarToDo, V.black `on` V.blue)
  , (whiteText, V.white `on` V.black)
  , (blueText, V.blue `on` V.black)
  , (blackText, V.black `on` V.black)
  ]
