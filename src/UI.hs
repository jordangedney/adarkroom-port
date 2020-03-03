module UI where

import           Brick
import           Brick.Widgets.Center
import           Brick.Widgets.Border
import           Brick.Widgets.Border.Style
import qualified Brick.Widgets.ProgressBar as P
import qualified Graphics.Vty as V
import Data.Bifunctor (second)

import Util
import GameTypes
import Game (getTime, isActive)
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
      toDisplay =
        unlines $ map (withWhitespace . countWhitespace . toString) stockpileItems

  in vLimit (height + 2) $ hLimit (width + 2) -- Extra padding for the border
     (if showWindow then
       borderWithLabel (str " stores ")
       $ center
       $ viewport StoreVP Vertical $ str toDisplay
     else str (replicate (width + 5) ' '))

buttonWithCoolDown :: Game -> String -> (GameEvents -> GameEvent) -> Widget n
buttonWithCoolDown g label coolDownGetter =
  withDefAttr blueBackground
  $ border
  $ updateAttrMap (mapAttrNames [ (progressBarDone, P.progressCompleteAttr)
                                , (progressBarToDo, P.progressIncompleteAttr)])
  $ P.progressBar (Just label)
                  (0.01 * fromIntegral
                          (getTime . coolDownGetter . _upcomingEvents $ g))

blueButton :: Name -> String -> Widget Name
blueButton buttonId label =
  clickable buttonId
  $ withDefAttr blueBackground
  $ border
  $ str $ justifyCenter15 label

lightFireButton :: Widget Name
lightFireButton = blueButton LightButton "light fire"

stokeFireButton :: Game -> Widget Name
stokeFireButton g =
  if isActive $ _fireStoked . _upcomingEvents $ g
  then buttonWithCoolDown g "stoke fire" _fireStoked
  else blueButton StokeButton "stoke fire"

stokeButton :: Game -> Widget Name
stokeButton g = if _fireValue g == Dead then lightFireButton else stokeFireButton g

actionWindow :: Game -> Widget Name
actionWindow g = padRight (Pad 3) $ vBox [stokeButton g
                             ]
eventsWindow :: Game -> Widget Name
eventsWindow g =
  let events' = _events g
      withWhitespace = interleave [events', replicate (length events') (" ", 0)]
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
  let shouldShow showP label = if showP . _uiState $ g then label else ""

      locationTxt Room    = if _fireValue g == Dead then "A Dark Room"
                            else "A Firelit Room"
      locationTxt Outside = shouldShow _showOutside "A Silent Forest"
      locationTxt Path    = shouldShow _showPath    "A Dusty Path"
      locationTxt Ship    = shouldShow _showShip    "An Old Starship"

      stylize locat = if _location g == locat
                    then (withAttr underlined . str $ locationTxt locat)
                         <+> str (replicate (16 - length (locationTxt locat)) ' ')
                    else str $ justifyLeft16 $ locationTxt locat

      top    = if _showOutside . _uiState $ g
               then stylize Room <+> str "|   " <+> stylize Outside
               else stylize Room

      bottom = if _showPath . _uiState $ g
               then stylize Path <+> str "|   " <+> stylize Ship
               else str ""

  in padBottom (Pad 1)
     $ vLimit 2
     $ hBox [vBox [top, bottom]]
     <=> str " "

bottomMenu :: Game -> Widget Name
bottomMenu _g =
  hBox [ str "debug.  "
       , str "save.  "
       , str "hyper.  "
       ]

drawUI :: Game -> [Widget Name]
drawUI g =
  [ center $ hLimit 77 $ vLimit 30
    $ withBorderStyle unicodeRounded
    $ border
    $ hBox [ eventsWindow g
           , padLeft (Pad 3)
             $ vBox [ locationsWindow g
                    , vLimit 24 (actionWindow g <+> storeWindow g
                                <=> str (replicate 30 '\n'))
                    , bottomMenu g
                    ]
           ]
  ]

blueBackground, underlined, progressBarDone, progressBarToDo, whiteText :: AttrName
blueText, blackText :: AttrName
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
