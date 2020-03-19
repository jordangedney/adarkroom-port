module UI where

import           Brick
import           Brick.Widgets.Center
import           Brick.Widgets.Border
import           Brick.Widgets.Border.Style
import qualified Brick.Widgets.ProgressBar as P
import qualified Graphics.Vty as V
import Data.Bifunctor (second)

-- import Control.Lens (over, set, view, _2, (&))
import Control.Lens

import Util
import GameTypes
import GameEvent (isActive, GameEvent, GameEvents, _fireStoked, _gatherWood)
import UIState

import Constants

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

buttonThatIsCooling :: Game -> String -> (GameEvents -> (GameEvent, Int)) -> Int -> Widget Name
buttonThatIsCooling g label coolDownGetter maxTime =
  let amountCooling =
        fromIntegral (snd . coolDownGetter . _upcomingEvents $ g) / fromIntegral maxTime
  in withDefAttr blueBackground
  $ border
  $ updateAttrMap (mapAttrNames [ (progressBarDone, P.progressCompleteAttr)
                                , (progressBarToDo, P.progressIncompleteAttr)])
  $ P.progressBar (Just label) amountCooling

buttonWithCoolDown ::
  Game -> (GameEvents -> (GameEvent, Int)) -> String -> Name -> Int -> Widget Name
buttonWithCoolDown game cooldownTimer label button maxTime =
  if isActive $ cooldownTimer . _upcomingEvents $ game
  then buttonThatIsCooling game label cooldownTimer maxTime
  else blueButton button label

blueButton :: Name -> String -> Widget Name
blueButton buttonId label =
  clickable buttonId
  $ withDefAttr blueBackground
  $ border
  $ str $ justifyCenter15 label

greyedButton :: Name -> String -> Widget Name
greyedButton buttonId label =
  clickable buttonId
  $ withDefAttr blueBackground
  $ withDefAttr progressBarToDo
  $ border
  $ str $ justifyCenter15 label

textButton :: Game -> Name -> String -> Widget Name
textButton game buttonId label =
  let button = clickable buttonId (str label)
      buttonWithUnderline = withAttr underlined button
      currentlyClicked =
        case (buttonId ==) . fst <$> view (uiState . lastReportedClick) game
        of Just True -> True
           _ -> False
  in if currentlyClicked then buttonWithUnderline else button


roomActions :: Game -> Widget Name
roomActions game =
  let fireIsOut = view fireValue game == Dead
      lightFireButton = blueButton LightButton "light fire"
      stokeFireButton =
        buttonWithCoolDown game _fireStoked "stoke fire" StokeButton stokeCooldown
      fireButton = if fireIsOut then lightFireButton else stokeFireButton

      buildMenuUnlocked  = view (milestones . trapsUnlocked) game
      buildCartsUnlocked = view (milestones . cartsUnlocked) game

      fullOnTraps = view (stored . traps) game >= maximumNumberOfTraps
      trapButton = if fullOnTraps
                   then greyedButton NoOpButton "trap"
                   else blueButton TrapButton "trap"
      cartIsBuilt = view (stored . carts) game > 0
      cartButton = if cartIsBuilt then greyedButton NoOpButton "cart"
                   else blueButton CartButton "cart"
      buildables = if buildCartsUnlocked then trapButton <=> cartButton else trapButton

      buildMenu = padTop (Pad 1) (str "build:") <=> buildables

  in if buildMenuUnlocked then fireButton <=> buildMenu else fireButton

forestActions :: Game -> Widget Name
forestActions game =
  let gatherWoodButton =
        buttonWithCoolDown game _gatherWood "gather wood" GatherButton gatherCooldown
  in gatherWoodButton

actionWindow :: Game -> Widget Name
actionWindow game =
  let currentRoom =
        case view location game of
          Room    -> roomActions
          Outside -> forestActions
          Path    -> error "TODO"
          Ship    -> error "TODO"
  in padRight (Pad 3) $ vBox [currentRoom game]

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
      topEvents' = foldl (<=>) (str "") topEvents
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

      stylize button locat =
        if _location g == locat
        then (withAttr underlined . str $ locationTxt locat)
             <+> str (replicate (16 - length (locationTxt locat)) ' ')
        else textButton g button (justifyLeft16 $ locationTxt locat)

      top    = if _showOutside . _uiState $ g
               then stylize RoomButton Room <+> str "|   " <+> stylize OutsideButton Outside
               else stylize RoomButton Room

      bottom = if _showPath . _uiState $ g
               then stylize PathButton Path <+> str "|   " <+> stylize ShipButton Ship
               else str ""

  in padBottom (Pad 1)
     $ vLimit 2
     $ hBox [vBox [top, bottom]]
     <=> str " "

bottomMenu :: Game -> Widget Name
bottomMenu g =
  let changingButton state button ifTrue ifFalse =
        if view state g then (textButton g button, ifTrue)
        else (textButton g button,  ifFalse)

      buttonsToLabels = [ (textButton g RestartButton,  "restart.")
                        , (textButton g SaveButton, "save.")
                        , changingButton hyper HyperButton "classic." "hyper."
                        , changingButton debug PauseButton  "pause." ""
                        , changingButton debug PrevButton "prev." ""
                        -- , (textButton g DebugButton, "debug.")
                        ]
      hiddenEmptyLabels = filter (("" /=) . snd) buttonsToLabels
      lengthOfLabels = (-2) + sum (map ((+2) . length . snd) hiddenEmptyLabels)
      width = 45
      leftPadding = width - lengthOfLabels

      paddingBetweenButtons = replicate (length hiddenEmptyLabels) (str "  ")
      withLabelsApplied = map (\(a, b) -> a b) hiddenEmptyLabels
      buttons = interleave [withLabelsApplied, paddingBetweenButtons]
   in padLeft (Pad leftPadding) (hBox buttons)

drawUI :: Game -> [Widget Name]
drawUI g =
  [ center $ hLimit 77 $ vLimit 30
    $ withBorderStyle unicodeRounded
    $ border
    $ hBox [ vBox [ vLimit 27 (eventsWindow g)
                  , str (if view debug g then show (view tickCount g) ++ "  " else "")
                  ]
           , vBox [ padLeft (Pad 3) $ locationsWindow g
                  , padLeft (Pad 3) $ vLimit 24 (actionWindow g <+> storeWindow g
                                                 <=> str (replicate 30 '\n'))
                  , bottomMenu g]
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
