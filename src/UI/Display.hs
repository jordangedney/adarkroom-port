module UI.Display where

import Brick
import Brick.Widgets.Center
import Brick.Widgets.Border
import Brick.Widgets.Border.Style

import Control.Lens
import Data.List (sortBy, intersperse)
import qualified Data.Function as Function

import Util
import GameTypes
import GameEvent (_fireStoked, _gatherWood, _checkTraps)
import UI.State
import UI.Events
import UI.Components
import Constants

import qualified Outside

roomStores :: Game -> Int -> Widget Name
roomStores = storesWindow

forestStores :: Game -> Int -> Widget Name
forestStores game width =
  let showBuildings = view (uiState . showForestBuildings) game
      getStored getter = view (stored . getter) game
      buildings' = [(name, show amount)| (name, amount, itemShouldBeShown) <-
        [ ("cart", getStored carts, getStored carts > 0)
        , ("trap", getStored traps, getStored traps > 0)
        ], itemShouldBeShown]
      currentPopulation = view (stored . people) game
      maxPopulation = Outside.maxPopulation game
      popCount = show currentPopulation <> "/" <> show maxPopulation
      buildings = ("pop", popCount) : buildings'

      buildingsWindow = vBox [ storeWidget ForestVP "forest" buildings width
                             , storesWindow game width]
  in if showBuildings then buildingsWindow else storesWindow game width

storeWidget :: Name -> String -> [(String, String)] -> Int -> Widget Name
storeWidget name label stockpileItems' width =
  let height = length stockpileItems

      stockpileItems = sortBy (compare `Function.on` fst) stockpileItems'
      linePadding = 4 -- 2 whitespace on each side
      countWhitespace (itemName, amount) =
        let strLen = width - (length itemName + length amount) - linePadding
        in (itemName, amount, strLen)

      withWhitespace (itemName, amount, len) =
        "  " <> itemName ++ replicate len ' ' ++ amount
      toDisplay =
        unlines $ map (withWhitespace . countWhitespace) stockpileItems

      showWindow = borderWithLabel (str (formatTitle label (width - 1)))
                    . center
                    . viewport name Vertical $ str toDisplay

      -- Extra padding for the border
      limitWindowSize = vLimit (height + 2) . hLimit (width + 2)
  in limitWindowSize showWindow

storesWindow :: Game -> Int -> Widget Name
storesWindow game width =
  let showStoreWindow = view (uiState . showStores) game
      getStored getter = view (stored . getter) game
      should getter = view (uiState . showItems . getter) game
      stockpileItems = [(name, show (getStored amount))|
                        (name, amount, itemShouldBeShown) <-
        [ ("wood",   wood,   showWood)
        , ("bait",   bait,   showBait)
        , ("fur",    fur,    showFur)
        , ("meat",   meat,   showMeat)
        , ("scales", scales, showScales)
        , ("teeth",  teeth,  showTeeth)
        , ("cloth",  cloth,  showCloth)
        , ("charm",  charm,  showCharm)
        ], should itemShouldBeShown]
      showNothing = str (replicate (width + 2) ' ')
      showWindow = storeWidget StoreVP "stores" stockpileItems width
  in if showStoreWindow then showWindow else showNothing

roomButtons :: Game -> Widget Name
roomButtons game =
  let fireIsOut = view fireValue game == Dead
      lightFireButton = actionButton game LightButton "light fire"
      stokeFireButton =
        buttonWithCoolDown
          game _fireStoked "stoke fire" StokeButton stokeCooldown
      fireButton = if fireIsOut then lightFireButton else stokeFireButton

      buildMenuUnlocked  = view (milestones . trapsUnlocked) game
      buildCartsUnlocked = view (milestones . cartsUnlocked) game

      fullOnTraps = view (stored . traps) game >= maximumNumberOfTraps
      trapButton = if fullOnTraps
                   then greyedButton "trap"
                   else actionButton game TrapButton "trap"
      cartIsBuilt = view (stored . carts) game > 0
      cartButton = if cartIsBuilt then greyedButton "cart"
                   else actionButton game CartButton "cart"
      buildables = if buildCartsUnlocked then trapButton <=> cartButton
                   else trapButton

      buildMenu = padTop (Pad 1) (str "build:") <=> buildables

  in if buildMenuUnlocked then fireButton <=> buildMenu else fireButton

forestButtons :: Game -> Widget Name
forestButtons game =
  let gatherWoodButton = buttonWithCoolDown game
        _gatherWood "gather wood" GatherButton gatherCooldown
      checkTrapsButton = buttonWithCoolDown game
        _checkTraps "check traps" CheckTrapsButton checkTrapsCooldown
      haveTraps = view (stored . traps) game > 0
      buttons = if haveTraps then vBox [gatherWoodButton, checkTrapsButton]
                else gatherWoodButton

  in buttons

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

locationsWindow' :: Game -> Widget Name
locationsWindow' g =
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
        else textButton g button (justifyLeftX 16 $ locationTxt locat)

      top    = if _showOutside . _uiState $ g
               then stylize RoomButton Room
                    <+> str "|   "
                    <+> stylize OutsideButton Outside
               else stylize RoomButton Room

      bottom = if _showPath . _uiState $ g
               then stylize PathButton Path
                    <+> str "|   "
                    <+> stylize ShipButton Ship
               else str ""

  in padBottom (Pad 1)
     $ vLimit 2
     $ hBox [vBox [top, bottom]]
     <=> str " "

locationsWindow :: Game -> Widget Name
locationsWindow g =
  let -- shouldShow showP label = if showP . _uiState $ g then label else ""
      shouldShow showP label = if showP . _uiState $ g then label else ""

      locationTxt Room    = if _fireValue g == Dead then "A Dark Room"
                            else "A Firelit Room"
      locationTxt Outside = shouldShow _showOutside "A Silent Forest"
      locationTxt Path    = shouldShow _showPath    "A Dusty Path"
      locationTxt Ship    = shouldShow _showShip    "An Old Starship"

      width = 31

      stylize button locat =
        if _location g == locat
        then (withAttr underlined . str $ locationTxt locat)
             <+> str (replicate (width - length (locationTxt locat)) ' ')
        else textButton g button (justifyLeftX width $ locationTxt locat)

      top    = if _showOutside . _uiState $ g
               then stylize RoomButton Room
                    <+> str "|                "
                    <+> stylize OutsideButton Outside
               else stylize RoomButton Room

      bottom = if _showPath . _uiState $ g
               then stylize PathButton Path
                    -- <+> str "|   "
                    <+> str "|                "
                    <+> stylize ShipButton Ship
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
                        , (textButton g DialogButton, "ay.")
                        , (textButton g CheatButton, "ch.")
                        , changingButton hyper HyperButton "classic." "hyper."
                        , changingButton debug PauseButton  "pause." ""
                        , changingButton debug PrevButton "prev. " ""
                        ]
      hiddenEmptyLabels = filter (("" /=) . snd) buttonsToLabels
      lengthOfLabels = (-2) + sum (map ((+2) . length . snd) hiddenEmptyLabels)
      -- This bad boy controls the whole width of the outer border.
      width = 69
      leftPadding = width - lengthOfLabels - 1

      paddingBetweenButtons = replicate (length hiddenEmptyLabels) (str "  ")
      withLabelsApplied = map (\(a, b) -> a b) hiddenEmptyLabels
      buttons = interleave [withLabelsApplied, paddingBetweenButtons]
   in padLeft (Pad leftPadding) (hBox buttons)

displayPath :: Game -> Widget Name
displayPath _game =
  vLimit 44 $
  hLimit 64 $
  borderWithLabel
    (str (formatTitle "1234567890123456789012345678901234567890123456789012345678" 60))
  -- $ vBox (replicate 59 (str  (replicate 59 '.' <> "!")) ++ [str "foo"])
  $ vBox (map str dummyMap)

locationMenu :: Game -> Widget Name
locationMenu game =
  let emptySpace = str (replicate 30 '\n')

      storeWidth = 30
      storePadding = padLeft (Pad 15)
      room = roomButtons game
             <+> storePadding (roomStores game storeWidth <=> emptySpace)
      forest = forestButtons game
             <+> storePadding (forestStores game storeWidth) <=> emptySpace
      path = displayPath game
      ship = room

      bottomMenuAtBottomOfWindow = padBottom (Pad 15)

      currentLocation =
        case view location game of
          Room    -> bottomMenuAtBottomOfWindow room
          Outside -> bottomMenuAtBottomOfWindow forest
          Path    -> path
          Ship    -> bottomMenuAtBottomOfWindow ship

  in vLimit 44 currentLocation

drawGameWindow :: Game -> Widget Name
drawGameWindow game =
  let showGameTick =
        str (if view debug game then show (view tickCount game) ++ "  " else "")
      notifications = vBox [vLimit 97 (eventsWindow game), showGameTick]

      gameActions =
        padLeft (Pad 3) (vBox [locationsWindow game, locationMenu game])
      actions = vBox [gameActions, bottomMenu game]

      outerBorder = center . hLimit 100 . vLimit 50 . withBorderStyle unicodeRounded . border
  in outerBorder (hBox [notifications , actions])

drawUI :: Game -> [Widget Name]
drawUI game = ($ game) <$> [drawDialogWindow, drawGameWindow]

dummyMap :: [String]
dummyMap = map (intersperse ' ')
  [ "....,,,,,,,,,.......;;;;;;;;;;;;;Y;;;;;;;;;;;;;;;;;.........,"
  , ",,,,,,,,,,,,......;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;.........,"
  , ",,,,,,,,,,,,......;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;.........,"
  , ",,,.,,,,,,,.....;;;;;;;;;;;;;;H;;;;;;;;;;;;;;;;;;;..........,"
  , ",,,.,,,,,,....;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;......;...,"
  , ",,,,,,........;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;......;...,"
  , ".,,,,........;;;;;;;;;;;;;;;.;;;;;B;;;;;;;;;;;;;;;......;;;;;"
  , "..,,,........;;;;;;;;;;;.....;;;;;;;;;;;;;;;;;;;;;......;;;;;"
  , "..,,,.......;;;;;;;;;;;......;;;;;;;;;;;;;;;;;;;;;........;;;"
  , "...,,,...,..;;;;;;;,,........;;;;;;;;;;;;;;;;;;;...........;;"
  , "....,,;;;;..;;;;;;,,,........;;;;;;;;;;;;;;;;;;;............."
  , "....,,Y,;;;,;;;;,,,,........;;;;;;;......;;;................."
  , "....,,,,,;;;;;;,,,,......Y;;;;;;;;;.....;;;.....,....F......."
  , "....,,,,,,;;;;;,,,,.....;;;;;;;;;;;....;;;;.................."
  , "....,,,,,,;;;;,,,,.....;;;;;;;;;;;;....;;;..................."
  , "....,,,,,,;;;;,,,,.....;;;;;;;;;.......;;;................F.."
  , "..Y...,,,,,,,;;;......;;;;;;;M;;......;;;;..;................"
  , "......,,,,,,,,;;;;....;;;;;;.........;;;;;..;................"
  , "Y.....,,,,,,,,........;;;H;.O........;;;;;....B.............."
  , "......,,,,,,,,,,,....;;;;;,..........;;;;;..................."
  , "......,,,,,,,,,,,....;;;;;,..........;;;.;..................."
  , ".....Y.,,F,,,,,,,....;;;;,,.........;;;..;..................."
  , ".........,,,,,,,,,...,,O,,,..,,.....;;O;.;..................."
  , ".........,,,,,,,,,,,..,,,,,C.,......;,;;;;..................."
  , ".........,,,,,,,,,,,....,,...,......;;;;;;O;................."
  , "Y........,,,,,,,,,,,,,,......,......;;;;;;;;.B..............."
  , ".........,,,,,,,,,,,,,,,,..,,,.......;;O;;;;................B"
  , "Y..........,,,,,,,,,,,,,,V,,,....,,..;;;;;;;................."
  , "F.......B..,,,,,,,,,,,,,,,,,.......H.....;;;;................"
  , "............,,,,,,,,,,,,,,,,H,H..........;;;;;..............B"
  , "............,,,,,,,,,,,,,,,,.;A;.........;;;;;..............."
  , "F...........,,,,,,,,,,,,,,,,P##,......V..;;;;;...S..........."
  , ".............,,,,,,,,,,,,,,,.,#,V;;...O.....................Y"
  , ".............,,,,,,,,,,,,,,,,,#,..;;........................."
  , ".Y..............,,,,,,,,,,,,,IP.............................."
  , "...Y........Y...,,,,,,,,,,,,,,,.............................."
  , "B..................,,,,H,,,,,,...H..................W........"
  , ",..................,,,,,,,,O,................................"
  , ",,..................,,,,,,,,,................................"
  , ",,..................,,,,,,,.....O............................"
  , ",,,,.................,,,,,..............Y...................."
  , ",,,,,............,,,,,,,,,.............,,...................."
  , ",,,,,............,,,,,,,...............,....................."
  , ",,,,,.Y..........,,,,,,,....................................."
  , ",,,,,............,,,,,,,.....H..O..O........................."
  , ",,,,,,.......,,,,,,,,,,.....................,,..............."
  , ",,,,,,.......,.,,,,,,,.....................,,,,Y,,,,........."
  , ",,,,,,,,,....,,,,,,,,,...................,,,,,,,,,,,........."
  , ",,,,,,,,,....,,,,,,......................,,,,,,,,,,,,,......."
  , ",,,,,,,,,Y,,,,,,Y........................,,,,,,,,,,,,,,,....."
  , ",,,,,,,,.,,,,,,......................,,,,,,,,,,,,,,,,,,,....."
  , ",,,,,,,,.,,,,,,.........H...........,,,,,,,,,,,,,,,,,,,,,,..."
  , ",,,,,,,,.,,,,,......................,,,,,,,,,,,,,,,,,,,,,,,,,"
  , ",,,,,,,,,,,,...............Y......,,,,,,,,,,,,,,,,,,,,,,,,,,,"
  , ",,,,,,,,,,....................,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,"
  , ",,,,,,,,,,...................,,,,,,,,,,,,,,,,Y,,,,,,,,,,,,,,,"
  , ",,,,,,,,,,...................,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,"
  , ",,,,,,,,,,...................,,,,,,,,,,,,,,,,,,,,,,,,,,,,..,,"
  , ",,,,,,,,.....................,,,,,,,,,H,,,,,,,,,,,,,,,,,....."
  , ",,,,,,.......................,B,,,,,,,,,,,,,,,,,,,,,,,,,....."
  , ";,,,,...............Y........,,B,,B,,,,,,,,,,,,,,,,,,,,......"
  ]
