{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module UI.Display where

import Brick
import Brick.Widgets.Center
import Brick.Widgets.Border
import Brick.Widgets.Border.Style

import Control.Lens
import Data.List (sortBy, intersperse, transpose)
import qualified Data.Function as Function

import Shared.Constants
import Shared.Game
import Shared.GameEvent (_fireStoked, _gatherWood, _checkTraps)
import Shared.UI
import UI.RandomEvent
import UI.Components

import qualified Outside
import Shared.Item
import Shared.Util

interleave :: [[a]] -> [a]
interleave = concat . transpose

roomStores :: Game -> Int -> Widget Name
roomStores = storesWindow

forestStores :: Game -> Int -> Widget Name
forestStores game width =
  let showBuildings = view (uiState . showForestBuildings) game
      forestItems = [Cart, Trap]
      buildings = filter (`playerHas` game) forestItems
                & map (\i -> (itemToStr i, show (getItem i game)))
      currentPopulation = view numPeople game
      maxPopulation = Outside.maxPopulation game
      popCount = show currentPopulation <> "/" <> show maxPopulation
      gapLen =  7 - length popCount
      title = "forest" <> " " <> replicate gapLen '─' <> " pop " <> popCount

      buildingsWindow = vBox [ storeWidget ForestVP title buildings width
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
      should getter = view (uiState . getter) game
      stockpileItems = [(name, show (getItem amount game))|
                        (name, amount, itemShouldBeShown) <-
        [ ("wood",   Wood,   showWood)
        , ("bait",   Bait,   showBait)
        , ("fur",    Fur,    showFur)
        , ("meat",   Meat,   showMeat)
        , ("scales", Scale,  showScales)
        , ("teeth",  Teeth,  showTeeth)
        , ("cloth",  Cloth,  showCloth)
        , ("charm",  Charm,  showCharm)
        ], should itemShouldBeShown]
      showNothing = str (replicate (width + 2) ' ')
      showWindow = storeWidget StoreVP "stores" stockpileItems width
  in if showStoreWindow then showWindow else showNothing

craftButtons :: Game -> Widget Name
craftButtons game =
  let craftMenuUnlocked  = view (milestones . craftUnlocked) game
      craftDemo =
        str "  craft:"
        <=> hCenter (actionButton game LightButton "light fire")
  in if craftMenuUnlocked then padTop (Pad 4) craftDemo else blank

buyButtons :: Game -> Widget Name
buyButtons game =
  let buyMenuUnlocked  = view (milestones . buyUnlocked) game
      buyDemo =
        str "  buy:"
        <=> hCenter (actionButton game LightButton "light fire")
  in if buyMenuUnlocked then padTop (Pad 4) buyDemo else blank

buildButtons :: Game -> Widget Name
buildButtons g =
  let maxNumCraftable Trap = maximumNumberOfTraps -- 10
      maxNumCraftable Hut = maximumNumberOfHuts   -- 20
      maxNumCraftable _ = 1

      tooMany c = getItem c g >= maxNumCraftable c

      toBuild = filter (\i -> g ^. craftableReady i) buildables

      mkButton c = if tooMany c
                   then greyedButton (itemToStr c)
                   else actionButton g (CraftButton c) (itemToStr c)
      buttons = vBox (map mkButton toBuild)
      buildMenu = padTop (Pad 1) (str "build:") <=> buttons
  in if null toBuild then blank else buildMenu

drawRoom :: Game -> Widget Name
drawRoom game =
  let leftCol = ensureWidth (hCenter (fireButton <=> buildButtons game))
      leftMidCol = ensureWidth (craftButtons game)
      rightMidCol = ensureWidth (buyButtons game)
      rightCol = hCenter (roomStores game 23)

      ensureWidth x = hLimit 21 (x <=> emptyLine)

      fireIsOut = view fireValue game == Dead
      lightFireButton = actionButton game LightButton "light fire"
      stokeFireButton =
        buttonWithCoolDown
          game _fireStoked "stoke fire" StokeButton stokeCooldown
      fireButton = if fireIsOut then lightFireButton else stokeFireButton

  in hBox [leftCol, leftMidCol, rightMidCol, rightCol]

drawForest :: Game -> Widget Name
drawForest game =
  let leftCol = ensureWidth (hCenter buttons)
      leftMidCol = ensureWidth blank
      rightMidCol = ensureWidth blank
      rightCol = hCenter (forestStores game 23)

      ensureWidth x = hLimit 21 (x <=> emptyLine)

      gatherWoodButton = buttonWithCoolDown game
        _gatherWood "gather wood" GatherButton gatherCooldown
      checkTrapsButton = buttonWithCoolDown game
        _checkTraps "check traps" CheckTrapsButton checkTrapsCooldown
      haveTraps = getItem Trap game > 0
      buttons = if haveTraps then vBox [gatherWoodButton, checkTrapsButton]
                else gatherWoodButton

  in hBox [leftCol, leftMidCol, rightMidCol, rightCol]

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
      topEvents' = foldl (<=>) blank topEvents
      bottomEvents = unlines $ drop 9 $ map fst withWhitespace
  in hLimit 30
  $ viewport EventsVP Vertical
  $ topEvents'
  <=> withAttr blueText (strWrap bottomEvents)

locationsWindow :: Game -> Widget Name
locationsWindow game =
  let locationTxt Room    = if _fireValue game == Dead then "A Dark Room"
                            else "A Firelit Room"
      locationTxt Outside = "A Silent Forest"
      locationTxt Path    = "A Dusty Path"
      locationTxt Ship    = "An Old Starship"

      stylize button room =
        let atLocation = _location game == room
            withUnderline = withAttr underlined (str (locationTxt room))
            withoutUnderline = textButton game button (locationTxt room)
        in if atLocation then withUnderline else withoutUnderline

      showUnlocked showP button locat =
        let locationUnlocked = showP (_uiState game)
        in if locationUnlocked then str " | " <+> stylize button locat else blank

      leftCol     = padLeft (Pad 3) (stylize RoomButton Room)
      leftMidCol  = showUnlocked _showOutside OutsideButton Outside
      rightMidCol = showUnlocked _showPath PathButton Path
      rightCol    = showUnlocked _showShip ShipButton Ship

  in  padBottom (Pad 1) (hBox [leftCol, leftMidCol, rightMidCol, rightCol])

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
                        -- , changingButton debug DebugButton  "debug. " ""
                        , changingButton debug PrevButton "prev. " ""
                        ]
      hiddenEmptyLabels = filter (("" /=) . snd) buttonsToLabels
      lengthOfLabels = (-2) + sum (map ((+2) . length . snd) hiddenEmptyLabels)
      -- How far right the bottom row is
      width = 94
      leftPadding = width - lengthOfLabels - 1

      paddingBetweenButtons = replicate (length hiddenEmptyLabels) (str "  ")
      withLabelsApplied = map (\(a, b) -> a b) hiddenEmptyLabels
      buttons = interleave [withLabelsApplied, paddingBetweenButtons]
   in padLeft (Pad leftPadding) (hBox buttons)

drawPath :: Game -> Widget Name
drawPath _game =
  let gameMap = withBorderStyle unicodeRounded . border
      align = hLimit 90 . padLeft (Pad 2)
      inventoryTitle = str "rucksack"
      inventory = hCenter $ borderWithLabel inventoryTitle emptyLine
  in align (vBox [inventory, map str dummyMap & vBox & gameMap])

locationMenu :: Game -> Widget Name
locationMenu game =
  let currentLocation = game &
        case view location game of
          Room    -> drawRoom
          Outside -> drawForest
          Path    -> drawPath
          Ship    -> drawRoom
  in vLimit 43 (currentLocation <=> emptyColumn)

drawGameWindow :: Game -> Widget Name
drawGameWindow game =
  let showGameTick =
        str (if view debug game then
               show (view tickCount game) ++ "  "
                                          ++ show (view nextRandomAt game)
              else "")
      notifications = padLeft (Pad 2) (vBox [vLimit 45 (eventsWindow game), showGameTick])

      gameActions =
        padLeft (Pad 2) (vBox [locationsWindow game, locationMenu game])
      actions = vBox [gameActions, bottomMenu game]

      outerBorder = center . hLimit 130 . vLimit 50 . withBorderStyle unicodeRounded . border
  in outerBorder (padAll 1 (hBox [notifications , actions]))

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
