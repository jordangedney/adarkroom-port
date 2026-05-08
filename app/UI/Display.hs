{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
module UI.Display where

import Brick
import Brick.Widgets.Center
import Brick.Widgets.Border
import Brick.Widgets.Border.Style

import Control.Lens
import Data.List (sortBy, intersperse, transpose)
import qualified Data.Function as Function
import Numeric (showFFloat)

import Shared.Constants
import Shared.Game
import Shared.GameEvent (GameEvent(FireStoked, GatherWood, CheckTraps, Random))
import Shared.UI
import Shared.Worker (Worker(..))
import UI.RandomEvent
import UI.Combat (drawCombatWindow)
import UI.Rewards (drawRewardsWindow)
import UI.Components

import qualified Room.Room as Room
import qualified Outside
import qualified Path
import Shared.Item
import Shared.Util
import qualified Data.Map as Map
import Control.Monad.Reader (asks)
import Data.Maybe (isJust)

interleave :: [[a]] -> [a]
interleave = concat . transpose

roomStores :: Int -> Game -> Widget Name
roomStores = storesWindow

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

formatStores :: [Item] -> Game -> [(String, String)]
formatStores toDisplay =
  asks (Map.toList . view stored)
  >>= (\allItems -> pure [(itemToStr i, show amt) | (i, amt) <- allItems, i `elem` toDisplay])

forestStores :: Int -> Game -> Widget Name
forestStores width = do
  haveBuildings <- asks (view (uiState . showForestBuildings))
  goodsWindow <- storesWindow width
  villagers <- villagersWindow width

  if haveBuildings then do
    pop <- asks (show . getItem People)
    maxP <- asks (show . Room.maxPopulation)
    let popAmt = pop <> "/" <> maxP
        gapLen =  7 - length popAmt
        title = "forest" <> " " <> replicate gapLen '─' <> " pop " <> popAmt
    items <- formatStores buildings
    pure $ vBox [ storeWidget ForestVP title items width, villagers, goodsWindow]
  else pure goodsWindow

-- | Compact rate string for a worker, e.g. "+0.5 fur, +0.5 meat".
-- Drops the "0" before the decimal to save horizontal room.
formatWorkerRates :: Worker -> String
formatWorkerRates w =
  let entries = Outside.workerReward w
      fmtAmt a
        | a < 0     = "-" <> trimZero (negate a)
        | otherwise = "+" <> trimZero a
      trimZero a
        | a == fromIntegral (truncate a :: Int) = show (truncate a :: Int)
        | a > 0 && a < 1 = drop 1 (show a)        -- "0.5" -> ".5"
        | otherwise = show a
      fmtEntry (i, a) = fmtAmt a <> " " <> itemToStr i
  in case entries of
       [] -> ""
       xs -> foldr1 (\a b -> a <> ", " <> b) (map fmtEntry xs)

villagersWindow :: Int -> Game -> Widget Name
villagersWindow width game =
  let totalWorkers = sum (Map.elems (game ^. workers))
      hasLodge = getItem Lodge game > 0

      nameWidth = 11
      countWidth = 3

      pad :: Int -> String -> String
      pad n s = s ++ replicate (max 0 (n - length s)) ' '

      padR :: Int -> String -> String
      padR n s = replicate (max 0 (n - length s)) ' ' ++ s

      mkSimpleRow label w =
        let count = Map.findWithDefault 0 w (game ^. workers)
        in str ("  " <> pad nameWidth label <> padR countWidth (show count))

      mkAssignableRow label w =
        let count = Map.findWithDefault 0 w (game ^. workers)
            gathererCount = Map.findWithDefault 0 Gatherer (game ^. workers)
            decBtn = if count > 0
                     then smallButton (DecWorker w) "-"
                     else greyedSmallButton "-"
            incBtn = if gathererCount > 0
                     then smallButton (IncWorker w) "+"
                     else greyedSmallButton "+"
            firstLine =
              str ("  " <> pad nameWidth label <> padR countWidth (show count) <> " ")
                <+> decBtn <+> incBtn
            rateLine =
              str ("    " <> formatWorkerRates w)
        in firstLine <=> rateLine

      gathererRow = [ mkSimpleRow "gatherer" Gatherer | totalWorkers > 0 ]
      lodgeRows
        | hasLodge = [ mkAssignableRow "hunter" Hunter
                     , mkAssignableRow "trapper" Trapper
                     ]
        | otherwise = []
      tanneryRows = [ mkSimpleRow "tanner" Tanner | getItem Tannery game > 0 ]
      smokehouseRows = [ mkSimpleRow "charcutier" Charcutier | getItem Smokehouse game > 0 ]

      allRows = gathererRow ++ lodgeRows ++ tanneryRows ++ smokehouseRows
      height = sum [ if hasLodge then 4 else 0  -- 2 lines per assignable row
                   , length gathererRow
                   , length tanneryRows
                   , length smokehouseRows
                   ]

      windowBody =
        borderWithLabel (str (formatTitle "villagers" (width - 1)))
        . viewport WorkersVP Vertical
        $ vBox allRows
  in if null allRows
     then str (replicate (width + 2) ' ')
     else vLimit (height + 2) (hLimit (width + 2) windowBody)

storesWindow :: Int -> Game -> Widget Name
storesWindow width = do
  showStoreWindow <- asks (view (uiState . showStores))
  if showStoreWindow then do
    stockpileItems <- formatStores goods
    pure $ storeWidget StoreVP "stores" stockpileItems width
  else pure $ str (replicate (width + 2) ' ')

craftButtons :: Game -> Widget Name
craftButtons game =
  let craftMenuUnlocked   = view (milestones . craftUnlocked) game
      weaponsMenuUnlocked = view (milestones . weaponsUnlocked) game

      mkCraftButton item =
        actionButton game (CraftButton item) (itemToStr item)

      craftItems  = [Torch, Waterskin, Rucksack, LeatherArmor, BoneSpear]
      weaponItems = [IronSword, SteelSword, Rifle]

      craftSection =
        padTop (Pad 1) (str "craft:")
        <=> vBox (map mkCraftButton craftItems)
      weaponsSection =
        padTop (Pad 1) (str "weapons:")
        <=> vBox (map mkCraftButton weaponItems)

      menu = case (craftMenuUnlocked, weaponsMenuUnlocked) of
        (True, True)  -> craftSection <=> weaponsSection
        (True, False) -> craftSection
        _             -> blank
  in menu

buyButtons :: Game -> Widget Name
buyButtons game =
  let buyMenuUnlocked = view (milestones . buyUnlocked) game

      -- scales unlock once you've seen them
      buyables =
        [ Scale | playerHasSeen Scale game ] ++
        [ Teeth, Compass ]

      mkButton i
        | i == Compass && getItem Compass game > 0 =
            greyedButton (itemToStr i)
        | otherwise = actionButton game (BuyButton i) (itemToStr i)

      buyMenu =
        str "  buy:"
        <=> vBox (map mkButton buyables)
  in if buyMenuUnlocked then padTop (Pad 4) buyMenu else blank

buildButtons :: Game -> Widget Name
buildButtons g =
  let maxNumCraftable Trap = maximumNumberOfTraps -- 10
      maxNumCraftable Hut = maximumNumberOfHuts   -- 20
      maxNumCraftable _ = 1

      tooMany c = getItem c g >= maxNumCraftable c

      toBuild = filter (flip Map.member (g ^. (uiState . showItemButton))) buildings

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
      rightCol = hCenter (vBox [villagersWindow 23 game, roomStores 23 game])

      ensureWidth x = hLimit 21 (x <=> emptyLine)

      fireIsOut = view fireState game == Dead
      lightFireButton = actionButton game LightButton "light fire"
      stokeFireButton =
        buttonWithCoolDown
          game FireStoked "stoke fire" StokeButton stokeCooldown
      fireButton = if fireIsOut then lightFireButton else stokeFireButton

  in hBox [leftCol, leftMidCol, rightMidCol, rightCol]

drawForest :: Game -> Widget Name
drawForest game =
  let leftCol = ensureWidth (hCenter buttons)
      leftMidCol = ensureWidth blank
      rightMidCol = ensureWidth blank
      rightCol = hCenter (forestStores 23 game)

      ensureWidth x = hLimit 21 (x <=> emptyLine)

      gatherWoodButton = buttonWithCoolDown game
        GatherWood "gather wood" GatherButton gatherCooldown
      checkTrapsButton = buttonWithCoolDown game
        CheckTraps "check traps" CheckTrapsButton checkTrapsCooldown
      haveTraps = getItem Trap game > 0
      buttons = if haveTraps then vBox [gatherWoodButton, checkTrapsButton]
                else gatherWoodButton

  in hBox [leftCol, leftMidCol, rightMidCol, rightCol]

eventsWindow :: Game -> Widget Name
eventsWindow g =
  let events' = _notifications g
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
  let locationTxt Room    = if _fireState game == Dead then "A Dark Room"
                            else "A Firelit Room"
      locationTxt Outside = forestText
      locationTxt Path    = "A Dusty Path"
      locationTxt Ship    = "An Old Starship"

      hts = Map.findWithDefault 0 Hut (game ^. stored)
      forestText | hts == 0  = "A Silent Forest"
                 | hts == 1  = "A Lonely Hut"
                 | hts <= 4  = "A Tiny Village"
                 | hts <= 8  = "A Modest Village"
                 | hts <= 14 = "A Large Village"
                 | otherwise = "A Raucous Village"

      stylize button room =
        let atLocation = _location game == room
            withUnderline = withAttr underlined (str (locationTxt room))
            withoutUnderline = textButton game button (locationTxt room)
        in if atLocation then withUnderline else withoutUnderline

      showUnlocked showP button locat =
        let locationUnlocked = showP (_uiState game)
        in if locationUnlocked then str " | " <+> stylize button locat else blank

      pathUnlocked ui = _showPath ui || getItem Compass game > 0

      leftCol     = padLeft (Pad 3) (stylize RoomButton Room)
      leftMidCol  = showUnlocked _showOutside OutsideButton Outside
      rightMidCol = showUnlocked pathUnlocked PathButton Path
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
                        , (textButton g DebugRewardsButton, "rw.")
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
drawPath game =
  if view embarked game then drawPathMap game else drawPathSupplies game

drawPathMap :: Game -> Widget Name
drawPathMap game =
  let (px, py) = view pathPlayer game

      placePlayer rowIdx line
        | rowIdx == py && px >= 0 && px < length line =
            take px line ++ "@" ++ drop (px + 1) line
        | otherwise = line

      mapRows = zipWith placePlayer [0..] Path.pathMapData
      withSpacing = map (intersperse ' ') mapRows
      gameMap = withBorderStyle unicodeRounded . border . vBox
              $ map str withSpacing
      align = hLimit 130 . padLeft (Pad 2)

      water = view pathWater game
      food  = Map.findWithDefault 0 CuredMeat (view expeditionInventory game)
      hpV   = view (playerStats . hp) game
      mxHp  = view (playerStats . maxHp) game
      stats = "  hp: " <> show hpV <> "/" <> show mxHp
            <> "    water: " <> show water
            <> "    " <> itemToStr CuredMeat <> ": " <> show food
            <> "    (hjkl/arrows to walk, A to head home)"
      statusBar = withBorderStyle unicodeRounded
                $ borderWithLabel (str "rucksack") (str stats)
      -- Manual fight trigger until map walking auto-spawns encounters;
      -- the combat machinery is already wired up, so this button just
      -- proves the loop end-to-end.
      fightBtn = hCenter (actionButton game StartBeastFightButton "fight a beast")
  in align (vBox [statusBar, gameMap, padTop (Pad 1) fightBtn])

-- Supply allocation screen shown after the dusty path is unlocked but
-- before the player embarks. Lets the player divvy out path-allocatable
-- items (cured meat, torches, ...) into a fresh expedition inventory.
drawPathSupplies :: Game -> Widget Name
drawPathSupplies game =
  let armorVal = view (playerStats . armor) game
      waterVal = view (playerStats . waterCapacity) game
      capVal   = view (playerStats . inventoryCapacity) game
      used     = Path.inventoryUsed game
      free     = Path.inventoryFree game
      bo       = view blackoutCooldown game

      statsLine label val =
        str (justifyLeftX 12 (label <> ":")) <+> str (show val)

      visibleSupplies =
        [ i | i <- Path.pathSupplies
            , getItem i game > 0 || Path.allocated i game > 0
        ]

      supplyRows = case visibleSupplies of
        [] -> str "  (no supplies to take yet — craft some first.)"
        is -> vBox (map (supplyRow game) is)

      embarkBtn
        | bo > 0    = greyedButton "embark"
        | otherwise = actionButton game EmbarkButton "embark"

      recovery = if bo > 0
        then padTop (Pad 1)
              (hCenter (str ("recovering... " <> show ((bo + 9) `div` 10) <> "s")))
        else blank

      content = vBox
        [ padBottom (Pad 1) (statsLine "armor" armorVal)
        , padBottom (Pad 1) (statsLine "water" waterVal)
        , str "supplies:"
        , padTop (Pad 1) supplyRows
        , padTop (Pad 1) (str ("  rucksack: " <> show used <> "/" <> show capVal
                               <> " (" <> show free <> " free)"))
        , padTop (Pad 1) (hCenter embarkBtn)
        , recovery
        ]

      panel = withBorderStyle unicodeRounded
            $ borderWithLabel (str "a dusty path") (padLeftRight 2 content)
  in hLimit 90 (padLeft (Pad 2) panel)

supplyRow :: Game -> Item -> Widget Name
supplyRow game i =
  let avail = Path.available i game
      have  = Path.allocated i game
      free  = Path.inventoryFree game
      modalActive = isJust (view inEvent game) || isJust (view inCombat game) || isJust (view inRewards game)

      mkBtn n btnLabel enabled =
        if enabled && not modalActive
        then clickable n (withDefAttr blueBackground (str btnLabel))
        else withDefAttr progressBarToDo (str btnLabel)

      decBtn = mkBtn (DecreaseSupplyButton i) " [-] " (have > 0)
      incBtn = mkBtn (IncreaseSupplyButton i) " [+] " (avail > 0 && free > 0)

      rowLabel = justifyLeftX 14 ("  " <> itemToStr i <> ":")
      countCell = str (justifyLeftX 4 (" " <> show have <> " "))
      availCell = str ("  " <> show avail <> " available")
  in str rowLabel <+> decBtn <+> countCell <+> incBtn <+> availCell

locationMenu :: Game -> Widget Name
locationMenu game =
  let currentLocation = game &
        case view location game of
          Room    -> drawRoom
          Outside -> drawForest
          Path    -> drawPath
          Ship    -> drawRoom
  in vLimit 40 (currentLocation <=> emptyColumn)

drawGameWindow :: Game -> Widget Name
drawGameWindow game =
  let showGameTick =
        str (if view debug game then
               show (view tickCount game) ++ "  "
                                          ++ show ((_upcomingEvents game) Map.! Random)
              else "")
      notificationsWindow = padLeft (Pad 2) (vBox [vLimit 45 (eventsWindow game), showGameTick])

      gameActions =
        padLeft (Pad 2) (vBox [locationsWindow game, locationMenu game])
      actions = vBox [gameActions, gatheringPanel game, bottomMenu game]

      outerBorder = center . hLimit 130 . vLimit 50 . withBorderStyle unicodeRounded . border
  in outerBorder (padAll 1 (hBox [notificationsWindow , actions]))

-- Always-on display of per-second worker production rates. Hidden until
-- there's at least one assigned worker, since the panel would otherwise be
-- empty in the opening Dark Room phase before villagers arrive.
gatheringPanel :: Game -> Widget Name
gatheringPanel g =
  let totalWorkers = sum (Map.elems (g ^. workers))
      perTick = Outside.workerProductionRates g
      perSec = sortBy (compare `Function.on` fst)
        [ (i, r / fromIntegral workerIncomeSeconds)
        | (i, r) <- perTick, r /= 0 ]

      formatRate (i, r) =
        let sign = if r >= 0 then "+" else ""
            num  = showFFloat (Just 1) r ""
        in itemToStr i <> " " <> sign <> num <> "/s"

      rateStr   = unwords (map formatRate perSec)
      rateLine  = if null perSec then "no production" else rateStr
      content   = padLeftRight 2 (str rateLine)
      labelText = "gathering"
      panel     = withBorderStyle unicodeRounded
                $ borderWithLabel (str labelText) content
  in if totalWorkers <= 0 then blank else padLeftRight 2 panel

drawUI :: Game -> [Widget Name]
drawUI game = ($ game) <$> [drawRewardsWindow, drawCombatWindow, drawDialogWindow, drawGameWindow]
