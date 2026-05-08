module UI.Rewards (drawRewardsWindow) where

import Brick
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Border.Style (unicodeRounded)
import Brick.Widgets.Center (centerLayer)
import Control.Lens (view)
import qualified Data.Map as Map

import UI.Components

import Shared.Game
import Shared.Item (Item(CuredMeat), itemToStr)
import Shared.Rewards
import Shared.UI
import qualified Path

drawRewardsWindow :: Game -> Widget Name
drawRewardsWindow game = case view rewards game of
  Nothing -> str ""
  Just r  -> rewardsView game r

rewardsView :: Game -> RewardsScreen -> Widget Name
rewardsView game r =
  let width = 60

      titleText  = view rewardsTitle r
      bodyLines  = vBox (map str (view rewardsText r))
      itemsMap   = view rewardsItems r
      canEat     = view rewardsCanEat r
      meatOnHand = Map.findWithDefault 0 CuredMeat (view expeditionInventory game)
      curHp      = view (playerStats . hp) game
      maxHpVal   = view (playerStats . maxHp) game
      exitLabel  = case view rewardsExit r of
        LeaveExit    -> "leave"
        ContinueExit -> "continue"

      free       = Path.inventoryFree game
      cap        = view (playerStats . inventoryCapacity) game
      used       = Path.inventoryUsed game

      visibleItems = [(i, n) | (i, n) <- Map.toList itemsMap, n > 0]

      itemRow (i, n) =
        let label    = justifyLeftX 14 ("  " <> itemToStr i)
            countStr = "x" <> show n
            countCell = str (justifyLeftX 6 countStr)
            takeBtn  = if free > 0
              then clickable (TakeItemButton i)
                     (withDefAttr blueBackground (str " [take] "))
              else withDefAttr progressBarToDo (str " [take] ")
        in str label <+> countCell <+> takeBtn

      itemRows = case visibleItems of
        [] -> str "  (nothing left to take.)"
        is -> vBox (map itemRow is)

      total = sum (map snd visibleItems)

      droppable =
        [ (i, n) | (i, n) <- Map.toList (view expeditionInventory game), n > 0 ]

      dropRow (i, n) =
        let label    = justifyLeftX 14 ("  " <> itemToStr i)
            countStr = "x" <> show n
            countCell = str (justifyLeftX 6 countStr)
            dropBtn  = clickable (DropItemButton i)
                         (withDefAttr blueBackground (str " [drop] "))
        in str label <+> countCell <+> dropBtn

      dropRows = case droppable of
        [] -> str "  (rucksack is empty.)"
        is -> vBox (map dropRow is)

      mkBtn name label enabled =
        if enabled
        then clickable name (withDefAttr blueBackground (str (" [" <> label <> "] ")))
        else withDefAttr progressBarToDo (str (" [" <> label <> "] "))

      takeAllBtn       = mkBtn TakeAllButton "take all"
                           (total > 0 && total <= free)
      takeAllYouCanBtn = mkBtn TakeAllYouCanButton "take all you can"
                           (total > 0 && free > 0)
      eatBtn           = mkBtn EatMeatButton "eat meat"
                           (canEat && meatOnHand > 0 && curHp < maxHpVal)
      exitBtn          = mkBtn LeaveRewardsButton exitLabel True

      hpLine = "  hp: " <> show curHp <> "/" <> show maxHpVal
      rucksackLine = "  rucksack: " <> show used <> "/" <> show cap
                     <> " (" <> show free <> " free)"

      eatLine =
        if canEat
        then padTop (Pad 1) (hBox [eatBtn, str ("    cured meat on hand: "
                                                <> show meatOnHand)])
        else blank

      content = vBox
        [ padBottom (Pad 1) bodyLines
        , str "drops:"
        , padTop (Pad 1) itemRows
        , padTop (Pad 1) (str hpLine)
        , str rucksackLine
        , padTop (Pad 1) (str "rucksack:")
        , padTop (Pad 1) dropRows
        , padTop (Pad 1) (hBox [takeAllBtn, str "  ", takeAllYouCanBtn])
        , eatLine
        , padTop (Pad 1) exitBtn
        ]

      panel = withBorderStyle unicodeRounded
            $ borderWithLabel (str (formatTitle titleText (width - 1)))
                              (padLeftRight 2 content)
  in centerLayer (hLimit width panel)
