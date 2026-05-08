module UI.Rewards (drawRewardsWindow) where

import Brick
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Border.Style (unicodeRounded)
import Brick.Widgets.Center (centerLayer)
import Control.Lens (view)
import qualified Data.Map as Map

import Shared.Game
import Shared.Item (Item(CuredMeat), itemToStr)
import Shared.Rewards
import Shared.UI
import UI.Components

import qualified Rewards

drawRewardsWindow :: Game -> Widget Name
drawRewardsWindow game = case view inRewards game of
  Nothing -> blank
  Just r  -> rewardsPanel game r

rewardsPanel :: Game -> Rewards -> Widget Name
rewardsPanel game r =
  let width = 56

      flavor = case lines (_rewardsText r) of
                 [] -> [str ""]
                 ls -> map str ls

      takeable = Map.toList (Map.filter (> 0) (_rewardsAvailable r))
      takeableSection = case takeable of
        [] -> str "  (nothing left to take.)"
        xs -> vBox (map (takeableRow game) xs)

      droppable = Map.toList (Map.filter (> 0) (view expeditionInventory game))
      droppableSection = case droppable of
        [] -> str "  (rucksack is empty.)"
        xs -> vBox (map droppableRow xs)

      used = Rewards.inventoryUsed game
      cap  = view (playerStats . inventoryCapacity) game
      free = Rewards.inventoryFree game
      rucksackLine =
        str ("  rucksack: " <> show used <> "/" <> show cap
              <> " (" <> show free <> " free)")

      hpLine =
        str ("  hp: " <> show (view (playerStats . hp) game)
              <> "/" <> show (view (playerStats . maxHp) game))

      anyLeft = any (> 0) (Map.elems (_rewardsAvailable r))
      takeAllBtn =
        if anyLeft && free > 0
        then dialogButton TakeAllRewardsButton "  take all  "
        else greyedButton "  take all  "

      haveMeat =
        Map.findWithDefault 0 CuredMeat (view expeditionInventory game) > 0
      atFullHp = view (playerStats . hp) game >= view (playerStats . maxHp) game
      eatMeatBtn =
        if haveMeat && not atFullHp
        then dialogButton EatMeatButton "  eat meat  "
        else greyedButton "  eat meat  "

      leaveBtn    = dialogButton LeaveRewardsButton    "   leave    "
      continueBtn = dialogButton ContinueRewardsButton "  continue  "

      contextRow = case _rewardsContext r of
        RewardsCombat      -> hBox [eatMeatBtn,    str "  ", leaveBtn]
        RewardsExploration -> hBox [continueBtn,   str "  ", leaveBtn]

      buttons = takeAllBtn <=> str " " <=> contextRow

      content = vBox
        [ padBottom (Pad 1) (vBox flavor)
        , str "takeable:"
        , padTop (Pad 1) takeableSection
        , padTop (Pad 1) (str "in rucksack:")
        , padTop (Pad 1) droppableSection
        , padTop (Pad 1) rucksackLine
        , hpLine
        , padTop (Pad 1) buttons
        ]

      panel = withBorderStyle unicodeRounded
            $ borderWithLabel (str (formatTitle "rewards" (width - 1)))
                              (padLeftRight 2 (padBottom (Pad 1) content))
   in centerLayer (hLimit width panel)

takeableRow :: Game -> (Item, Int) -> Widget Name
takeableRow game (i, n) =
  let free = Rewards.inventoryFree game
      label = "  " <> itemToStr i <> ":"
      btn = if n > 0 && free > 0
            then dialogButton (TakeRewardButton i) " [+] "
            else greyedButton " [+] "
      amount = "  " <> show n <> " left"
  in str (justifyLeftX 16 label) <+> btn <+> str amount

droppableRow :: (Item, Int) -> Widget Name
droppableRow (i, n) =
  let label = "  " <> itemToStr i <> ":"
      countCell = " " <> show n <> " "
      btn = if n > 0
            then dialogButton (DropRewardButton i) " [-] "
            else greyedButton " [-] "
  in str (justifyLeftX 16 label) <+> str countCell <+> btn
