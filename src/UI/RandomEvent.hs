module UI.RandomEvent where

import Brick
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Center (centerLayer)
import Control.Lens (view, (&))
import Data.List (intersperse)

import UI.State
import GameTypes
import UI.Components

import RandomEvent.EventType (RandomEventChoice(..), RandomEvent(..))
import RandomEvent.Event (getEvent, Scene(..), SceneEvent(..), SceneChoice(..), Item(..))

drawDialogWindow :: Game -> Widget Name
drawDialogWindow game =
  case view inEvent game of
    Nothing -> str ""
    Just e  -> genericEvent e game

optionalDialogButton :: Bool -> Name -> String -> Widget Name
optionalDialogButton predicate buttonID label =
  if predicate then dialogButton buttonID label else greyedButton label

genericEvent :: Scene -> Game -> Widget Name
genericEvent event game =
  let width = 54
      dialogText =
        text (currentScene event)
        & intersperse "\n"
        & map str
        & vBox
        & padBottom (Pad 2)

      buttonMaker choice =
        let item Fur   = fur
            item Cloth = cloth
            item Scale = scales
            item Teeth = teeth
            canAfford (i, amnt) = view (stored . item i) game >= amnt
            btn y = y (RandomEventButton (uiID choice)) (choiceTxt choice)
        in case cost choice of
             Nothing -> btn dialogButton
             Just c -> btn (optionalDialogButton (canAfford c))

      buttons =
        choices (currentScene event)
        & map buttonMaker
        & map (<+> str "    ")
        & hBox
        -- & vBox

      controlDialogBoxWidth = hLimit width $ str (replicate width ' ')

      dialogItems = controlDialogBoxWidth <=>
                    (padLeft (Pad 2 ) (dialogText <=> padBottom (Pad 1) buttons))

  in dialogItems
     & borderWithLabel (str (formatTitle (title event) (width - 1)))
     & centerLayer


-- theFurBeggar' :: Game -> Widget Name
-- theFurBeggar' game =
--   let width = 54
--       dialogWindow =
--         dialogItems
--         & borderWithLabel (str (formatTitle "The Beggar" (width - 1)))
--         & centerLayer
--
--       blankLine = str " "
--       dialogText =
--         padBottom (Pad 2)
--           (str "a beggar arrives." <=> blankLine
--            <=> str "asks for any spare furs to keep him warm at night.")
--
--       dialogItems =
--         hLimit width $ str (replicate width ' ') -- Control dialog box width
--         <=> padLeft (Pad 2 ) (dialogText <=> padBottom (Pad 1) buttons)
--
--       giveFur amnt bttnId =
--         optionalDialogButton (view (stored . fur) game >= amnt) bttnId ("give " ++ show amnt)
--
--       buttons =
--         giveFur 50 (RandomEventButton FurBeggarFifty)
--         <+> str "    "
--         <+> giveFur 100 (RandomEventButton FurBeggarHundred)
--         <=> blankLine
--         <=> dialogButton ExitEventButton "turn him away"
--
--   in dialogWindow
