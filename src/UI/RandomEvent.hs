module UI.RandomEvent where

import Brick
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Center (centerLayer)
import Control.Lens (view, (&))

import UI.State
import GameTypes
import UI.Components
import Data.Maybe (isJust)

drawDialogWindow :: Game -> Widget Name
drawDialogWindow = theFurBeggar

optionalDialogButton :: Bool -> Name -> String -> Widget Name
optionalDialogButton predicate buttonID label =
  if predicate then dialogButton buttonID label else greyedButton label

theFurBeggar :: Game -> Widget Name
theFurBeggar game =
  let width = 54
      dialogWindow =
        dialogItems
        & borderWithLabel (str (formatTitle "The Beggar" (width - 1)))
        & centerLayer

      blankLine = str " "
      dialogText =
        padBottom (Pad 2)
          (str "a beggar arrives." <=> blankLine
           <=> str "asks for any spare furs to keep him warm at night.")

      dialogItems =
        hLimit width $ str (replicate width ' ') -- Control dialog box width
        <=> padLeft (Pad 2 ) (dialogText <=> padBottom (Pad 1) buttons)

      giveFur amnt bttnId =
        optionalDialogButton (view (stored . fur) game >= amnt) bttnId ("give " ++ show amnt)

      buttons =
        giveFur 50 FurBeggarFiftyButton
        <+> str "    "
        <+> giveFur 100 FurBeggarHundredButton
        <=> blankLine
        <=> dialogButton ExitEventButton "turn him away"

  in if isJust (view inEvent game) then dialogWindow else str ""
