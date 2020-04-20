module EventsUI where

import Brick
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Center (centerLayer)
import Control.Lens (view, (&))

import UIState
import GameTypes
import UIComponents

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

      buttons = optionalDialogButton False NoOpButton "give 50"
                <+> str "    "
                <+> dialogButton NoOpButton "give 100" <=> blankLine
                <=> dialogButton ExitEventButton "turn him away"

  in if view (uiState . dialogBox) game then dialogWindow else str ""
