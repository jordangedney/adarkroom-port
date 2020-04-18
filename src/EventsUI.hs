module EventsUI where

import Brick
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Center (centerLayer)
import Control.Lens (view, (&))

import UIState
import GameTypes
import UIComponents


drawDialogWindow :: Game -> Widget Name
drawDialogWindow game = theFurBeggar game

theFurBeggar :: Game -> Widget Name
theFurBeggar game =
  let dialogWindow =
        centerLayer $
        borderWithLabel (str (formatTitle "The Beggar" (width - 1))) $ dialogText

      width = 54

      dialogText =
        hLimit width $
        str "                                                                  ."
        <=>
        (padBottom (Pad 2) $
         padLeft (Pad 2) $
         (str "a beggar arrives."
          <=> str " "
          <=> str "asks for any spare furs to keep him warm at night."))
        <=> padLeft (Pad 2) buttons

      buttons = dialogButton NoOpButton "give 50" <+> str "    "
                <+> dialogButton NoOpButton "give 100"
                <=> str " "
                <=> padBottom (Pad 1) (dialogButton NoOpButton "turn him away")

  in if view (uiState . dialogBox) game then dialogWindow else str ""
