module UI.RandomEvent where

import Brick
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Center (centerLayer)
import Control.Lens (view, (&))

import UI.Components

import Shared.UI
import Shared.Game
import Shared.RandomEvent (Scene(..), SceneEvent(..), SceneChoice(..))
import Shared.Util (canAfford)

drawDialogWindow :: Game -> Widget Name
drawDialogWindow game =
  case view inEvent game of
    Nothing -> str ""
    Just e  -> genericEvent e game

optionalDialogButton :: Bool -> Name -> String -> Widget Name
optionalDialogButton predicate buttonID label =
  if predicate then dialogButton buttonID label else cantAffordButton buttonID label

genericEvent :: Scene -> Game -> Widget Name
genericEvent event game =
  let width = windowSize event

      dialogText =
        text (currentScene event)
        & map str
        & vBox

      buttonMaker choice =
        let btn y = y (RandomEventButton choice) (choiceTxt choice)
        in case cost choice of
             [] -> btn dialogButton
             cs -> btn (optionalDialogButton (canAfford cs game))

      groupsOfTwo [] = error "events should at least have a leave button"
      groupsOfTwo [x] = x
      groupsOfTwo (x:y:[]) = hBox [(x <+> str "    "), y]
      groupsOfTwo (x:y:xs) =
        hBox [(x <+> str "    "), y]
        <=> str " "
        <=> groupsOfTwo xs

      buttons =
        choices (currentScene event)
        & map buttonMaker
        & groupsOfTwo

      controlDialogBoxWidth = hLimit width (str (replicate width ' '))

      dialogItems =
        controlDialogBoxWidth
        <=> (padBottom (Pad 1) dialogText <=> buttons)
        & padBottom (Pad 1)
        & padLeft (Pad 2)

  in dialogItems
     & borderWithLabel (str (formatTitle (title event) (width - 1)))
     & centerLayer
