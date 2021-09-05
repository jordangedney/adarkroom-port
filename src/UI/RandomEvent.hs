module UI.RandomEvent where

import Brick
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Center (centerLayer)
import Control.Lens (view, (&))

import UI.State
import GameTypes
import UI.Components

import RandomEvent.Event (Scene(..), SceneEvent(..), SceneChoice(..))
import RandomEvent.Handler (canAfford)


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
             Nothing -> btn dialogButton
             Just c -> btn (optionalDialogButton (canAfford c game))

      groupsOfTwo [] = str ""
      groupsOfTwo [x] = x
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
