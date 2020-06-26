module UI.RandomEvent where

import Brick
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Center (centerLayer)
import Control.Lens (view, (&))
import Data.List (intersperse)

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
  if predicate then dialogButton buttonID label else greyedButton label

genericEvent :: Scene -> Game -> Widget Name
genericEvent event game =
  let width = windowSize event
      sidePadding = 2

      dialogText =
        text (currentScene event)
        & intersperse "\n"
        & map str
        & vBox
        & padBottom (Pad sidePadding)

      buttonMaker choice =
        let btn y = y (RandomEventButton choice) (choiceTxt choice)
        in case cost choice of
             Nothing -> btn dialogButton
             Just c -> btn (optionalDialogButton (canAfford c game))

      mapButLast :: (a -> a) -> [a] -> [a]
      mapButLast _ [] = []
      mapButLast _ [x] = [x]
      mapButLast f (x:xs) = f x : mapButLast f xs

      applyLast :: (a -> a) -> [a] -> [a]
      applyLast _ [] = []
      applyLast f [x] = [f x]
      applyLast f (x:xs) = x : applyLast f xs

      buttons =
        choices (currentScene event)
        & map buttonMaker
        & mapButLast (<+> str "    ")
        & applyLast (<+> str (replicate sidePadding ' '))
        & hBox

      controlDialogBoxWidth = hLimit width (str (replicate width ' '))

      dialogItems = controlDialogBoxWidth <=>
                    padLeft (Pad sidePadding) (dialogText <=> padBottom (Pad 1) buttons)

  in dialogItems
     & borderWithLabel (str (formatTitle (title event) (width - 1)))
     & centerLayer
