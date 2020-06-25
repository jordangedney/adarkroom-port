module UI.RandomEvent where

import Brick
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Center (centerLayer)
import Control.Lens (view, (&))
import Data.List (intersperse)

import UI.State
import GameTypes
import UI.Components

import RandomEvent.Event (Scene(..), SceneEvent(..), SceneChoice(..), Item(..))

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
      sidePadding = 2

      dialogText =
        text (currentScene event)
        & intersperse "\n"
        & map str
        & vBox
        & padBottom (Pad sidePadding)

      buttonMaker choice =
        let item Fur   = fur
            item Cloth = cloth
            item Scale = scales
            item Teeth = teeth
            canAfford (i, amnt) = view (stored . item i) game >= amnt
            btn y = y (RandomEventButton choice) (choiceTxt choice)
        in case cost choice of
             Nothing -> btn dialogButton
             Just c -> btn (optionalDialogButton (canAfford c))

      mapButLast :: (a -> a) -> [a] -> [a]
      mapButLast _ [] = []
      mapButLast _ [x] = [x]
      mapButLast f (x:xs) = f x : mapButLast f xs

      mapLast :: (a -> a) -> [a] -> [a]
      mapLast _ [] = []
      mapLast f [x] = [f x]
      mapLast f (x:xs) = x : mapLast f xs

      buttons =
        choices (currentScene event)
        & map buttonMaker
        & mapButLast (<+> str "    ")
        & mapLast (<+> str (replicate sidePadding ' '))
        & hBox

      controlDialogBoxWidth = hLimit width $ str (replicate width ' ')

      dialogItems = controlDialogBoxWidth <=>
                    (padLeft (Pad 2 ) (dialogText <=> padBottom (Pad 1) buttons))

  in dialogItems
     & borderWithLabel (str (formatTitle (title event) (width - 1)))
     & centerLayer
