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
import RandomEvent.Event (getEvent, Scene(..), SceneEvent(..), SceneChoice(..))

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

      scene = currentScene event

      dialogWindow =
        dialogItems
        & borderWithLabel (str (formatTitle (title event) (width - 1)))
        & centerLayer

      blankLine = str " "
      dialogText =
        text scene
        & intersperse "\n"
        & map str
        & vBox
        & padBottom (Pad 2)

      dialogItems =
        hLimit width $ str (replicate width ' ') -- Control dialog box width
        <=> padLeft (Pad 2 ) (dialogText <=> padBottom (Pad 1) buttons)

      giveFur amnt bttnId =
        optionalDialogButton (view (stored . fur) game >= amnt) bttnId ("give " ++ show amnt)

      buttons =
        choices scene
        & map (\x -> case cost x of
                  -- Nothing -> dialogButton (RandomEventButton  (uiID x)) (choiceTxt x)
                  Nothing -> dialogButton ExitEventButton  (choiceTxt x)
                  Just (_, y) -> giveFur y (RandomEventButton (uiID x)))
        & vBox
      -- buttons =
      --   giveFur 50 (RandomEventButton FurBeggarFifty)
      --   <+> str "    "
      --   <+> giveFur 100 (RandomEventButton FurBeggarHundred)
      --   <=> blankLine
      --   <=> dialogButton ExitEventButton "turn him away"

  in dialogWindow


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
