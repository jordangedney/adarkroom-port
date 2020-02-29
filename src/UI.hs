module UI where

import           Brick
import           Brick.Widgets.Center
import           Brick.Widgets.Border
import           Brick.Widgets.Border.Style
import qualified Graphics.Vty as V
import Data.Bifunctor (second)

import Util (interleave)
import Game
import UIState

storeWindow :: [(String, Int)] -> Widget Name
storeWindow stockpileItems =
  let (width, height) = (20, length stockpileItems)
      toString = second show
      countWhitespace (a, b) = (a, width - (length a + length b), b)
      withWhitespace (a, b, c) = a ++ replicate b ' ' ++ c
      toDisplay = unlines $ map (withWhitespace . countWhitespace . toString) stockpileItems
  in vLimit (height + 2) $ hLimit (width + 2) $ -- Extra padding for the border
     borderWithLabel (str " stores ") $
     center $
     viewport StoreVP Vertical $ str toDisplay

fireButton' lastClicked =
  viewport StokeFireButton Vertical $
  clickable StokeFireButton  $
  withDefAttr button1 $
  border $
  if lastClicked == Just StokeFireButton then str "stoke fire" else str "light fire"

fireButton lastClicked =
  viewport StokeFireButton Vertical $
  clickable StokeFireButton  $
  withDefAttr button1 $
  border $
  if lastClicked == Just StokeFireButton
  then str (justifyCenter15 "turn him away")
  else str (justifyCenter15 "light fire")

justifyCenter15 str =
  let whitespace = replicate ((15 - length str) `div` 2) ' '
      newStr = whitespace ++ str ++ whitespace
  in if length newStr == 15 then newStr else newStr ++ " "

actionWindow g =
  let lastClicked = fst <$> _lastReportedClick (_uiState g)
  in center $ padLeft (Pad 5) $
     fireButton lastClicked

eventsWindow :: [String] -> Widget Name
eventsWindow events = center $
  viewport EventsVP Vertical $
  hLimit 30 $
  strWrap $ unlines $ interleave [events, replicate (length events) " "]

drawUI :: Game -> [Widget Name]
drawUI g =
  [
  center $ hLimit 77 $ vLimit 30 $
  withBorderStyle unicodeRounded $
    borderWithLabel (str $ " " <> _location g <> " | " <> (show (_tickCount g)) <> " ") $
      padTop (Pad 1) $
      eventsWindow (_events g) <+>
      actionWindow g <+>
      storeWindow (_stored g)
  ]

button1 :: AttrName
button1 = attrName "button1"

theMap :: AttrMap
theMap = attrMap V.defAttr
  [(button1, V.white `on` V.blue)]
