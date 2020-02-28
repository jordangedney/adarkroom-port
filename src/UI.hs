module UI where

import           Brick
import           Brick.Widgets.Center
import           Brick.Widgets.Border
import           Brick.Widgets.Border.Style
import qualified Graphics.Vty as V
import Data.Bifunctor (second)

import Util
import Game

data Name = VP1
          | VP2
          | VP3
  deriving (Eq, Show, Ord)

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
     viewport VP1 Vertical $ str toDisplay

actionWindow =
  let color = V.white `on` V.cyan
  in
  center $ padLeft (Pad 5) $
  viewport VP2 Vertical $
  border $
  str "Light Fire"

eventsWindow events = center $
  viewport VP3 Vertical $
  hLimit 30 $
  strWrap $ head events

tickWindow ticks = str $ show ticks

drawUI :: Game -> [Widget Name]
drawUI g =
  [
  center $ hLimit 77 $ vLimit 30 $
  withBorderStyle unicodeRounded $
    borderWithLabel (str $ " " <> _location g <> " ") $
      eventsWindow (_events g) <+>
      tickWindow (_tickCount  g) <+>
      actionWindow <+>
      storeWindow (_stored g)
  ]

theMap :: AttrMap
theMap = attrMap V.defAttr
  [(attrName "button1", V.blue `on` V.blue)]
