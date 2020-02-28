module Main where

import           Lib
import           Brick
import           Brick.Widgets.Center
import           Brick.Widgets.Border
import           Brick.Widgets.Border.Style
import qualified Graphics.Vty as V
import Data.Bifunctor (second)

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
  withAttr color
  str "Light Fire"

eventsWindow = center $
  viewport VP3 Vertical $
  hLimit 30 $
  strWrap dummy

ui :: String -> Widget Name
ui location =
  center $ hLimit 77 $ vLimit 30 $
  withBorderStyle unicodeRounded $
    borderWithLabel (str $ " " <> location <> " ") $
      eventsWindow <+>
      actionWindow <+>
      storeWindow dummyItems

main :: IO ()
main = simpleMain $ ui "A Dark Room"

-- DummyData
dummy = "the stranger is standing by the fire. she says she can help. says she builds things"

dummyItems = [("wood", 10)
             ,("scales", 150)
             ]


-- Util
count :: Eq a => a -> [a] -> Int
count x = length . filter (x==)
