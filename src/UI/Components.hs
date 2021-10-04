module UI.Components where

import Brick
import Brick.Widgets.Border (border)
import qualified Brick.Widgets.ProgressBar as P
import qualified Graphics.Vty as V
import Control.Lens (view, (&))

import Shared.Game
import Shared.UI
import Shared.GameEvent

import Data.Maybe (isJust)

blank :: Widget Name
blank = str ""

emptyLine :: Widget Name
emptyLine = str (replicate 500 ' ')

emptyColumn :: Widget Name
emptyColumn = str (replicate 500 '\n')

justifyLeftX :: Int -> String -> String
justifyLeftX x s = take x $ s ++ repeat ' '

justifyCenter15 :: String -> String
justifyCenter15 s =
  let whitespace = replicate ((15 - length s) `div` 2) ' '
      newStr = whitespace ++ s ++ whitespace
  in if length newStr == 15 then newStr else newStr ++ " "

formatTitle :: String -> Int -> String
formatTitle label width =
  let titleWithWS = " " <> label <> " "
      titleLength = length titleWithWS
      endingOffset = if titleLength < width then replicate (width - titleLength) '─' else ""
  in titleWithWS <> endingOffset

buttonThatIsCooling
  :: Game -> String -> (GameEvents -> (GameEvent, Int)) -> Int -> Widget Name
buttonThatIsCooling game label cooldownGetter maxTime =
  let amountCooling =
        (game & _upcomingEvents & cooldownGetter & snd & fromIntegral)
        / fromIntegral maxTime
      blueBox = withDefAttr blueBackground . border
      progressAttrs =
        updateAttrMap (mapAttrNames
                       [ (progressBarDone, P.progressCompleteAttr)
                       , (progressBarToDo, P.progressIncompleteAttr)])
      progressbar = progressAttrs (P.progressBar (Just label) amountCooling)
  in blueBox progressbar

buttonWithCoolDown
  :: Game -> (GameEvents -> (GameEvent, Int)) -> String -> Name -> Int
  -> Widget Name
buttonWithCoolDown game cooldownTimer label button maxTime =
  if game & _upcomingEvents & cooldownTimer & isActive
  then hLimit 17 $ buttonThatIsCooling game label cooldownTimer maxTime
  else actionButton game button label

newButton :: String -> Widget Name
newButton label =
  str (justifyCenter15 label)
  & border

blueButton :: Name -> String -> Widget Name
blueButton buttonId label =
  newButton label
  & withDefAttr blueBackground
  & clickable buttonId

actionButton :: Game -> Name -> String -> Widget Name
actionButton game buttonId label =
  if isJust (view inEvent game)
  then greyedButton label
  else blueButton buttonId label

dialogButton :: Name -> String -> Widget Name
dialogButton = blueButton

greyedButton :: String -> Widget Name
greyedButton label = newButton label & withDefAttr progressBarToDo

cantAffordButton buttonId label =
  newButton label
  & withDefAttr progressBarToDo
  & clickable buttonId

textButton :: Game -> Name -> String -> Widget Name
textButton game buttonId label =
  let button = clickable buttonId (str label)
      buttonWithUnderline = withAttr underlined button
      currentlyClicked =
        case (buttonId ==) . fst <$> view (uiState . lastReportedClick) game
        of Just True -> True
           _ -> False
  in if currentlyClicked then buttonWithUnderline else button

blueBackground :: AttrName
blueBackground = attrName "blueBackground"

underlined :: AttrName
underlined = attrName "underlined"

progressBarDone :: AttrName
progressBarDone = attrName "progressBarDone"

progressBarToDo :: AttrName
progressBarToDo = attrName "progressBarToDo"

whiteText :: AttrName
whiteText = attrName "whiteText"

blueText :: AttrName
blueText = attrName "blueText"

blackText :: AttrName
blackText = attrName "blackText"

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (blueBackground, V.white `on` V.blue)
  , (underlined, fg V.white `V.withStyle` V.underline)
  , (progressBarDone, V.black `on` V.white)
  , (progressBarToDo, V.black `on` V.blue)
  , (whiteText, V.white `on` V.black)
  , (blueText, V.blue `on` V.black)
  , (blackText, V.black `on` V.black)
  ]
