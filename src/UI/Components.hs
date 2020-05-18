module UI.Components where

import Brick
import Brick.Widgets.Border (border)
import qualified Brick.Widgets.ProgressBar as P
import qualified Graphics.Vty as V
import Control.Lens (view, (&))

import GameTypes
import UI.State
import GameEvent

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

buttonThatIsCooling :: Game -> String -> (GameEvents -> (GameEvent, Int)) -> Int -> Widget Name
buttonThatIsCooling g label coolDownGetter maxTime =
  let amountCooling =
        fromIntegral (snd . coolDownGetter . _upcomingEvents $ g) / fromIntegral maxTime
  in withDefAttr blueBackground
  $ border
  $ updateAttrMap (mapAttrNames [ (progressBarDone, P.progressCompleteAttr)
                                , (progressBarToDo, P.progressIncompleteAttr)])
  $ P.progressBar (Just label) amountCooling

buttonWithCoolDown ::
  Game -> (GameEvents -> (GameEvent, Int)) -> String -> Name -> Int -> Widget Name
buttonWithCoolDown game cooldownTimer label button maxTime =
  if isActive $ cooldownTimer . _upcomingEvents $ game
  then buttonThatIsCooling game label cooldownTimer maxTime
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
  if view (uiState . dialogBox) game then greyedButton label else blueButton buttonId label

dialogButton :: Name -> String -> Widget Name
dialogButton = blueButton

greyedButton :: String -> Widget Name
greyedButton label = newButton label & withDefAttr progressBarToDo

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
