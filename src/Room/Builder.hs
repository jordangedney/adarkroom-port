module Room.Builder
  ( update
  , canHelp
  , approach
  , gatherWood
  , canBuildTraps
  , canBuildCarts
  , buildTrap
  , buildCart
  )
where

import Control.Lens (over, set, view, (&))

import Shared.UI (showForestBuildings)
import Shared.Game
import Shared.GameEvent (GameEvent(..))
import Shared.Constants

import GameUtil (notifyRoom, updateEvents)

showState :: BuilderState -> String
showState Approaching =
  "the light from the fire spills from the windows, out into the dark."
showState Collapsed   =
  "a ragged stranger stumbles through the door and collapses in the corner."
showState Shivering   =
  "the stranger shivers, and mumbles quietly. her words are unintelligible."
showState Sleeping    =
  "the stranger in the corner stops shivering. her breathing calms."
showState Helping     =
  "the stranger is standing by the fire. she says she can help. says she builds things."

builderSucc :: BuilderState -> BuilderState
builderSucc Helping =  Helping
builderSucc x = succ x

canHelp :: Game -> Game
canHelp game =
  let doNothing = game
      doHelping = game & over builderState builderSucc
                       & set (milestones . builderIsHelping) True
                       & updateEvents BuilderGathersWood builderGatherDelay
                       & updateEvents UnlockTraps unlockTrapsDelay
      builderIsSleeping = view builderState game == Sleeping
      builderIsNowHelping = doHelping & notifyRoom (showState (view builderState doHelping))
  in if builderIsSleeping then builderIsNowHelping else doNothing

approach :: Game -> Game
approach game =
  game & notifyRoom (showState (view builderState game))
       & updateEvents BuilderUpdate builderStateDelay
       & updateEvents UnlockForest needWoodDelay

update :: Game -> Game
update game =
  let doNothing = game
      builderIsFine = view builderState game == Sleeping
                    -- A bit of a hack to deal with a race condition with canHelp
                    -- (a hack because a cleaner solution would be to reshuffle this logic)
                    || view builderState game == Helping
      doBetter = game & updateEvents BuilderUpdate builderStateDelay
                      & over builderState builderSucc
      getBetter = doBetter & notifyRoom (showState (view builderState doBetter))
  in if builderIsFine then doNothing else getBetter

gatherWood :: Game -> Game
gatherWood game =
  let gatherMoreInAwhile = game & updateEvents BuilderGathersWood builderGatherDelay
      tooColdToWork = view roomTemperature game == Freezing
                    || view roomTemperature game == Cold
  in if tooColdToWork then gatherMoreInAwhile
     else gatherMoreInAwhile & over (stored . wood) (+ 2)

canBuildTraps :: Game -> Game
canBuildTraps game =
  game & set (milestones . trapsUnlocked) True
       & notifyRoom ("builder says she can make traps to catch any creatures "
                     <> "might still be alive out there.")

canBuildCarts :: Game -> Game
canBuildCarts game =
  let doNothing = game
      cartsNeedToBeUnlocked = view (milestones . preCartsUnlocked) game
                            && not (view (milestones . cartsUnlocked) game)
      unlockCarts =
        game & set (milestones . cartsUnlocked) True
             & notifyRoom "builder says she can make a cart for carrying wood"
  in if cartsNeedToBeUnlocked then unlockCarts else doNothing

buildIfWarm :: Game -> Game -> Game
buildIfWarm buildTheItem game =
  let tooColdToWork = view roomTemperature game == Freezing
                      || view roomTemperature game == Cold
      shiver = game & notifyRoom "builder just shivers."
  in if tooColdToWork then shiver else buildTheItem

buildIfEnoughWood :: Int -> Game -> Game -> Game
buildIfEnoughWood cost buildTheItem game =
  let notEnoughWood = view (stored . wood) game < cost
      showResourceError = game & notifyRoom ("not enough wood (" <> show cost <> ").")
  in if notEnoughWood then showResourceError
     else buildTheItem & over (stored . wood) (subtract cost) & unlockBuildingView

buildItem :: Int -> Game -> Game -> Game
buildItem cost item game = buildIfWarm (buildIfEnoughWood cost item game) game

unlockBuildingView :: Game -> Game
unlockBuildingView game =
  game & set (uiState . showForestBuildings) True

buildTrap :: Game -> Game
buildTrap game =
  let cost = 10 * view (stored . traps) game
      buildTheTrap = game & notifyRoom "more traps to catch more creatures."
                          & over (stored . traps) (+ 1)
      haveEnough = view (stored . traps) buildTheTrap >= maximumNumberOfTraps
      doneMakingTraps = notifyRoom "more traps won't help now."
      builtTrap = if haveEnough then buildTheTrap & doneMakingTraps else buildTheTrap
  in buildItem cost builtTrap game

buildCart :: Game -> Game
buildCart game =
  let cost = 30
      buildTheCart =
        game & notifyRoom "the rickety cart will carry more wood from the forest"
             & over (stored . carts) (+ 1)
  in buildItem cost buildTheCart game
