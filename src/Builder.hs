module Builder
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

import GameTypes (Game, BuilderState(..),
                  milestones, builderIsHelping, builderState, stored, wood, traps, carts,
                  trapsUnlocked, cartsUnlocked, preCartsUnlocked)
import GameEvent (GameEvent(BuilderUpdate, UnlockForest, BuilderGathersWood, UnlockTraps))
import Constants (builderStateDelay, needWoodDelay, builderGatherDelay, unlockTrapsDelay)

import GameUtil (notifyRoom, updateEvents)

-- Defined in GameTypes to avoid an import cycle:
-- data BuilderState
--   = Approaching
--   | Collapsed
--   | Shivering
--   | Sleeping
--   | Helping

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
  game & updateEvents BuilderGathersWood builderGatherDelay
       & over (stored . wood) (+ 2)

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

buildTrap :: Game -> Game
buildTrap game =
  let enoughWood = view (stored . wood) game > 9
      showError = game & notifyRoom "not enough wood. (10)"
      buildTrap = game & notifyRoom "more traps to catch more creatures"
                       & over (stored . wood) (subtract 10)
                       & over (stored . traps) (+ 1)
  in if enoughWood then buildTrap else showError

buildCart :: Game -> Game
buildCart game =
  let doNothing = game
  in doNothing
