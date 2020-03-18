module Builder
  ( update
  , canHelp
  , approach
  , gatherWood
  )
where

import Control.Lens (over, set, view, (&))

import GameTypes (Game, BuilderState(..),
                  milestones, builderIsHelping, builderState, stored, wood)
import GameEvent (GameEvent(BuilderUpdate, UnlockForest, BuilderGathersWood))
import Constants (builderStateDelay, needWoodDelay, builderGatherDelay)

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
      builderIsSleeping = view builderState game == Sleeping
      doBetter = game & updateEvents BuilderUpdate builderStateDelay
                      & over builderState builderSucc
      getBetter = doBetter & notifyRoom (showState (view builderState doBetter))
  in if builderIsSleeping then doNothing else getBetter

gatherWood :: Game -> Game
gatherWood game =
  game & updateEvents BuilderGathersWood builderGatherDelay
       & over (stored . wood) (+ 2)
