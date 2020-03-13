module Builder
  ( update
  )
where

import Control.Lens (over, set, view, (&))

import GameTypes (Game, BuilderState(..),
                  upcomingEvents, builderState, builderArrived, milestones)
import GameEvent (GameEvent(BuilderUpdate, UnlockForest), updateEvents)
import Constants (builderStateDelay, needWoodDelay)

import qualified Room

-- Defined in GameTypes to avoid an import cycle:
-- data BuilderState
--   = Approaching
--   | Collapsed
--   | Shivering
--   | Sleeping
--   | Helping

showState :: BuilderState -> String
showState Approaching = "the light from the fire spills from the windows, out into the dark."
showState Collapsed   = "a ragged stranger stumbles through the door and collapses in the corner."
showState Shivering   = "the stranger shivers, and mumbles quietly. her words are unintelligible."
showState Sleeping    = "the stranger in the corner stops shivering. her breathing calms."
showState Helping     = "the stranger is standing by the fire. she says she can help. says she builds things."

builderSucc :: BuilderState -> BuilderState
builderSucc Helping =  Helping
builderSucc Sleeping = Sleeping
builderSucc x = succ x

builderAppears :: Game -> Game
builderAppears game =
  let doNothing = game
      builderInRoom = view (milestones . builderArrived) game
      builderArrives =
        game & set (milestones . builderArrived) True
             & over upcomingEvents (updateEvents (UnlockForest needWoodDelay))
  in if builderInRoom then doNothing else builderArrives

update :: Game -> Game
update game =
  let withStatus =
        game & Room.notify (showState (view builderState game))
             & over upcomingEvents (updateEvents (BuilderUpdate builderStateDelay))
             & over builderState builderSucc
             & builderAppears
  in withStatus
