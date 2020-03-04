module Builder
  ( update
  )
where

import Control.Lens (over, set, view, (&))

import GameTypes (Game, events, upcomingEvents,
                  builderLevel, builderArrived, milestones)
import GameEvent (GameEvent(BuilderUpdate, UnlockForest), updateEvents)
import Constants (builderStateDelay, needWoodDelay)
import Util (addEvent)

-- Defined in GameTypes to avoid an import cycle:
data BuilderState
  = Approaching
  | Collapsed
  | Shivering
  | Sleeping
  | Helping
  deriving (Eq, Show, Enum, Ord)

builderState :: BuilderState -> String
builderState Approaching = "the light from the fire spills from the windows, out into the dark."
builderState Collapsed   = "a ragged stranger stumbles through the door and collapses in the corner."
builderState Shivering   = "the stranger shivers, and mumbles quietly. her words are unintelligible."
builderState Sleeping    = "the stranger in the corner stops shivering. her breathing calms."
builderState Helping     = "the stranger is standing by the fire. she says she can help. says she builds things."

builderSucc :: BuilderState -> BuilderState
builderSucc Helping =  Helping
builderSucc Sleeping = Sleeping
builderSucc x = succ x

builderAppears :: Game -> Game
builderAppears game =
  let doNothing = game
      builderInRoom = view (milestones . builderArrived) game
      builderArrivalMessage =
        "a ragged stranger stumbles through the door and collapses in the corner."
      builderArrives =
        game & set (milestones . builderArrived) True
             & over events (addEvent builderArrivalMessage)
             & over upcomingEvents (updateEvents (UnlockForest needWoodDelay))
             & over builderLevel (+1)

  in if builderInRoom then doNothing else builderArrives

update :: Game -> Game
update game =
  let builderUpdate =
        game & over upcomingEvents (updateEvents (BuilderUpdate builderStateDelay))
             & builderAppears
  in builderUpdate
