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
