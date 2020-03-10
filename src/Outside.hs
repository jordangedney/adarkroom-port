module Outside
  ( unlock
  , arrival
  , gather
  )
where

import Control.Lens (over, set, view, (&))

import UIState (showStores, showWood, showOutside)
import GameEvent (GameEvent(GatherWood), updateEvents)
import GameTypes (Game, Location(Outside),
                  events, stored, wood, uiState, seenForest, milestones, location,
                  upcomingEvents,
                  )
import Constants
import Util (addEvent)

unlock :: Game -> Game
unlock game =
  game & set (uiState . showStores . showWood) True
       & set (uiState . showOutside) True
       & set (stored . wood) 4
       & over events (addEvent "the wind howls outside.")
       & over events (addEvent "the wood is running out.")

firstArrival :: Game -> Game
firstArrival game =
  let doNothing = game
      haveGoneOutsideBefore = view (milestones . seenForest) game
      firstTimeOutsideMessage = "the sky is grey and the wind blows relentlessly"
      firstTimeOutside =
        game & set (milestones . seenForest) True
             & over events (addEvent firstTimeOutsideMessage)
  in if haveGoneOutsideBefore then doNothing else firstTimeOutside

arrival :: Game -> Game
arrival game =
  game & firstArrival
       & set location Outside

gather :: Game -> Game
gather game =
  game & over (stored . wood) (+10)
       & over upcomingEvents (updateEvents (GatherWood gatherCooldown))
       & over events (addEvent "dry brush and dead branches litter the forest floor")
