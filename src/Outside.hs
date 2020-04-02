module Outside
  ( unlock
  , arrival
  , gather
  )
where

import Control.Lens (over, set, view, (&))

import UIState (showStores, showOutside)
import GameEvent (GameEvent(GatherWood))
import GameTypes (Game, Location(Outside),
                  stored, wood, uiState, seenForest, milestones, location, builderIsHelping,
                  preCartsUnlocked, carts,
                  )
import Constants

import GameUtil (addEvent, updateEvents)

unlock :: Game -> Game
unlock game =
  game & set (uiState . showStores) True
       & set (uiState . showOutside) True
       & set (stored . wood) 4
       & addEvent "the wind howls outside."
       & addEvent "the wood is running out."

firstArrival :: Game -> Game
firstArrival game =
  let doNothing = game
      haveGoneOutsideBefore = view (milestones . seenForest) game
      firstTimeOutsideMessage = "the sky is grey and the wind blows relentlessly"
      firstTimeOutside =
        game & set (milestones . seenForest) True
             & addEvent firstTimeOutsideMessage
  in if haveGoneOutsideBefore then doNothing else firstTimeOutside

arrival :: Game -> Game
arrival game =
  game & firstArrival
       & set location Outside

gather :: Game -> Game
gather game =
  let amountToGather = if view (stored . carts) game > 0 then 50 else 10
      woodGathered =
        game & over (stored . wood) (+ amountToGather)
             & updateEvents GatherWood gatherCooldown
             & addEvent "dry brush and dead branches litter the forest floor"

  in if view (milestones . builderIsHelping) game
     then woodGathered & set (milestones . preCartsUnlocked) True
     else woodGathered
