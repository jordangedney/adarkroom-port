module Outside
  ( unlock
  , arrival
  , gather
  , checkTraps
  , maxPopulation
  )
where

import Control.Lens (over, set, view, (&))

import UIState (showStores, showOutside, showItems,
                showBait, showFur, showMeat, showScales, showTeeth, showCloth, showCharm)
import GameEvent (GameEvent(GatherWood, CheckTraps))
import GameTypes (Game, Location(Outside),
                  stored, wood, uiState, seenForest, milestones, location, builderIsHelping,
                  preCartsUnlocked, carts, huts, traps,
                  bait, fur, meat, scales, teeth, cloth, charm)
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

-- trapItems =
--   [ 50%  (0.5,  fur, "scraps of fur")
--   , 25%  (0.75, meat, "bits of meat")
--   , 10% (0.85, scales, "strange scales")
--   , 8% (0.93, teeth, "scattered teeth")
--   , 6% (0.995, cloth, "tattered cloth")
--   , 1% (1.0, charm, "a crudely made charm")
--   ]

--  bait
--  fur
--  meat
--  scales
--  teeth
--  cloth
--  charm

unlockTrapItemView :: Game -> Game
unlockTrapItemView game =
  let items =
        [ (bait,   showBait)
        , (fur,    showFur)
        , (meat,   showMeat)
        , (scales, showScales)
        , (teeth,  showTeeth)
        , (cloth,  showCloth)
        , (charm,  showCharm)
        ]
      unlockItems = [set (uiState . showItems . shower) True | (getter, shower) <- items,
                     view (stored . getter) game > 0]
  in foldr (\update g -> update g) game unlockItems

checkTraps :: Game -> Game
checkTraps game =
  let numTraps = view (stored . traps) game
      numBait = view (stored . bait) game
      additionalDrops = min numTraps numBait
      numDrops = numTraps + additionalDrops

      thingsGathered =
        game & over (stored . bait)   (+ 1)
             & over (stored . fur)    (+ 1)
             & over (stored . meat)   (+ 1)
             & over (stored . scales) (+ 1)
             & over (stored . teeth)  (+ 1)
             & over (stored . cloth)  (+ 1)
             & over (stored . charm)  (+ 1)
             & updateEvents CheckTraps checkTrapsCooldown
             & addEvent "the traps contain a lot of shit"
             & unlockTrapItemView

  in thingsGathered

maxPopulation :: Game -> Int
maxPopulation game = view (stored . huts) game * 4
