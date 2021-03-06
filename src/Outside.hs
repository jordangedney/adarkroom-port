module Outside
  ( unlock
  , arrival
  , gather
  , checkTraps
  , maxPopulation
  )
where

import System.Random (StdGen)
import Control.Lens (over, set, view, (&))
import Data.List (nub, intercalate)

import UI.State (showStores, showOutside, showItems,
                showBait, showFur, showMeat, showScales, showTeeth, showCloth, showCharm)
import GameEvent (GameEvent(GatherWood, CheckTraps))
import GameTypes (Game, Location(Outside),
                  stored, wood, uiState, seenForest, milestones, location, builderIsHelping,
                  preCartsUnlocked, carts, huts, traps,
                  bait, fur, meat, scales, teeth, cloth, charm)
import Constants

import GameUtil (addEvent, updateEvents)
import Util (randomChoices)

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
      firstTimeOutsideMessage = "the sky is grey and the wind blows relentlessly."
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
             & addEvent "dry brush and dead branches litter the forest floor."

  in if view (milestones . builderIsHelping) game
     then woodGathered & set (milestones . preCartsUnlocked) True
     else woodGathered

unlockTrapItemView :: Game -> Game
unlockTrapItemView game =
  let items =
        [ (fur,    showFur)
        , (meat,   showMeat)
        , (scales, showScales)
        , (teeth,  showTeeth)
        , (cloth,  showCloth)
        , (charm,  showCharm)
        ]
      unlockItems = [set (uiState . showItems . shower) True | (getter, shower) <- items,
                     view (stored . getter) game > 0] ++ [set (uiState . showItems . showBait) True]
  in foldr (\update g -> update g) game unlockItems

checkTraps :: StdGen -> Game -> Game
checkTraps randomGen game =
  let trapItems =
        [ (50, (fur,    "scraps of fur"))
        , (25, (meat,   "bits of meat"))
        , (10, (scales, "strange scales"))
        , (8,  (teeth,  "scattered teeth"))
        , (6,  (cloth,  "tattered cloth"))
        , (1,  (charm,  "a crudely made charm"))
        ]

      numTraps = view (stored . traps) game
      numBait = view (stored . bait) game
      additionalDrops = min numTraps numBait
      numDrops = numTraps + additionalDrops

      -- Wood doesn't drop, its a safeguard in case I can't add to 100
      drops = take numDrops (randomChoices randomGen (wood, "a broken stick") trapItems)
      gatherDrops = map ((\found -> over (stored . found) (+ 1)) . fst) drops

      eventMsgs = nub (map snd drops)
      eventItems = if length eventMsgs == 1 then last eventMsgs
                   else intercalate ", " (init eventMsgs) ++ " and " ++  last eventMsgs
      eventMsg = "the traps contain " ++ eventItems ++ "."

      thingsGathered = foldr (\a b -> a b) game gatherDrops

  in thingsGathered & unlockTrapItemView & updateEvents CheckTraps checkTrapsCooldown & addEvent eventMsg

maxPopulation :: Game -> Int
maxPopulation game = view (stored . huts) game * 4
