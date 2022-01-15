module Outside
  ( unlock
  , arrival
  , gather
  , checkTraps
  , maxPopulation
  )
where

import System.Random (StdGen)
import Control.Lens (set, view, (&))
import Data.List (nub, intercalate)

import Shared.UI
import Shared.GameEvent (GameEvent(GatherWood, CheckTraps))
import Shared.Game
import Shared.Constants

import Util (addEvent, updateEvents, randomChoices)
import Shared.Item
import Shared.Util (getItem, overItem, playerHasSeen)

unlock :: Game -> Game
unlock game =
  game & set (uiState . showStores) True
       & set (uiState . showOutside) True
       & overItem Wood (const 4)
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
  let amountToGather = if playerHasSeen Cart game then 50 else 10
  in game & overItem Wood (+ amountToGather)
          & updateEvents GatherWood gatherCooldown
          & addEvent "dry brush and dead branches litter the forest floor."

unlockTrapItemView :: Game -> Game
unlockTrapItemView game =
  let items =
        [ (Fur,    showFur)
        , (Meat,   showMeat)
        , (Scale,  showScales)
        , (Teeth,  showTeeth)
        , (Cloth,  showCloth)
        , (Charm,  showCharm)
        ]
      unlockItems = [set (uiState . shower) True | (i, shower) <- items,
                     getItem i game > 0] ++ [set (uiState . showBait) True]
  in foldr (\update g -> update g) game unlockItems

checkTraps :: StdGen -> Game -> Game
checkTraps randomGen game =
  let trapItems =
        [ (50, (Fur,   "scraps of fur"))
        , (25, (Meat,  "bits of meat"))
        , (10, (Scale, "strange scales"))
        , (8,  (Teeth, "scattered teeth"))
        , (6,  (Cloth, "tattered cloth"))
        , (1,  (Charm, "a crudely made charm"))
        ]

      numTraps = getItem Trap game
      numBait = getItem Bait game
      additionalDrops = min numTraps numBait
      numDrops = numTraps + additionalDrops

      -- Wood doesn't drop, its a safeguard in case I can't add to 100
      drops = take numDrops (randomChoices randomGen (Wood, "a broken stick") trapItems)
      gatherDrops = map ((\found -> overItem found (+ 1)) . fst) drops

      eventMsgs = nub (map snd drops)
      eventItems = if length eventMsgs == 1 then last eventMsgs
                   else intercalate ", " (init eventMsgs) ++ " and " ++  last eventMsgs
      eventMsg = "the traps contain " ++ eventItems ++ "."

      thingsGathered = foldr (\a b -> a b) game gatherDrops

  in thingsGathered
     & unlockTrapItemView
     & updateEvents CheckTraps checkTrapsCooldown
     & addEvent eventMsg
     & overItem Bait (subtract additionalDrops)

maxPopulation :: Game -> Int
maxPopulation game = getItem Hut game * 4
