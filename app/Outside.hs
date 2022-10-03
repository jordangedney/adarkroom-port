{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Outside
  ( unlock
  , arrival
  , gather
  , checkTraps
  , workerReward
  )
where

import System.Random (StdGen)
import Control.Lens
import Data.List (nub, intercalate)

import Shared.UI
import Shared.GameEvent (GameEvent(GatherWood, CheckTraps))
import Shared.Game
import Shared.Constants

import Util (notify, updateEvent, randomChoices)
import Shared.Item
import Shared.Util (playerHasSeen, overStored, getStored)
import Control.Monad (forM_, unless)
import Control.Monad.State (gets)

unlock :: DarkRoom
unlock = do
  -- the world is bigger than just this cabin
  (uiState . showStores) .= True
  (uiState . showOutside) .= True
  overStored Wood (const 4)
  notify "the wind howls outside."
  notify "the wood is running out."

firstArrival :: DarkRoom
firstArrival = do
  goneOutside <- use (milestones . seenForest)
  -- the sunlight hurts my eyes
  unless goneOutside $ do
    (milestones . seenForest) .= True
    notify "the sky is grey and the wind blows relentlessly."

arrival :: DarkRoom
arrival = do
  firstArrival
  location .= Outside

gather :: DarkRoom
gather = do
  -- more logs for more fire
  amountToGather <- gets ((\p -> if p then 50 else 10) . playerHasSeen Cart)
  overStored Wood (+ amountToGather)
  updateEvent GatherWood gatherCooldown
  notify "dry brush and dead branches litter the forest floor."

checkTraps :: StdGen -> DarkRoom
checkTraps rndGen = do
  numTraps <- getStored Trap
  numBait <- getStored Bait

  let trapItems =
        [ (50, (Fur,   "scraps of fur"))
        , (25, (Meat,  "bits of meat"))
        , (10, (Scale, "strange scales"))
        , (8,  (Teeth, "scattered teeth"))
        , (6,  (Cloth, "tattered cloth"))
        , (1,  (Charm, "a crudely made charm"))
        ]

      additionalDrops = min numTraps numBait
      nDrops = numTraps + additionalDrops

      -- XXX Wood doesn't drop, its a safeguard in case I can't add to 100
      drops = take nDrops (randomChoices rndGen (Wood, "a broken stick") trapItems)

  overStored Bait (subtract additionalDrops)
  forM_ drops $ \(i, _) -> do
    overStored i (+1)

  let dropMsgs = nub (map snd drops)
      eventItems = if length dropMsgs == 1 then last dropMsgs
                   else intercalate ", " (init dropMsgs) ++ " and " ++ last dropMsgs
      eventMsg = "the traps contain " ++ eventItems ++ "."

  notify eventMsg
  updateEvent CheckTraps checkTrapsCooldown

workerReward :: Worker -> [(Item, Float)]
workerReward = \case
  Gatherer -> [(Wood, 1)]
  Hunter -> [(Fur, 0.5), (Meat, 0.5)]
  Trapper -> [(Meat, -1), (Bait, 1)]
  Tanner -> [(Fur, -5), (Leather, 1)]
  Charcutier -> [(Meat, -5), (Wood, -5), (CuredMeat, -1)]
  IronMiner -> [(CuredMeat, -1), (Iron, -1)]
  CoalMiner -> [(CuredMeat, -1), (Coal, 1)]
  SulphurMiner -> [(CuredMeat, -1), (Sulphur, 1)]
  Steelworker -> [(Iron, -1), (Coal, -1), (Steel, 1)]
  Armourer -> [(Steel, -1), (Sulphur, -1), (Bullets, 1)]
