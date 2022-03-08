{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Room.Builder
  ( update
  , canHelp
  , approach
  , gatherWood
  , build
  , updateBuildables
  )
where

import Control.Lens
import Control.Monad.State (get, gets, forM_, when, unless)

import Shared.UI
import Shared.Game
import Shared.GameEvent (GameEvent(..))
import Shared.Constants
import Shared.Item
import Shared.Util
import Util (notifyRoom, updateEvent, displayCosts)

showState :: BuilderState -> String
showState = \case
  Approaching ->
    "the light from the fire spills from the windows, out into the dark."
  Collapsed ->
    "a ragged stranger stumbles through the door and collapses in the corner."
  Shivering ->
    "the stranger shivers, and mumbles quietly. her words are unintelligible."
  Sleeping ->
    "the stranger in the corner stops shivering. her breathing calms."
  Helping ->
    "the stranger is standing by the fire. "
    <> "she says she can help. says she builds things."

builderSucc :: BuilderState -> BuilderState
builderSucc = \case { Helping -> Helping; x -> succ x }

canHelp :: DarkRoom
canHelp = do
  bs <- use builderState

  -- she waits until the player returns from the forest before building
  when (bs == Helping) $ do
    (milestones . buildUnlocked) .= True

  when (bs == Sleeping) $ do
    -- builder gets up to help
    builderState %= builderSucc
    -- she finds a way to pass the time
    updateEvent BuilderGathersWood builderGatherDelay
    displayBuilderState

displayBuilderState :: DarkRoom
displayBuilderState = do
  builderIs <- showState <$> use builderState
  notifyRoom builderIs

approach :: DarkRoom
approach = do
  -- let the player know Builder is coming
  displayBuilderState

  -- builder is on the way; going to need wood soon
  updateEvent BuilderUpdate builderStateDelay
  updateEvent UnlockForest needWoodDelay

update :: DarkRoom
update = do
  builderIs <- use builderState
  unless (builderIs == Sleeping || builderIs == Helping) $ do
    -- builder is getting better all the time
    updateEvent BuilderUpdate builderStateDelay
    builderState %= builderSucc
    displayBuilderState

gatherWood :: DarkRoom
gatherWood = do
  -- gather more wood later
  updateEvent BuilderGathersWood builderGatherDelay

  -- only gather if the room is warm
  temp <- use roomTemperature
  unless (temp == Freezing || temp == Cold) (overStored Wood (+2))

-- XXX unable to reside in Shared.Item because of dependency cycling
data CraftableAttributes
  = Building
     String -- available message
     String -- build message
     [(Item, Int)] -- cost
  | Tool
     String -- build message
     [(Item, Int)] -- cost
  | Resource
     String -- available message
     String -- build message
     (Int, String) -- max number of constructions
     (Game -> [(Item, Int)]) -- cost

getCraftableAttrs :: Item -> CraftableAttributes
getCraftableAttrs = \case
  Trap -> Resource
    "builder says she can make traps to catch any creatures might still be alive out there."
    "more traps to catch more creatures."
    (maximumNumberOfTraps, "more traps won't help now.")
    (\g -> [(Wood, (getItem Trap g * 10) + 10)])
  Cart -> Building
    "builder says she can make a cart for carrying wood."
    "the rickety cart will carry more wood from the forest."
    [(Wood, 30)]
  Hut -> Resource
    "builder says there are more wanderers. says they'll work, too."
    "builder puts up a hut, out in the forest. says word will get around."
    (maximumNumberOfHuts, "no more room for huts.")
    (\g -> [(Wood, (getItem Hut g * 50) + 100)])
  Lodge -> Building
    "villagers could help hunt, given the means."
    "the hunting lodge stands in the forest, a ways out of town."
    [(Wood, 200), (Fur, 10), (Meat, 5)]
  TradingPost -> Building
    "a trading post would make commerce easier."
    "now the nomads have a place to set up shop, they might stick around a while"
    [(Wood, 400), (Fur, 100)]
  Tannery -> Building
    "leather could be useful. says the villagers could make it."
    "tannery goes up quick, on the edge of the village. "
    [(Wood, 500), (Fur, 50)]
  Smokehouse -> Building
    "should cure the meat, or it'll spoil. builder says she can fix something up."
    "builder finishes the smokehouse. she looks hungry."
    [(Wood, 600), (Meat, 50)]
  Workshop -> Building
     "builder says she could make finer things, if she had the tools."
     "workshop's finally ready. builder's excited to get to it."
     [(Wood, 800), (Leather, 100), (Scale, 10)]
  Steelworks -> Building
     "builder says the villagers could make steel, given the tools."
     "a haze falls over the village as the steelworks fires up."
     [(Wood, 1500), (Iron, 100), (Coal, 100)]
  Armory -> Building
     "builder says it'd be useful to have a steady source of bullets."
     "armoury's done, welcoming back the weapons of the past."
     [(Wood, 3000), (Steel, 100), (Sulphur, 50)]
  Torch -> Tool
    "a torch to keep the dark away."
    [(Wood, 1), (Cloth, 1)]
  Waterskin -> Tool
    "this waterskin'll hold a bit of water, at least."
    [(Leather, 50)]
  Cask -> Tool
    "the cask holds enough water for longer expeditions."
    [(Leather, 100), (Iron, 20)]
  WaterTank -> Tool
    "never go thirsty again."
    [(Iron, 100), (Steel, 50)]
  BoneSpear -> Tool
    "this spear's not elegant, but it's pretty good at stabbing."
    [(Wood, 100), (Teeth, 5)]
  Rucksack -> Tool
    "carrying more means longer expeditions to the wilds."
    [(Leather, 200)]
  Wagon -> Tool
    "the wagon can carry a lot of supplies."
    [(Wood, 500), (Iron, 100)]
  Convoy -> Tool
    "the convoy can haul mostly everything."
    [(Wood, 1000), (Iron, 200), (Steel, 100)]
  LeatherArmor -> Tool
    "leather's not strong. better than rags, though."
    [(Leather, 200), (Scale, 20)]
  IronArmor -> Tool
    "iron's stronger than leather."
    [(Leather, 200), (Iron, 100)]
  SteelArmor -> Tool
    "steel's stronger than iron"
    [(Leather, 200), (Steel, 100)]
  IronSword -> Tool
    "sword is sharp. good protection out in the wilds."
    [(Wood, 200), (Leather, 50), (Iron, 20)]
  SteelSword -> Tool
    "the steel is strong, and the blade true."
    [(Wood, 500), (Leather, 100), (Steel, 20)]
  Rifle -> Tool
    "black powder and bullets, like the old days."
    [(Wood, 200), (Steel, 50), (Sulphur, 50)]
  _ -> error "you done fucked"

updateBuildables :: DarkRoom
updateBuildables = do
  g <- get

      -- those buttons not yet displayed
  let notBuildable = filter (not . (\i -> g ^. craftableReady i)) buildables

      -- so close, yet so far
      nearlyAfford (Wood, c) = getItem Wood g >= c `div` 2
      nearlyAfford (i   , _) = playerHasSeen i g

      -- same data, just takes some shuffling
      unpackCost c = case getCraftableAttrs c of
        (Building m _ cost')    -> (m, cost')
        (Resource m _ _ costFn) -> (m, costFn g)
        _                       -> error $ "Unexpected item "<> show c

  -- once builder is well
  when (g ^. milestones . buildUnlocked) $ do
    forM_ notBuildable $ \c -> do
      -- if you can nearly afford it, builder realizes she can build it
      let (msg, cost) = unpackCost c
      when (all nearlyAfford cost) $ do
        craftableReady c .= True
        notifyRoom msg

build :: Item -> DarkRoom
build i = do
  case getCraftableAttrs i of
   (Building _ b c) -> ifAffordable c (doBuild b c)
   (Tool b c) -> ifAffordable c (doBuild b c)
   (Resource _ b (maxNum, maxMsg) costFn) -> do
     -- traps and huts have variable cost depending on how many exist
     c <- gets costFn
     numItem <- gets (getItem i)

     -- can only check so many traps; cities can only get so big
     ifAffordable c $ do
        when (numItem < maxNum) $ do doBuild b c
        when (maxNum - numItem <= 1) $ do notifyRoom maxMsg

   where
     ifAffordable :: [(Item, Int)] -> DarkRoom -> DarkRoom
     ifAffordable cost buildItem = do
       temp <- use roomTemperature
       if temp == Freezing || temp == Cold then do
         notifyRoom "builder just shivers."
       else do
         cA <- gets (canAfford cost)
         if cA then do buildItem
         -- time to gather more wood
         else do displayCosts cost

     doBuild :: String -> [(Item, Int)] -> DarkRoom
     doBuild buildMsg cost = do
       -- show that which can be used in the forest
       uiState . showForestBuildings .= True

       overStored i (+1)
       forM_ cost $ \(item', amt) -> do
        overStored item' (+ (-amt))
       notifyRoom buildMsg
