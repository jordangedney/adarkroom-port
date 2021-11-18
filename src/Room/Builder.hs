{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Room.Builder
  ( update
  , canHelp
  , approach
  , gatherWood
  , canBuildTraps
  , canBuildCarts
  , build
  , updateBuildables
  )
where

import Control.Monad.State
import Control.Lens hiding (pre)

import Shared.UI
import Shared.Game
import Shared.GameEvent (GameEvent(..))
import Shared.Constants
import Shared.Item
import Shared.Util

import Util (notifyRoom', updateEvent, costMsg)

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
builderSucc = \case
  Helping -> Helping
  x -> succ x

canHelp :: Game -> Game
canHelp = execState $ do
  bs <- use builderState
  when (bs == Sleeping) $ do
    -- builder gets up to help
    builderState %= builderSucc
    (milestones . builderIsHelping) .= True
    -- she finds a way to pass the time
    updateEvent BuilderGathersWood builderGatherDelay
    updateEvent UnlockTraps unlockTrapsDelay
    displayBuilderState

displayBuilderState :: DarkRoom
displayBuilderState = do
  builderIs <- showState <$> use builderState
  notifyRoom' builderIs

-- approach :: DarkRoom
approach :: Game -> Game
approach = execState $ do
  -- let the player know Builder is coming
  displayBuilderState

  -- builder is on the way; going to need wood soon
  updateEvent BuilderUpdate builderStateDelay
  updateEvent UnlockForest needWoodDelay

-- update :: DarkRoom
update :: Game -> Game
update = execState $ do
  builderIs <- use builderState

  -- XXX Hack with canHelp? Sucky comments suck
  unless (builderIs == Sleeping || builderIs == Helping) $ do
    -- Builder is getting better all the time
    updateEvent BuilderUpdate builderStateDelay
    builderState %= builderSucc
    displayBuilderState

gatherWood :: DarkRoom
gatherWood = do
  -- gather more wood later
  updateEvent BuilderGathersWood builderGatherDelay

  -- only gather if the room is warm
  temp <- use roomTemperature
  unless (temp == Freezing || temp == Cold) (stored.wood += 2)

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

getCraftableAttrs :: Craftable -> CraftableAttributes
getCraftableAttrs = \case
  Trap -> Resource
    "builder says she can make traps to catch any creatures might still be alive out there."
    "more traps to catch more creatures."
    (maximumNumberOfTraps, "more traps won't help now.")
    (\g -> [(Wood, (view (stored . trap) g * 10) + 10)])
  Cart -> Building
    "builder says she can make a cart for carrying wood."
    "the rickety cart will carry more wood from the forest."
    [(Wood, 30)]
  Hut -> Resource
    "builder says there are more wanderers. says they'll work, too."
    "builder puts up a hut, out in the forest. says word will get around."
    (maximumNumberOfHuts, "no more room for huts.")
    (\g -> [(Wood, (view (stored . hut) g * 50) + 100)])
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

canBuildTraps :: Game -> Game
canBuildTraps = execState $ do
  (uiState . showTrapBtn) .= True
  notifyRoom' ("builder says she can make traps to catch any creatures "
              <> "might still be alive out there.")

canBuildCarts :: Game -> Game
canBuildCarts = execState $ do
  pre <- use (milestones . preCartsUnlocked)
  post <- use (uiState . showCartBtn)
  -- unlocking a cart is a 3 stage progress- this takes you from stage 2 -> 3
  when (pre && not post) $ do
    (uiState . showCartBtn) .= True
    notifyRoom' "builder says she can make a cart for carrying wood"

updateBuildables :: Game -> Game
updateBuildables = execState $ do
  g <- get
  let notBuildable = filter (not . craftableShowBtn g) buildables

      nearlyAfford :: [(Item, Int)] -> Bool
      nearlyAfford [] = True
      nearlyAfford ((Wood, c):cs) =
        g ^. (stored . wood) >= c `div` 2 && nearlyAfford cs
      nearlyAfford ((i, _):cs) =
        -- TODO: This needs to be changed to 'has ever been seen' but instead of
        -- hardcoding the stored items marking flags I should just use a
        -- dictionary to store the items in and then check their existence. As
        -- it is now, just check if we have one.
        g ^. getItem i >= 1 && nearlyAfford cs

  forM_ notBuildable $ \c -> do
    let (msg, cost) = case getCraftableAttrs c of
                        (Building a _ cost') -> (a, cost')
                        (Resource a _ _ costFn) -> (a, costFn g)
                        _ -> error $ "Unexpected item "<> show c
    when (nearlyAfford cost) $ do
      (uiState . craftableReady c) .= True
      notifyRoom' msg

build :: Craftable -> Game -> Game
build i = case getCraftableAttrs i of
  (Building _ b c) -> execState $ go b c
  (Tool b c) -> execState $ go b c
  (Resource _ b (maxNum, maxMsg) costFn) -> execState $ do
    -- traps and huts have variable cost depending on how many exist
    c <- gets costFn
    numItem <- use (getCraftable i)

    if numItem < maxNum then do
      go b c
    else do
      notifyRoom' maxMsg
  where
    go :: String -> [(Item, Int)] -> DarkRoom
    go buildMsg cost = do
      uiState . showForestBuildings .= True
      -- only build if the room is warm
      temp <- use roomTemperature
      if temp == Freezing || temp == Cold then do
        notifyRoom' "builder just shivers."

      else do
        canBuild <- gets (canAfford cost)
        if canBuild then do
          getCraftable i += 1
          forM_ cost $ \(item', amt) -> do
            getItem item' -= amt
          notifyRoom' buildMsg
        else do
          modify (costMsg cost)
