{-# LANGUAGE DuplicateRecordFields #-}

module Room.Builder
  ( update
  , canHelp
  , approach
  , gatherWood
  , canBuildTraps
  , canBuildCarts
  , buildTrap
  , buildCart
  )
where

import Control.Monad.State (State, execState, get, state, modify)
import Control.Lens hiding (pre)
import Control.Monad (unless, when)

import Shared.UI (showForestBuildings)
import Shared.Game
import Shared.GameEvent (GameEvent(..), eventGetter)
import Shared.Constants
import Shared.Item

import Util (notifyRoom, updateEvents, notifyRoom', updateEvent)

showState :: BuilderState -> String
showState Approaching =
  "the light from the fire spills from the windows, out into the dark."
showState Collapsed   =
  "a ragged stranger stumbles through the door and collapses in the corner."
showState Shivering   =
  "the stranger shivers, and mumbles quietly. her words are unintelligible."
showState Sleeping    =
  "the stranger in the corner stops shivering. her breathing calms."
showState Helping     =
  "the stranger is standing by the fire. she says she can help. says she builds things."

builderSucc :: BuilderState -> BuilderState
builderSucc Helping =  Helping
builderSucc x = succ x

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

data CraftableCost = Static [(Item, Int)] | Dynamic (Game -> [(Item, Int)])

-- XXX Need to change this not to be records because record problem or w/e;
-- (Don't use records with sum types)
data Craftable
  = Building
  { name :: String
  , availableMsg :: String
  , buildMsg :: String
  , maxNum  :: Maybe Int
  , maxMsg  :: Maybe String
  , cost :: CraftableCost
  }
  | Tool
  { name :: String
  , buildMsg :: String
  -- , cost :: [(Item, Int)]
  , cost :: CraftableCost
  }
  | Upgrade
  { name :: String
  , buildMsg :: String
  -- , cost :: [(Item, Int)]
  , cost :: CraftableCost
  }
  | Weapon
  { name :: String
  , buildMsg :: String
  -- , cost :: [(Item, Int)]
  , cost :: CraftableCost
  }

getCraftable Hut = Building
  { name         = "hut"
  , availableMsg = "builder says there are more wanderers. says they'll work, too."
  , buildMsg     =
      "builder puts up a hut, out in the forest. says word will get around."
  , maxNum       = Just 20
  , maxMsg       = Just "no more room for huts."
  , cost         = Dynamic (\g -> [(Wood, (view (stored . huts) g * 50) + 100)])
  }
getCraftable Lodge = Building
  { name         = "lodge"
  , availableMsg = "villagers could help hunt, given the means."
  , buildMsg     = "the hunting lodge stands in the forest, a ways out of town."
  , maxNum       = Just 1
  , maxMsg       = Nothing
  , cost         = Static [(Wood, 200), (Fur, 10), (Meat, 5)]
  }
getCraftable TradingPost = Building
  { name         = "trading post"
  , availableMsg = "a trading post would make commerce easier."
  , buildMsg     =
      "now the nomads have a place to set up shop, they might stick around a while."
  , maxNum       = Just 1
  , maxMsg       = Nothing
  , cost         = Static [(Wood, 400), (Fur, 100)]
  }
getCraftable Tannery = Building
  { name         = "tannery"
  , availableMsg =
      "builder says leather could be useful. says the villagers could make it."
  , buildMsg     = "tannery goes up quick, on the edge of the village. "
  , maxNum       = Just 1
  , maxMsg       = Nothing
  , cost         = Static [(Wood, 500), (Fur, 50)]
  }
getCraftable Smokehouse = Building
  { name         = "smokehouse"
  , availableMsg =
      "should cure the meat, or it'll spoil. builder says she can fix something up."
  , buildMsg     = "builder finishes the smokehouse. she looks hungry."
  , maxNum       = Just 1
  , maxMsg       = Nothing
  , cost         = Static [(Wood, 600), (Meat, 50)]
  }
getCraftable Workshop = Building
  { name         = "workshop"
  , availableMsg = "builder says she could make finer things, if she had the tools."
  , buildMsg     = "workshop's finally ready. builder's excited to get to it."
  , maxNum       = Just 1
  , maxMsg       = Nothing
  , cost         = Static [(Wood, 800), (Leather, 100), (Scale, 10)]
  }
getCraftable Steelworks = Building
  { name         = "steelworks"
  , availableMsg = "builder says the villagers could make steel, given the tools."
  , buildMsg     = "a haze falls over the village as the steelworks fires up."
  , maxNum       = Just 1
  , maxMsg       = Nothing
  , cost         = Static [(Wood, 1500), (Iron, 100), (Coal, 100)]
  }
getCraftable Armoury = Building
  { name         = "armoury"
  , availableMsg = "builder says it'd be useful to have a steady source of bullets."
  , buildMsg     = "armoury's done, welcoming back the weapons of the past."
  , maxNum       = Just 1
  , maxMsg       = Nothing
  , cost         = Static [(Wood, 3000), (Steel, 100), (Sulphur, 50)]
  }
getCraftable Torch = Tool
  { name         = "torch"
  , buildMsg     = "a torch to keep the dark away."
  , cost         = Static [(Wood, 1), (Cloth, 1)]
  }
getCraftable Waterskin = Upgrade
  { name         = "waterskin"
  , buildMsg     = "this waterskin'll hold a bit of water, at least."
  , cost         = Static [(Leather, 50)]
  }
getCraftable Cask = Upgrade
  { name         = "cask"
  , buildMsg     = "the cask holds enough water for longer expeditions."
  , cost         = Static [(Leather, 100), (Iron, 20)]
  }
getCraftable WaterTank = Upgrade
  { name         = "water tank"
  , buildMsg     = "never go thirsty again."
  , cost         = Static [(Iron, 100), (Steel, 50)]
  }
getCraftable BoneSpear = Weapon
  { name         = "bone spear"
  , buildMsg     = "this spear's not elegant, but it's pretty good at stabbing."
  , cost         = Static [(Wood, 100), (Teeth, 5)]
  }
getCraftable Rucksack = Upgrade
  { name         = "rucksack"
  , buildMsg     = "carrying more means longer expeditions to the wilds."
  , cost         = Static [(Leather, 200)]
  }
getCraftable Wagon = Upgrade
  { name         = "wagon"
  , buildMsg     = "the wagon can carry a lot of supplies."
  , cost         = Static [(Wood, 500), (Iron, 100)]
  }
getCraftable Convoy = Upgrade
  { name         = "convoy"
  , buildMsg     = "the convoy can haul mostly everything."
  , cost         = Static [(Wood, 1000), (Iron, 200), (Steel, 100)]
  }
getCraftable LeatherArmour = Upgrade
  { name         = "l armour"
  , buildMsg     = "leather's not strong. better than rags, though."
  , cost         = Static [(Leather, 200), (Scale, 20)]
  }
getCraftable IronArmour = Upgrade
  { name         = "i armour"
  , buildMsg     = "iron's stronger than leather."
  , cost         = Static [(Leather, 200), (Iron, 100)]
  }
getCraftable SteelArmour = Upgrade
  { name         = "s armour"
  , buildMsg     = "steel's stronger than iron"
  , cost         = Static [(Leather, 200), (Steel, 100)]
  }
getCraftable IronSword = Weapon
  { name         = "iron sword"
  , buildMsg     = "sword is sharp. good protection out in the wilds."
  , cost         = Static [(Wood, 200), (Leather, 50), (Iron, 20)]
  }
getCraftable SteelSword = Weapon
  { name         = "steel sword"
  , buildMsg     = "the steel is strong, and the blade true."
  , cost         = Static [(Wood, 500), (Leather, 100), (Steel, 20)]
  }
getCraftable Rifle = Weapon
  { name         = "rifle"
  , buildMsg     = "black powder and bullets, like the old days."
  , cost         = Static [(Wood, 200), (Steel, 50), (Sulphur, 50)]
  }
--unlockCraftables :: Game -> Game
--unlockCraftables = do
--  where
--        needsWorkshop Hut = False
--        needsWorkshop Lodge = False
--        needsWorkshop TradingPost = False
--        needsWorkshop Tannery = False
--        needsWorkshop Smokehouse = False
--        needsWorkshop Workshop = False
--        needsWorkshop Steelworks = False
--        needsWorkshop Armoury = False
--        needsWorkshop _ = True

canBuildTraps :: Game -> Game
canBuildTraps = execState $ do
  (milestones . trapsUnlocked) .= True
  notifyRoom' ("builder says she can make traps to catch any creatures "
              <> "might still be alive out there.")

canBuildCarts :: Game -> Game
canBuildCarts = execState $ do
  pre <- use (milestones . preCartsUnlocked)
  post <- use (milestones . cartsUnlocked)
  -- unlocking a cart is a 3 stage progress- this takes you from stage 2 -> 3
  when (pre && not post) $ do
    (milestones . cartsUnlocked) .= True
    notifyRoom' "builder says she can make a cart for carrying wood"

buildIfWarm :: Game -> Game -> Game
buildIfWarm buildTheItem game =
  let tooColdToWork = view roomTemperature game == Freezing
                      || view roomTemperature game == Cold
      shiver = game & notifyRoom "builder just shivers."
  in if tooColdToWork then shiver else buildTheItem

buildIfEnoughWood :: Int -> Game -> Game -> Game
buildIfEnoughWood cost buildTheItem game =
  let notEnoughWood = view (stored . wood) game < cost
      showResourceError = game & notifyRoom ("not enough wood (" <> show cost <> ").")
  in if notEnoughWood then showResourceError
     else buildTheItem & over (stored . wood) (subtract cost) & unlockBuildingView

buildItem :: Int -> Game -> Game -> Game
buildItem cost item game = buildIfWarm (buildIfEnoughWood cost item game) game

unlockBuildingView :: Game -> Game
unlockBuildingView game =
  game & set (uiState . showForestBuildings) True

buildTrap :: Game -> Game
buildTrap game =
  let cost = 10 * view (stored . traps) game
      buildTheTrap = game & notifyRoom "more traps to catch more creatures."
                          & over (stored . traps) (+ 1)
      haveEnough = view (stored . traps) buildTheTrap >= maximumNumberOfTraps
      doneMakingTraps = notifyRoom "more traps won't help now."
      builtTrap = if haveEnough then buildTheTrap & doneMakingTraps else buildTheTrap
  in buildItem cost builtTrap game

buildCart :: Game -> Game
buildCart game =
  let cost = 30
      buildTheCart =
        game & notifyRoom "the rickety cart will carry more wood from the forest"
             & over (stored . carts) (+ 1)
  in buildItem cost buildTheCart game
