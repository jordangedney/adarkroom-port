{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE LambdaCase #-}

module Shared.UI where

import GHC.Generics (Generic)
import Data.Yaml (FromJSON, ToJSON)

import Control.Lens (makeLenses)
import Brick.Types (Location)

import Shared.RandomEvent (SceneChoice)
import Shared.Item (Craftable)

deriving instance ToJSON Location
deriving instance FromJSON Location

data Name
  = NoOpButton

  -- Scrollable (?) windows
  | StoreVP
  | ForestVP
  | EventsVP

  -- Random events
  | ExitEventButton
  | RandomEventButton SceneChoice

  -- Room buttons
  | LightButton
  | StokeButton

  -- Forest buttons
  | GatherButton
  | CheckTrapsButton

  -- User options
  | RestartButton
  | SaveButton
  | HyperButton
  | DebugButton
  | PrevButton
  | PauseButton
  | DialogButton
  | CheatButton

  -- Move between rooms
  | RoomButton
  | OutsideButton
  | PathButton
  | ShipButton

  -- Build and craft buttons
  | CraftButton Craftable
  deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)

data UIState = UIState
  { _lastReportedClick   :: Maybe (Name, Location)
  , _showStores          :: Bool
  , _showOutside         :: Bool
  , _showPath            :: Bool
  , _showShip            :: Bool
  , _showForestBuildings :: Bool

  -- Items
  ,  _showAlienAlloy     :: Bool
  ,  _showBait           :: Bool
  ,  _showBayonet        :: Bool
  ,  _showBolas          :: Bool
  ,  _showBullets        :: Bool
  ,  _showCharm          :: Bool
  ,  _showCloth          :: Bool
  ,  _showCoal           :: Bool
  ,  _showCompass        :: Bool
  ,  _showEnergyCell     :: Bool
  ,  _showFur            :: Bool
  ,  _showGrenade        :: Bool
  ,  _showIron           :: Bool
  ,  _showMeat           :: Bool
  ,  _showMedicine       :: Bool
  ,  _showScales         :: Bool
  ,  _showSteel          :: Bool
  ,  _showTeeth          :: Bool
  ,  _showWood           :: Bool

  -- Craftables
  , _showTrap            :: Bool
  , _showCart            :: Bool
  , _showHut             :: Bool
  , _showLodge           :: Bool
  , _showTradingPost     :: Bool
  , _showTannery         :: Bool
  , _showSmokehouse      :: Bool
  , _showWorkshop        :: Bool
  , _showSteelworks      :: Bool
  , _showArmory          :: Bool
  , _showTorch           :: Bool
  , _showWaterskin       :: Bool
  , _showCask            :: Bool
  , _showWaterTank       :: Bool
  , _showBoneSpear       :: Bool
  , _showRucksack        :: Bool
  , _showWagon           :: Bool
  , _showConvoy          :: Bool
  , _showLeatherArmor    :: Bool
  , _showIronArmor       :: Bool
  , _showSteelArmor      :: Bool
  , _showIronSword       :: Bool
  , _showSteelSword      :: Bool
  , _showRifle           :: Bool

  -- Buttons
  , _showTrapBtn         :: Bool
  , _showCartBtn         :: Bool
  , _showHutBtn          :: Bool
  , _showLodgeBtn        :: Bool
  , _showTradingPostBtn  :: Bool
  , _showTanneryBtn      :: Bool
  , _showSmokehouseBtn   :: Bool
  , _showWorkshopBtn     :: Bool
  , _showSteelworksBtn   :: Bool
  , _showArmoryBtn       :: Bool
  , _showTorchBtn        :: Bool
  , _showWaterskinBtn    :: Bool
  , _showCaskBtn         :: Bool
  , _showWaterTankBtn    :: Bool
  , _showBoneSpearBtn    :: Bool
  , _showRucksackBtn     :: Bool
  , _showWagonBtn        :: Bool
  , _showConvoyBtn       :: Bool
  , _showLeatherArmorBtn :: Bool
  , _showIronArmorBtn    :: Bool
  , _showSteelArmorBtn   :: Bool
  , _showIronSwordBtn    :: Bool
  , _showSteelSwordBtn   :: Bool
  , _showRifleBtn        :: Bool
  } deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)

makeLenses ''UIState

uiStateInit :: UIState
uiStateInit = UIState
  { _lastReportedClick   = Nothing
  , _showStores          = False
  , _showOutside         = False
  , _showPath            = False
  , _showShip            = False
  , _showForestBuildings = False

  -- Items
  , _showAlienAlloy      = False
  , _showBait            = False
  , _showBayonet         = False
  , _showBolas           = False
  , _showBullets         = False
  , _showCharm           = False
  , _showCloth           = False
  , _showCoal            = False
  , _showCompass         = False
  , _showEnergyCell      = False
  , _showFur             = False
  , _showGrenade         = False
  , _showIron            = False
  , _showMeat            = False
  , _showMedicine        = False
  , _showScales          = False
  , _showSteel           = False
  , _showTeeth           = False
  , _showWood            = True

  -- Craftables
  , _showTrap            = False
  , _showCart            = False
  , _showHut             = False
  , _showLodge           = False
  , _showTradingPost     = False
  , _showTannery         = False
  , _showSmokehouse      = False
  , _showWorkshop        = False
  , _showSteelworks      = False
  , _showArmory          = False
  , _showTorch           = False
  , _showWaterskin       = False
  , _showCask            = False
  , _showWaterTank       = False
  , _showBoneSpear       = False
  , _showRucksack        = False
  , _showWagon           = False
  , _showConvoy          = False
  , _showLeatherArmor    = False
  , _showIronArmor       = False
  , _showSteelArmor      = False
  , _showIronSword       = False
  , _showSteelSword      = False
  , _showRifle           = False

  -- Buttons
  , _showTrapBtn         = False
  , _showCartBtn         = False
  , _showHutBtn          = False
  , _showLodgeBtn        = False
  , _showTradingPostBtn  = False
  , _showTanneryBtn      = False
  , _showSmokehouseBtn   = False
  , _showWorkshopBtn     = False
  , _showSteelworksBtn   = False
  , _showArmoryBtn       = False
  , _showTorchBtn        = False
  , _showWaterskinBtn    = False
  , _showCaskBtn         = False
  , _showWaterTankBtn    = False
  , _showBoneSpearBtn    = False
  , _showRucksackBtn     = False
  , _showWagonBtn        = False
  , _showConvoyBtn       = False
  , _showLeatherArmorBtn = False
  , _showIronArmorBtn    = False
  , _showSteelArmorBtn   = False
  , _showIronSwordBtn    = False
  , _showSteelSwordBtn   = False
  , _showRifleBtn        = False

  }
