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
import Shared.Item (Item)

deriving instance ToJSON Location
deriving instance FromJSON Location

data Name
  -- Scrollable (?) windows
  = StoreVP
  | ForestVP
  | EventsVP

  -- Random events
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
  | CraftButton Item
  deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)

data UIState = UIState
  { _lastReportedClick   :: Maybe (Name, Location)
  , _showStores          :: Bool
  , _showOutside         :: Bool
  , _showPath            :: Bool
  , _showShip            :: Bool
  , _showForestBuildings :: Bool

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
