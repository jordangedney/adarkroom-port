{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}

module UIState where

import GHC.Generics
import Data.Yaml

import Control.Lens (makeLenses)
import Brick.Types (Location)

deriving instance ToJSON Location
deriving instance FromJSON Location

data Name
  = StoreVP
  | VP2
  | EventsVP
  | LightButton
  | StokeButton

  | RestartButton
  | SaveButton
  | HyperButton
  | DebugButton
  | PrevButton

  -- Craftables
  | TrapButton
  | CartButton
  | HutButton
  | LodgeButton
  | TradingPostButton
  | TanneryButton
  | SmokehouseButton
  | WorkshopButton
  | SteelworksButton
  | ArmouryButton
  | TorchButton
  | WaterskinButton
  | CaskButton
  | WatertankButton
  | BonespearButton
  | RucksackButton
  | WagonButton
  | ConvoyButton
  | LarmourButton
  | IarmourButton
  | SarmourButton
  | IronswordButton
  | SteelswordButton
  | RifleButton
  deriving (Eq, Show, Enum, Ord, Generic, ToJSON, FromJSON)

data ShowStores = ShowStores
  { _showWood       :: Bool
  , _showScales     :: Bool
  , _showTeeth      :: Bool
  , _showIron       :: Bool
  , _showCoal       :: Bool
  , _showSteel      :: Bool
  , _showMedicine   :: Bool
  , _showBullets    :: Bool
  , _showEnergyCell :: Bool
  , _showBolas      :: Bool
  , _showGrenade    :: Bool
  , _showBayonet    :: Bool
  , _showAlienAlloy :: Bool
  , _showCompass    :: Bool
  } deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)

makeLenses ''ShowStores

showStoresInit :: ShowStores
showStoresInit = ShowStores
  { _showWood       = False
  , _showScales     = False
  , _showTeeth      = False
  , _showIron       = False
  , _showCoal       = False
  , _showSteel      = False
  , _showMedicine   = False
  , _showBullets    = False
  , _showEnergyCell = False
  , _showBolas      = False
  , _showGrenade    = False
  , _showBayonet    = False
  , _showAlienAlloy = False
  , _showCompass    = False
  }

data UIState = UIState
  { _lastReportedClick :: Maybe (Name, Location)
  , _showStores        :: ShowStores
  , _showOutside       :: Bool
  , _showPath          :: Bool
  , _showShip          :: Bool
  } deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)

makeLenses ''UIState

uiStateInit :: UIState
uiStateInit = UIState
  { _lastReportedClick = Nothing
  , _showStores        = showStoresInit
  , _showOutside       = False
  , _showPath          = False
  , _showShip          = False
  }
