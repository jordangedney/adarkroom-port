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
  | ForestVP
  | EventsVP
  | LightButton
  | StokeButton
  | NoOpButton
  | DialogButton

  | GatherButton
  | CheckTrapsButton

  | RestartButton
  | SaveButton
  | HyperButton
  | DebugButton
  | PrevButton
  | PauseButton

  | RoomButton
  | OutsideButton
  | PathButton
  | ShipButton

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

data ShowItems = ShowItems
  { _showWood       :: Bool
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
  , _showBait       :: Bool
  , _showFur        :: Bool
  , _showMeat       :: Bool
  , _showScales     :: Bool
  , _showTeeth      :: Bool
  , _showCloth      :: Bool
  , _showCharm      :: Bool
  } deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)

makeLenses ''ShowItems

showItemsInit :: ShowItems
showItemsInit = ShowItems
  { _showWood       = True
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
  , _showBait       = False
  , _showFur        = False
  , _showMeat       = False
  , _showScales     = False
  , _showTeeth      = False
  , _showCloth      = False
  , _showCharm      = False
  }

data UIState = UIState
  { _lastReportedClick            :: Maybe (Name, Location)
  , _showItems                    :: ShowItems
  , _showStores                   :: Bool
  , _showOutside                  :: Bool
  , _showPath                     :: Bool
  , _showShip                     :: Bool
  , _showForestBuildings          :: Bool
  , _dialogBox                    :: Bool
  } deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)

makeLenses ''UIState

uiStateInit :: UIState
uiStateInit = UIState
  { _lastReportedClick   = Nothing
  , _showItems           = showItemsInit
  , _showStores          = False
  , _showOutside         = False
  , _showPath            = False
  , _showShip            = False
  , _showForestBuildings = False
  , _dialogBox           = True
  }
