{-# LANGUAGE TemplateHaskell #-}

module UIState where

import Control.Lens (makeLenses, set)

import qualified Brick.Types as T
import qualified Brick.Widgets.Edit as E

data Name = StoreVP
          | VP2
          | EventsVP
          | LightButton
          | StokeButton

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
  deriving (Eq, Show, Ord)

data ShowStores = ShowStores
  { _showWood :: Bool
  , _showScales :: Bool
  , _showTeeth :: Bool
  , _showIron :: Bool
  , _showCoal :: Bool
  , _showSteel :: Bool
  , _showMedicine :: Bool
  , _showBullets :: Bool
  , _showEnergyCell :: Bool
  , _showBolas :: Bool
  , _showGrenade :: Bool
  , _showBayonet :: Bool
  , _showAlienAlloy :: Bool
  , _showCompass :: Bool
  } deriving (Show, Eq, Ord)

showStoresInit = ShowStores
  False False False False False False False False False False False False False False

data UIState = UIState
  { _lastReportedClick :: Maybe (Name, T.Location)
  , _showStores :: ShowStores
  , _showOutside :: Bool
  , _showPath :: Bool
  , _showShip :: Bool
  } deriving (Show, Eq, Ord)

makeLenses ''ShowStores
makeLenses ''UIState
