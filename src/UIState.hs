module UIState where

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
  { showWood :: Bool
  , showScales :: Bool
  , showTeeth :: Bool
  , showIron :: Bool
  , showCoal :: Bool
  , showSteel :: Bool
  , showMedicine :: Bool
  , showBullets :: Bool
  , showEnergyCell :: Bool
  , showBolas :: Bool
  , showGrenade :: Bool
  , showBayonet :: Bool
  , showAlienAlloy :: Bool
  , showCompass :: Bool
  } deriving (Show)

showStoresInit = ShowStores
  False False False False False False False False False False False False False False

data UIState = UIState
  { lastReportedClick :: Maybe (Name, T.Location)
  , showStores :: ShowStores
  , showOutside :: Bool
  } deriving (Show)
