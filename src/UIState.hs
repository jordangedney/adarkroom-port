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

data UIState = UIState
  { _lastReportedClick :: Maybe (Name, T.Location)
  } deriving (Show)
