module UIState where

import qualified Brick.Types as T
import qualified Brick.Widgets.Edit as E

data Name = StoreVP
          | VP2
          | EventsVP
          | StokeFireButton
  deriving (Eq, Show, Ord)

data UIState = UIState
  { _lastReportedClick :: Maybe (Name, T.Location)
  } deriving (Show)
