{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE LambdaCase #-}

module Shared.UI where

import GHC.Generics (Generic)
import Data.Yaml (FromJSON, ToJSON)
import Data.Map (Map, fromList)

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
  , _showItemButton      :: Map Item Bool
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
  , _showItemButton      = fromList []
  }
