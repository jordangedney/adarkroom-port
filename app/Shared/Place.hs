{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

-- | Pure data types for the explorable places on the path. Scene definitions
-- and exploration state-machine logic live in 'Path.Place'; this module just
-- carries the type used by 'Shared.Game'.
module Shared.Place
  ( Place(..)
  , PlaceState(..)
  , psPlace
  , psTile
  , psStep
  ) where

import GHC.Generics (Generic)
import Data.Yaml (FromJSON, ToJSON)
import Control.Lens (makeLenses)

data Place
  = DampCave
  | IronMine
  | Outpost
  | OldHouse
  | CoalMine
  | DesertedTown
  | SulfurMine
  | Battlefield
  | RuinedCity
  | HugeBorehole
  | CrashedShip
  | MurkySwamp
  deriving (Eq, Show, Ord, Enum, Bounded, Generic, ToJSON, FromJSON)

-- | While exploring a place we just track which place, the map tile we
-- entered from, and how many of its scripted events have already fired.
-- The events list itself is static per place so we don't serialise it.
data PlaceState = PlaceState
  { _psPlace :: Place
  , _psTile  :: (Int, Int)
  , _psStep  :: Int
  } deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)

makeLenses ''PlaceState
