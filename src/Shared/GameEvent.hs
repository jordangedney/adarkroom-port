{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Shared.GameEvent where

import GHC.Generics
import Data.Yaml
import Data.Aeson.Types (ToJSONKey, FromJSONKey)
import qualified Data.Map as Map
import Control.Lens ((&))

data GameEvent
  = UnlockForest
  | GatherWood
  | CheckTraps
  | FireStoked
  | FireShrinking
  | BuilderUpdate
  | RoomChanged
  | BuilderGathersWood
  | Random
  deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Enum, Bounded)

-- Hacky, but >0 means active, 0 triggers, and <0 means inactive
gameEventsInit :: Map.Map GameEvent Int
gameEventsInit = Map.fromList [(e, -1) | e <- enumFrom (toEnum 0)]
  & Map.insert FireShrinking 1
  & Map.insert RoomChanged 1
  & Map.insert Random 1

-- Helper Functions ------------------------------------------------------------
tickEvents :: Map.Map k Int -> Map.Map k Int
tickEvents = Map.map (+ (-1))

isActive :: Maybe Int -> Bool
isActive (Just x) = x > 0
isActive _ = False
