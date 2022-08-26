{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Shared.GameEvent where

import GHC.Generics
import Data.Yaml
import Data.Aeson.Types (ToJSONKey, FromJSONKey)
import qualified Data.Map as Map

data GameEvent
  = UnlockForest
  | GatherWood
  | CheckTraps
  | FireStoked
  | FireShrinking
  | BuilderUpdate
  | RoomChanged
  | BuilderGathersWood
  deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Enum, Bounded)

-- Hacky, but >0 means active, 0 triggers, and <0 means inactive
gameEventsInit :: Map.Map GameEvent Int
gameEventsInit = Map.fromList
  [ (UnlockForest,       -1)
  , (GatherWood,         -1)
  , (CheckTraps,         -1)
  , (FireStoked,         -1)
  , (FireShrinking,       1)
  , (BuilderUpdate,      -1)
  , (RoomChanged,         1)
  , (BuilderGathersWood, -1)
  ]

-- Helper Functions ------------------------------------------------------------
tickEvents :: Map.Map k Int -> Map.Map k Int
tickEvents eventMap = Map.map (+ (-1)) eventMap
  -- let step Inactive = Inactive
  --     step Triggered = Inactive
  --     step (Waiting x) = if x < 1 then Triggered else Waiting (x - 1)
  -- in Map.map step eventMap

isActive :: Maybe Int -> Bool
isActive (Just x) = x > 0
isActive _ = False
