{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Shared.Worker where

import GHC.Generics
import Data.Yaml
import Data.Aeson.Types (ToJSONKey, FromJSONKey)

data Worker
  = Gatherer
  | Hunter
  | Trapper
  | Tanner
  | Charcutier
  | IronMiner
  | CoalMiner
  | SulphurMiner
  | Steelworker
  | Armourer
  deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON, ToJSONKey, FromJSONKey)
