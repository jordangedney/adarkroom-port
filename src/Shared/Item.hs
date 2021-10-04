{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Shared.Item where

import GHC.Generics
import Data.Yaml

data Item = Fur | Cloth | Scale | Teeth | Bait | Compass | Wood | Hut
  deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)
