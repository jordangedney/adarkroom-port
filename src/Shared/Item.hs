{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Shared.Item where

import GHC.Generics
import Data.Yaml

data Item = Fur | Cloth | Scale | Teeth | Bait | Compass | Wood | Hut | Meat | Coal
          | Leather | Iron | Steel | Sulphur
  deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)
