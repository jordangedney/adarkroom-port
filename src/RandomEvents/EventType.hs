{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module RandomEvents.EventType where

import GHC.Generics
import Data.Yaml

data RandomEvent
  = BeggarEvent
  deriving (Eq, Enum, Show, Ord, Generic, ToJSON, FromJSON)

allEvents :: [RandomEvent]
allEvents = enumFrom (toEnum 0)
