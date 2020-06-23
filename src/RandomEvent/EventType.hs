{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module RandomEvent.EventType where

import GHC.Generics
import Data.Yaml

data RandomEventChoice
  = FurBeggarFifty
  | FurBeggarHundred
  | End
  deriving (Eq, Enum, Show, Ord, Generic, ToJSON, FromJSON)

data RandomEvent
  = TheBeggar
  deriving (Eq, Enum, Show, Ord, Generic, ToJSON, FromJSON)

allEvents :: [RandomEvent]
allEvents = enumFrom (toEnum 0)
