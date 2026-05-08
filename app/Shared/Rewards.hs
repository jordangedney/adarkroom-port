{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Shared.Rewards where

import GHC.Generics
import Data.Yaml
import qualified Data.Map as Map

import Control.Lens (makeLenses)

import Shared.Item (Item)

-- Different callers need different button sets:
--   RewardsCombat      → shows "eat meat" + "leave"
--   RewardsExploration → shows "continue" + "leave"
data RewardsContext
  = RewardsCombat
  | RewardsExploration
  deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)

data Rewards = Rewards
  { _rewardsText      :: String
  , _rewardsAvailable :: Map.Map Item Int
  , _rewardsContext   :: RewardsContext
  } deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)

makeLenses ''Rewards
