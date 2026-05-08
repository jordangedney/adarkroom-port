{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Shared.Rewards where

import GHC.Generics
import Data.Yaml
import qualified Data.Map as Map
import Control.Lens (makeLenses)

import Shared.Item

-- Whether the bottom-right exit button reads "leave" (e.g. after combat —
-- you head home) or "continue" (e.g. after exploring a place — you keep
-- pressing on into the next area).
data RewardsExit = LeaveExit | ContinueExit
  deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)

-- The reusable rewards screen — populated by combat (beast death) or
-- place exploration (room loot). The screen is a modal overlay on the
-- Path map; while it's up, the rest of the UI greys out.
data RewardsScreen = RewardsScreen
  { _rewardsTitle  :: String
  , _rewardsText   :: [String]
  , _rewardsItems  :: Map.Map Item Int
  , _rewardsCanEat :: Bool
  , _rewardsExit   :: RewardsExit
  } deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)

makeLenses ''RewardsScreen
