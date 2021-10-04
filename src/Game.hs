module Game where

import Shared.Game (Game)
import Shared.GameEvent (GameEvent(..))

import qualified Room.Fire as Fire
import qualified Room.Builder as Builder
import qualified Room.Room as Room

import qualified Outside

getGameEvent :: GameEvent -> Game -> Game
getGameEvent UnlockForest       = Outside.unlock
getGameEvent FireShrinking      = Fire.shrinking
getGameEvent BuilderUpdate      = Builder.update
getGameEvent BuilderGathersWood = Builder.gatherWood
getGameEvent UnlockTraps        = Builder.canBuildTraps
getGameEvent RoomChanged        = Room.update

-- Button Cooldowns
getGameEvent GatherWood         = id
getGameEvent FireStoked         = id
getGameEvent CheckTraps         = id
