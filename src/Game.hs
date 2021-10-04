module Game where

import Shared.Game (Game)
import Shared.GameEvent (GameEvent(..))

import qualified Fire
import qualified Outside
import qualified Builder
import qualified Room

getGameEvent :: (GameEvent, Int) -> Game -> Game
getGameEvent (UnlockForest,       _) = Outside.unlock
getGameEvent (FireShrinking,      _) = Fire.shrinking
getGameEvent (BuilderUpdate,      _) = Builder.update
getGameEvent (BuilderGathersWood, _) = Builder.gatherWood
getGameEvent (UnlockTraps,        _) = Builder.canBuildTraps
getGameEvent (RoomChanged,        _) = Room.update

-- Button Cooldowns
getGameEvent (GatherWood,         _) = id
getGameEvent (FireStoked,         _) = id
getGameEvent (CheckTraps,         _) = id
