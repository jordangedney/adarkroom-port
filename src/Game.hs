module Game where

import GameTypes (Game)
import GameEvent (GameEvent(..))

import qualified Fire
import qualified Outside
import qualified Builder
import qualified Room

getGameEvent :: (GameEvent, Int) -> Game -> Game
getGameEvent (UnlockForest,  _) = Outside.unlock
getGameEvent (GatherWood,    _) = id
getGameEvent (FireStoked,    _) = id
getGameEvent (FireShrinking, _) = Fire.shrinking
getGameEvent (BuilderUpdate, _) = Builder.update
getGameEvent (RoomChanged,   _) = Room.update
