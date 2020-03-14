{-# LANGUAGE TupleSections #-}

module GameUtil where
-- For game manipulation utilities
-- fns are placed here to avoid import cycles,
-- and to keep the global game state free of partial functions (serializable)

import Control.Lens (over, set, view, (&))

import GameTypes (Game, Location(Room), events, location, roomEventBacklog)

addEvent :: String -> Game -> Game
addEvent message game =
  game & over events ((message, 0):)

notifyRoom :: String -> Game -> Game
notifyRoom message game =
  if view location game == Room
  then game & addEvent message
  else game & over roomEventBacklog ((:) message)

clearRoomBacklog :: Game -> Game
clearRoomBacklog game =
  game & over events (\es ->  map (, 0) (view roomEventBacklog game) ++ es)
       & set roomEventBacklog []
