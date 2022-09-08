{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module RandomEvent where

import Data.Maybe (isNothing)
import System.Random (StdGen, randomR)
import Control.Lens

import Shared.GameEvent (GameEvent(Random))
import Shared.Game
import Shared.RandomEvent
import Shared.Item
import Shared.Util

import Room.Event

import Util (randomChoice, choice, notifyRoom, displayCosts, updateEvent)
import Control.Monad.State (get, gets)
import Control.Monad (unless, forM_, when)

start :: StdGen -> DarkRoom
start stdGen = do
  -- set up the next random event
  let ticksPerMinute = 10 * 60
      (nextRandom :: Int, stdGen') = randomR (ticksPerMinute * 3, ticksPerMinute * 6) stdGen
  updateEvent Random nextRandom

  -- jump into an available event if not in one
  needEvent <- isNothing <$> use inEvent
  availEvs <- gets (map fst . filter snd . Room.Event.events)

  when (needEvent && not (null availEvs)) $ do
    let ev = choice stdGen' availEvs
    inEvent .= Just ev
    addReward (currentScene ev)

-- TODO SOME QA Round this func
addReward :: SceneEvent -> DarkRoom
addReward (SceneEvent _ _ r _) = case r of
  -- pretty shity reward
  None -> do pure () -- TODO this right?

  -- straight shooter
  (Give xs) -> do
    forM_ xs $ \(i, amt) -> do
      overStored i (+ amt)

  -- I'll give you 5 fur for every 10 wood... or something
  (GiveSome xs) -> do
    forM_ xs $ \(toRemove, removePercent, toAdd, addPercent) -> do
      let takePercent x n = fromIntegral n
                          & (* (fromIntegral x * 0.01 :: Double))
                          & floor
                          & (\y -> if y == 0 then 1 else y)
      numAvail <- takePercent removePercent <$> getStored toRemove
      let numAdd = takePercent addPercent numAvail

      overStored toRemove (subtract numAvail)
      overStored toAdd (+ numAdd)

doSceneChoice :: StdGen -> Maybe StayOrGo -> DarkRoom
doSceneChoice rnd = \case
  -- go home
  Nothing -> do inEvent .= Nothing

  -- stay and buy something!
  Just (Stay alert (rItem, rAmt)) -> do
    overStored rItem (+ rAmt)
    forM_ alert $ \a -> do notifyRoom a

  -- time marches ever forward
  Just (Go (possibleScenes, defaultNextScene)) -> do
    ev <- use inEvent
    forM_ ev $ \scene -> do
      let next = randomChoice rnd defaultNextScene possibleScenes
      inEvent .= Just (scene {currentScene = next})
      addReward next

handleButton :: StdGen -> SceneChoice -> DarkRoom
handleButton r (SceneChoice _ cs next) =
  -- if there's no item to buy just continue on
  if null cs then doSceneChoice r next

  else do
    -- buy the item, if you can
    game <- get
    if canAfford cs game then do
      forM_ cs $ \(i, amt) -> do
        overStored i (subtract amt)
        doSceneChoice r next

    -- else print the cost
    else do
      haveCompass <- (== 1) <$> getStored Compass
      -- prevent players from buying 2 compasses
      unless (haveCompass && (Compass, 0) `elem` cs) (displayCosts cs)
