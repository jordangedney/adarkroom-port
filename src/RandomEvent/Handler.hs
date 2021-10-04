{-# LANGUAGE ScopedTypeVariables #-}

module RandomEvent.Handler where

import Data.Maybe (isJust)
import System.Random (StdGen, randomR)
import Control.Lens (view, over, set, (&))

import Shared.Game (Game, stored, tickCount, nextRandomAt,
                   inEvent, hyperspeedAmt, compass)

import Shared.Event (SceneChoice(..), currentScene,
                          Scene,
                          StayOrGo(..),
                          SceneEvent(..), Reward(..))
import Shared.Item
import Shared.Util

import RandomEvent.Room

import Util (randomChoice, choice)
import GameUtil (notifyRoom)

availableEvents :: Game -> [Scene]
availableEvents g = [e | (e, p) <- evs, p]
  where evs = RandomEvent.Room.events g

shouldDoRandomEvent :: Game -> Bool
shouldDoRandomEvent game = view tickCount game == view nextRandomAt game

setNextRandomEvent :: Game -> StdGen -> Game
setNextRandomEvent game randomGen =
  let ticksPerMinute = 10 * 60
      (nextRandom' :: Int, _) =
        randomR (ticksPerMinute * 3, ticksPerMinute * 6) randomGen
      hs = view hyperspeedAmt game
      nextRandom = nextRandom' + (hs - (nextRandom' `mod` hs))
  in game & set nextRandomAt (view tickCount game + nextRandom)

doRandomEvent :: Game -> StdGen -> Game
doRandomEvent game randomGen =
  let updated = setNextRandomEvent game randomGen
  in if isJust (view inEvent game) then updated
     else case availableEvents game of
            [] -> updated
            es -> let ev = choice randomGen es
                 in updated & set inEvent (Just ev) & addReward (currentScene ev)

getItem :: Functor f => Item -> (Int -> f Int) -> Game -> f Game
getItem i = stored . item i

doSceneChoice :: StdGen -> Maybe StayOrGo -> Game -> Game
doSceneChoice _ Nothing game = game & set inEvent Nothing
doSceneChoice _ (Just (Stay alert (rItem, rAmt))) game =
  let loot g = g & over (getItem rItem) (+ rAmt)
      notify g = case alert of
        Nothing -> g
        Just a -> notifyRoom a g
  in game & loot & notify
doSceneChoice random (Just (Go (possibleScenes, defaultNextScene))) game =
  case view inEvent game of
    Nothing -> game
    Just scene ->
      let next = randomChoice random defaultNextScene possibleScenes
      in game & set inEvent (Just (scene {currentScene = next}))
              & addReward next

addReward :: SceneEvent -> Game -> Game
addReward (SceneEvent _ _ None _) game = game
addReward (SceneEvent _ _ (Give xs) _) game =
  foldl (\g (i, amt) -> g & over (getItem i) (+ amt)) game xs
addReward (SceneEvent _ _ (GiveSome xs) _) game =
  let go :: Game -> (Item, Int, Item, Int) -> Game
      go g (toRemove, removePercent, toAdd, addPercent) =
        let numAvail' = fromIntegral (view (getItem toRemove) g)
                        & (* (fromIntegral removePercent * 0.01 :: Double))
                        & floor
            numAvail = if numAvail' == 0 then 1 else numAvail'
            numAdd' = fromIntegral numAvail
                      & (* (fromIntegral addPercent * 0.01 :: Double))
                      & floor
            numAdd = if numAdd' == 0 then 1 else numAdd'
        in g & over (getItem toRemove) (subtract numAvail)
             & over (getItem toAdd) (+ numAdd)
  in foldl go game xs

handleButton :: StdGen -> SceneChoice -> Game -> Game
handleButton r (SceneChoice _ [] next) game = doSceneChoice r next game
handleButton r (SceneChoice _ cs next) game =
  if canAfford cs game
  then foldl (\g (i, amt) -> g & over (getItem i) (subtract amt)) game cs
       & doSceneChoice r next
  else if tryingToBuyCompassTwice cs game
       then game
       else costMsg cs game
  where
    tryingToBuyCompassTwice costs g =
      (filter (\(c, amt) -> c == Compass && amt == 0) costs & null & not)
      && ((view (stored . compass) g) == 1)

    costMsg [] g = g
    -- Handle the hidden requirement allowing players to only buy one compass
    costMsg ((Compass, 0):xs) g = costMsg xs g
    costMsg ((i, cost'):xs) g =
      notifyRoom ("not enough " <> itemToStr i <> " (" <> show cost' <> ").") g
      & costMsg xs
