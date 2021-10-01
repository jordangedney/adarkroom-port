{-# LANGUAGE ScopedTypeVariables #-}

module RandomEvent.Handler where

import Data.Maybe (isJust)
import System.Random (StdGen, randomR)
import Control.Lens (view, over, set, (&))

import GameTypes (Game, stored, fur, tickCount, nextRandomAt, cloth, scales,
                  teeth, inEvent, location, Location(..), hyperspeedAmt, bait,
                  compass, Stored, wood)
import RandomEvent.Event (SceneChoice(..), Item(..), currentScene,
                          Scene, theBeggar, theNomad, noisesOutside, noisesInside,
                          StayOrGo(..),
                          SceneEvent(..), Reward(..))
import Util (randomChoice, choice)
import GameUtil (notifyRoom)

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
            Just es ->
              let ev = choice randomGen es
              in updated & set inEvent (Just ev) & addReward (currentScene ev)
            Nothing -> updated

item :: Functor f => Item -> (Int -> f Int) -> Stored -> f Stored
item Fur   = fur
item Cloth = cloth
item Scale = scales
item Teeth = teeth
item Bait = bait
item Compass = compass
item Wood = wood

itemToStr :: Item -> String
itemToStr Fur     = "fur"
itemToStr Cloth   = "cloth"
itemToStr Scale   = "scales"
itemToStr Teeth   = "teeth"
itemToStr Bait    = "bait"
itemToStr Compass = "compass"
itemToStr Wood = "wood"

canAfford :: [(Item, Int)] -> Game -> Bool
canAfford items game = all afford items
  where afford (Compass, 0) = view (stored . compass) game == 0
        afford (i, amnt) = view (stored . item i) game >= amnt

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
handleButton r (SceneChoice _ Nothing next) game = doSceneChoice r next game
handleButton r (SceneChoice _ (Just cs) next) game =
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


availableEvents :: Game -> Maybe [Scene]
availableEvents g =
  let events =
        [ (theBeggar,     playerIn Room && playerHasFound fur)
        , (theNomad,      playerIn Room && playerHasFound fur)
        , (noisesOutside, playerIn Room && playerHasFound wood)
        , (noisesInside,  playerIn Room && playerHasFound wood)
        ]
      avail = [e | (e, p) <- events, p]
      playerHasFound i = view (stored . i) g > 0
      playerIn x = view location g == x
  in if null avail then Nothing else Just avail
