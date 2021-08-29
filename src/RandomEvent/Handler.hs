{-# LANGUAGE ScopedTypeVariables #-}

module RandomEvent.Handler where

import Data.Maybe (isJust)
import System.Random (StdGen, randomR)
import Control.Lens (view, over, set, (&))

import GameTypes (Game, stored, fur, tickCount, nextRandomAt, cloth, scales,
                  teeth, inEvent, location, Location(..), hyperspeedAmt, bait, compass)
import RandomEvent.Event (SceneChoice(..), Item(..), currentScene,
                          Scene, theBeggar, theNomad, StayOrGo(..))
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
            Just es -> updated & set inEvent (Just (choice randomGen es))
            Nothing -> updated

item Fur   = fur
item Cloth = cloth
item Scale = scales
item Teeth = teeth
item Bait = bait
item Compass = compass

canAfford' :: (Item, Int) -> Game -> Bool
canAfford' (i, amnt) game = view (stored . item i) game >= amnt

canAfford :: [(Item, Int)] -> Game -> Bool
canAfford items game = all (`canAfford'` game) items

getItem :: Functor f => Item -> (Int -> f Int) -> Game -> f Game
getItem i = stored . item i

handleButton :: StdGen -> SceneChoice -> Game -> Game
handleButton _ (SceneChoice _ _ Nothing) game =
  game & set inEvent Nothing
handleButton _ (SceneChoice _ choiceCost (Just (Stay notification (rItem, rAmt))))  game =
  let pay = case choiceCost of
        Nothing -> game
        Just xs -> foldl (\g (i, amt) -> g & over (getItem i) (subtract amt)) game xs
      notify = case notification of
        Nothing -> pay
        Just alert -> notifyRoom alert pay
      loot = notify & over (getItem rItem) (+ rAmt)
  in loot

handleButton random (SceneChoice _ _ (Just (Go (possibleScenes, defaultNextScene)))) game =
  let swapScenes g = case view inEvent game of
        Nothing -> g
        Just scene ->
          let next = randomChoice random defaultNextScene possibleScenes
          in g & set inEvent (Just (scene {currentScene = next}))
  in game & over (stored . fur) (subtract 50)
          & swapScenes

availableEvents :: Game -> Maybe [Scene]
availableEvents g =
  let events =
        [(theBeggar, view location g == Room && view (stored . fur) g > 0)
        ,(theNomad, view location g == Room && view (stored . fur) g > 0)
        ]
      avail = [e | (e, p) <- events, p]
  in if null avail then Nothing else Just avail
