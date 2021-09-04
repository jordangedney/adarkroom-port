{-# LANGUAGE ScopedTypeVariables #-}

module RandomEvent.Handler where

import Data.Maybe (isJust)
import System.Random (StdGen, randomR)
import Control.Lens (view, over, set, (&))

import GameTypes (Game, stored, fur, tickCount, nextRandomAt, cloth, scales,
                  teeth, inEvent, location, Location(..), hyperspeedAmt, bait, compass, Stored)
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

item :: Functor f => Item -> (Int -> f Int) -> Stored -> f Stored
item Fur   = fur
item Cloth = cloth
item Scale = scales
item Teeth = teeth
item Bait = bait
item Compass = compass

itemToStr :: Item -> String
itemToStr Fur     = "fur"
itemToStr Cloth   = "cloth"
itemToStr Scale   = "scales"
itemToStr Teeth   = "teeth"
itemToStr Bait    = "bait"
itemToStr Compass = "compass"

canAfford' :: (Item, Int) -> Game -> Bool
-- Prevent players from buying more than one compass
canAfford' (Compass, 0) game = view (stored . compass) game == 0
canAfford' (i, amnt) game = view (stored . item i) game >= amnt

canAfford :: [(Item, Int)] -> Game -> Bool
canAfford items game = all (`canAfford'` game) items

getItem :: Functor f => Item -> (Int -> f Int) -> Game -> f Game
getItem i = stored . item i

handleButton :: StdGen -> SceneChoice -> Game -> Game
handleButton _ (SceneChoice _ _ Nothing) game =
  game & set inEvent Nothing
handleButton _ (SceneChoice _ choiceCost (Just (Stay notification (rItem, rAmt))))  game =
  let loot = case choiceCost of
        Nothing -> game
        Just xs -> if canAfford xs game
                  then foldl (\g (i, amt) -> g & over (getItem i) (subtract amt)) game xs
                       & notify
                       & over (getItem rItem) (+ rAmt)
                  else if view (stored . compass) game == 1
                       then game
                       else costMsg xs game
      notify g = case notification of
        Nothing -> g
        Just alert -> notifyRoom alert g
      costMsg [] g = g
      -- Handle the hidden requirement allowing players to only buy one compass
      costMsg ((Compass, 0):xs) g = costMsg xs g
      costMsg ((i, cost'):xs) g =
        notifyRoom ("not enough " <> itemToStr i <> " (" <> show cost' <> ").") g
        & costMsg xs
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
