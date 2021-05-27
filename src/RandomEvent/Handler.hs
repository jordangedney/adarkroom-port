{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module RandomEvent.Handler where

import System.Random (StdGen, randomR)
import Control.Lens (view, over, set, (&))

import GameTypes (Game, stored, fur, tickCount, nextRandomAt, cloth, scales, teeth, inEvent)
import RandomEvent.Event (SceneChoice(..), Item(..), currentScene)
import Util (randomChoice)

shouldDoRandomEvent :: Game -> Bool
shouldDoRandomEvent game = view tickCount game == view nextRandomAt game

doRandomEvent :: StdGen -> Game -> Game
doRandomEvent randomGen game =
  let ticksPerMinute = 10 * 60
      (nextRandom :: Int, gen) =
        randomR (ticksPerMinute * 3, ticksPerMinute * 6) randomGen

      -- availableEvents = [e | e <- allRandomEvents, (isAvailable e game)]

  in game & set nextRandomAt (view tickCount game + nextRandom)

canAfford :: (Item, Int) -> Game -> Bool
canAfford (i, amnt) game =
  let item Fur   = fur
      item Cloth = cloth
      item Scale = scales
      item Teeth = teeth
  in view (stored . item i) game >= amnt

handleButton :: StdGen -> SceneChoice -> Game -> Game
handleButton _ (SceneChoice _ _ Nothing) game =
  game & set inEvent Nothing
handleButton random (SceneChoice txt cost (Just (possibleScenes, defaultNextScene))) game =
  let swapScenes g = case (view inEvent game) of
        Nothing -> g
        Just scene ->
          let next = randomChoice random defaultNextScene possibleScenes
          in g & set inEvent (Just (scene {currentScene = next}))
  in game & over (stored . fur) (subtract 50)
          & swapScenes
