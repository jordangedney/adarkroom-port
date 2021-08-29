{-# LANGUAGE ScopedTypeVariables #-}


module RandomEvent.Handler where

import System.Random (StdGen, randomR)
import Control.Lens (view, over, set, (&))

import GameTypes (Game, stored, fur, tickCount, nextRandomAt, cloth, scales,
                  teeth, inEvent, location, Location(..), hyperspeedAmt)
import RandomEvent.Event (SceneChoice(..), Item(..), currentScene,
                          Scene, theBeggar)
import Util (randomChoice)

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
  in case availableEvents game of
      Just es -> updated & set inEvent (Just (head es))
      Nothing -> updated

canAfford :: (Item, Int) -> Game -> Bool
canAfford (i, amnt) game =
  let itm Fur   = fur
      itm Cloth = cloth
      itm Scale = scales
      itm Teeth = teeth
  in view (stored . itm i) game >= amnt

-- getItem :: Item -> Game ->
item :: Functor f => Item -> (Int -> f Int) -> Game -> f Game
item Fur   = stored . fur
item Cloth = stored . cloth
item Scale = stored . scales
item Teeth = stored . teeth

handleButton :: StdGen -> SceneChoice -> Game -> Game
handleButton _ (SceneChoice _ _ Nothing) game =
  game & set inEvent Nothing
handleButton random (SceneChoice _ _ (Just (possibleScenes, defaultNextScene))) game =
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
        [(theBeggar, view location g == Room && view (stored . fur) g > 0)]
      avail = [e | (e, p) <- events, p]
  in if null avail then Nothing else Just avail
