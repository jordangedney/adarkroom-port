module Fire
  ( light
  , stoke
  , shrinking
  )
where

import Control.Lens (over, set, view, (&))

import GameTypes (FireState(..), Game,
                  upcomingEvents, fireValue, fireLit, milestones, stored, wood)
import GameEvent (GameEvent(FireShrinking, FireStoked, RoomChanged),
                  updateEvents)
import Constants (fireCoolDelay, stokeCooldown, roomWarmDelay)

import qualified Room
import qualified Builder


-- Defined in GameTypes to avoid an import cycle:
-- data FireState
--   = Dead
--   | Smouldering
--   | Flickering
--   | Burning
--   | Roaring

fireState :: FireState -> String
fireState Dead        = "the fire is dead."
fireState Smouldering = "the fire is smouldering."
fireState Flickering  = "the fire is flickering."
fireState Burning     = "the fire is burning."
fireState Roaring     = "the fire is roaring."

firePred :: FireState -> FireState
firePred Dead = Dead
firePred x = pred x

fireSucc :: FireState -> FireState
fireSucc Roaring = Roaring
fireSucc x = succ x

fireChanged :: Game -> Game
fireChanged game =
  let showFire = game & Room.notify (fireState (view fireValue game))
      fireIsBurning = view fireValue game /= Dead
      fireContinuesBurning =
        showFire & over upcomingEvents (updateEvents (FireShrinking fireCoolDelay))
  in if fireIsBurning then fireContinuesBurning else showFire

firstLight :: Game -> Game
firstLight game =
  let doNothing = game
      fireHasBeenLitBefore = view (milestones . fireLit) game
      builderIsOnTheWay =
        game & set (milestones . fireLit) True
             & Builder.update
  in if fireHasBeenLitBefore then doNothing else builderIsOnTheWay

light :: Game -> Game
light game =
  let withLitFire =
        game & set fireValue Burning
             & over (stored . wood) (subtract 5)
             & over upcomingEvents (updateEvents (FireStoked stokeCooldown))
             & fireChanged
             & firstLight
      withUnlitFire =
        game & Room.notify "not enough wood to get the fire going."
      enoughWood = view (stored . wood) game > 4
  in if enoughWood then withLitFire else withUnlitFire

stoke :: Game -> Game
stoke game =
  let withStokedFire =
        game & over fireValue fireSucc
             & over (stored . wood) (subtract 1)
             & over upcomingEvents (updateEvents (FireStoked stokeCooldown))
             & fireChanged
      withUnstokedFire =
        game & Room.notify "the wood has run out."
      enoughWood = view (stored . wood) game > 0
  in if enoughWood then withStokedFire else withUnstokedFire

shrinking :: Game -> Game
shrinking game =
  game & over fireValue firePred
       & fireChanged
