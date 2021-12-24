module Room.Fire
  ( light
  , stoke
  , shrinking
  )
where

import Control.Lens (over, set, view, (&))

import Shared.Game
import Shared.GameEvent (GameEvent(FireShrinking, FireStoked))
import Shared.Constants (fireCoolDelay, stokeCooldown)
import Util (notifyRoom, updateEvents)

import qualified Room.Builder as Builder
import Shared.Util (getItem, overItem)
import Shared.Item (Item(Wood))

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
  let showFire g = g & notifyRoom (fireState (view fireValue g))
      showDeadFire = showFire game

      fireIsBurning = view fireValue game /= Dead
      builderCanStoke = view builderState game ==
        Helping && getItem Wood game > 0
      builderShouldStoke = builderCanStoke && view fireValue game == Smouldering

      theFire =
        if builderShouldStoke
        then game & overItem Wood (+ (-1))
                  & over fireValue fireSucc
                  & notifyRoom "builder stokes the fire."
        else game

      burnOn =
        theFire
        & updateEvents FireShrinking fireCoolDelay
        & showFire

  in if fireIsBurning then burnOn else showDeadFire

firstLight :: Game -> Game
firstLight game =
  let doNothing = game
      fireHasBeenLitBefore = view (milestones . fireLit) game
      builderIsOnTheWay =
        game & set (milestones . fireLit) True
             & Builder.approach
  in if fireHasBeenLitBefore then doNothing else builderIsOnTheWay

light :: Game -> Game
light game =
  let withLitFire =
        game & set fireValue Burning
             & overItem Wood (+ (-5))
             & updateEvents FireStoked stokeCooldown
             & fireChanged
             & firstLight
      withUnlitFire =
        game & notifyRoom "not enough wood to get the fire going."
      enoughWood = getItem Wood game > 4
  in if enoughWood then withLitFire else withUnlitFire

stoke :: Game -> Game
stoke game =
  let withStokedFire =
        game & over fireValue fireSucc
             & overItem Wood (+ (-1))
             & updateEvents FireStoked stokeCooldown
             & fireChanged
      withUnstokedFire =
        game & notifyRoom "the wood has run out."
      enoughWood = getItem Wood game > 0
  in if enoughWood then withStokedFire else withUnstokedFire

shrinking :: Game -> Game
shrinking game =
  game & over fireValue firePred
       & fireChanged
