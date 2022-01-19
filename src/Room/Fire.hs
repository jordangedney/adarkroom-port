{-# LANGUAGE LambdaCase #-}

module Room.Fire
  ( light
  , stoke
  , shrinking
  )
where

import Control.Lens

import Shared.Game
import Shared.GameEvent (GameEvent(FireShrinking, FireStoked))
import Shared.Constants (fireCoolDelay, stokeCooldown)
import Util (notifyRoom, updateEvent)

import qualified Room.Builder as Builder
import Shared.Util (getStored, overStored, overStored)
import Shared.Item (Item(Wood))
import Control.Monad (unless, when)

showState :: FireState -> String
showState = \case
  Dead        -> "the fire is dead."
  Smouldering -> "the fire is smouldering."
  Flickering  -> "the fire is flickering."
  Burning     -> "the fire is burning."
  Roaring     -> "the fire is roaring."

firePred :: FireState -> FireState
firePred = \case { Dead -> Dead; x -> pred x }

fireSucc :: FireState -> FireState
fireSucc = \case { Roaring -> Roaring; x -> succ x }

fireChanged :: DarkRoom
fireChanged = do
  fs <- use fireValue

  when (fs /= Dead) $ do
    haveWood <- (> 0) <$> getStored Wood
    bs <- use builderState

    when (fs == Smouldering && haveWood && bs == Helping) $ do
      overStored Wood (+ (-1))
      fireValue %= fireSucc
      notifyRoom "builder stokes the fire."

    updateEvent FireShrinking fireCoolDelay

  notifyRoom (showState fs)

light :: DarkRoom
light = do
  canLight <- (> 4) <$> getStored Wood

  if canLight then do
    fireValue .= Burning
    overStored Wood (+ (-5))
    updateEvent FireStoked stokeCooldown
    fireChanged

    fireHasBeenLitBefore <- use (milestones . fireLit)
    unless fireHasBeenLitBefore $ do
      (milestones . fireLit) .= True
      Builder.approach

  else do
    notifyRoom "not enough wood to get the fire going."


stoke :: DarkRoom
stoke = do
  haveWood <- (> 0) <$> getStored Wood
  if haveWood then do
    fireValue %= fireSucc
    overStored Wood (+ (-1))
    updateEvent FireStoked stokeCooldown
    fireChanged
  else do
    notifyRoom "the wood has run out."

-- stoke' =
--   let withStokedFire =
--         game & over fireValue fireSucc
--              & overStored Wood (+ (-1))
--              & updateEvent FireStoked stokeCooldown
--              & fireChanged
--       withUnstokedFire =
--         game & notifyRoom "the wood has run out."
--       enoughWood = getStored Wood game > 0
--   in if enoughWood then withStokedFire else withUnstokedFire

shrinking :: DarkRoom
shrinking = do
  fireValue %= firePred
  fireChanged
