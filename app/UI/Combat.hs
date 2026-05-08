module UI.Combat (drawCombatWindow) where

import Brick
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Center (centerLayer, hCenter)
import Control.Lens (view)
import qualified Data.Map as Map

import Shared.Game
import Shared.Item (Item(CuredMeat))
import Shared.UI
import UI.Components

-- Visual width of the combat dialog. Wide enough for two stages plus
-- HP bars and an action row underneath.
combatWidth :: Int
combatWidth = 56

-- Reach of the lunge animation (how many cells the sprite shifts on attack).
lungeStep :: Int
lungeStep = 3

drawCombatWindow :: Game -> Widget Name
drawCombatWindow game = case view combat game of
  Nothing -> str ""
  Just c  -> centerLayer (combatModal game c)

combatModal :: Game -> Combat -> Widget Name
combatModal game c =
  let titleStr   = formatTitle (_enemyName c) (combatWidth - 1)

      hpLine label cur mx =
        str (justifyLeftX 16 (label <> ": " <> show cur <> "/" <> show mx))

      playerHpVal = view (playerStats . hp) game
      playerMaxHp = view (playerStats . maxHp) game
      hpRow = hCenter $ hBox
        [ hpLine "you" playerHpVal playerMaxHp
        , str "   "
        , hpLine (_enemyName c) (_enemyHp c) (_enemyMaxHp c)
        ]

      stage = padTop (Pad 1) (padBottom (Pad 1) (combatStage c))

      buttons = padTop (Pad 1) (hCenter (combatButtons game c))

      statusBanner = case _combatStatus c of
        Fighting -> emptyWidget
        Won      -> hCenter (str "the beast lies still.")
        Lost     -> hCenter (str "you collapse. the world goes black.")

      content = padBottom (Pad 1)
              $ padLeftRight 2
              $ vBox [hpRow, stage, statusBanner, buttons]

      shellWidth = hLimit combatWidth (str (replicate combatWidth ' '))
   in borderWithLabel (str titleStr) (shellWidth <=> content)

-- Two-character stage. Each side lunges toward the centre when its
-- attack-anim timer is non-zero, satisfying the "animate both on attack"
-- requirement without needing any extra event plumbing.
combatStage :: Combat -> Widget Name
combatStage c =
  let stageInner = combatWidth - 6

      playerOffset = if _playerAttackAnim c > 0 then lungeStep else 0
      enemyOffset  = if _enemyAttackAnim  c > 0 then lungeStep else 0

      playerCol = max 0 (1 + playerOffset)
      enemyCol  = max 0 (stageInner - 2 - enemyOffset)

      blanks = replicate stageInner ' '

      placeChar idx ch row =
        if idx < 0 || idx >= length row
          then row
          else take idx row <> [ch] <> drop (idx + 1) row

      stageLine =
        placeChar enemyCol  (_enemyChar c)
        $ placeChar playerCol '@' blanks

      groundLine = replicate stageInner '_'
   in vBox [str stageLine, str groundLine]

combatButtons :: Game -> Combat -> Widget Name
combatButtons game c =
  let canEat = Map.findWithDefault 0 CuredMeat (view expeditionInventory game) > 0

      attackBtn  = blueButton AttackButton "attack"
      eatBtn     = if canEat
                   then blueButton EatMeatButton "eat meat"
                   else greyedButton "eat meat"
      leaveBtn   = blueButton LeaveCombatButton (case _combatStatus c of
                                                   Won  -> "leave"
                                                   Lost -> "wake up"
                                                   _    -> "flee")
   in case _combatStatus c of
        Fighting ->
          attackBtn <+> str "  " <+> eatBtn
        _ ->
          leaveBtn

