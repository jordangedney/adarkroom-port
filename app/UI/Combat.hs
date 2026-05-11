module UI.Combat (drawCombatWindow) where

import Brick
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Center (centerLayer, hCenter)
import Control.Lens (view, (&))
import qualified Data.Map.Strict as Map

import Shared.Game
  ( Combat
  , CombatState(..)
  , Game
  , combatState
  , enemyName
  , enemyHp
  , enemyMaxHp
  , enemyArt
  , enemyAtkAnim
  , playerAtkAnim
  , attackCooldown
  , victoryText
  , defeatText
  , inCombat
  , playerStats
  , hp
  , maxHp
  , expeditionInventory
  )
import Shared.Item (Item(..))
import Shared.UI (Name(..))
import UI.Components
import qualified Path.Combat as Combat

drawCombatWindow :: Game -> Widget Name
drawCombatWindow game = case view inCombat game of
  Nothing -> str ""
  Just c  -> renderCombat game c

renderCombat :: Game -> Combat -> Widget Name
renderCombat game c =
  let titleStr = "an attack on the path"
      width    = 60

      curPlayerHp = view (playerStats . hp) game
      maxPlayerHp = view (playerStats . maxHp) game
      curEnemy    = view enemyHp c
      maxEnemy    = view enemyMaxHp c

      playerColumn =
        vBox
          [ hCenter (str "you")
          , str " "
          , vBox (map str (playerArt (view playerAtkAnim c > 0)))
          , str " "
          , hCenter (str (hpLine curPlayerHp maxPlayerHp))
          ]

      enemyColumn =
        vBox
          [ hCenter (str (view enemyName c))
          , str " "
          , vBox (map str (enemyArtFor c))
          , str " "
          , hCenter (str (hpLine curEnemy maxEnemy))
          ]

      arena = padBottom (Pad 1)
            $ hBox [ padLeftRight 2 playerColumn
                   , padLeftRight 2 (str "  ")
                   , padLeftRight 2 enemyColumn
                   ]

      buttons = combatButtons game c

      flavor = case view combatState c of
        CombatVictory -> vBox (map str (view victoryText c))
        CombatDefeat  -> vBox (map str (view defeatText c))
        CombatActive  -> str " "

      controlWidth = hLimit width (str (replicate width ' '))

      content =
        controlWidth
        <=> arena
        <=> padBottom (Pad 1) (hCenter flavor)
        <=> hCenter buttons
        & padBottom (Pad 1)
        & padLeft (Pad 2)

  in content
     & borderWithLabel (str (formatTitle titleStr (width - 1)))
     & centerLayer

combatButtons :: Game -> Combat -> Widget Name
combatButtons game c = case view combatState c of
  CombatActive ->
    let onCooldown = view attackCooldown c > 0
        haveMeat = Map.findWithDefault 0 CuredMeat (view expeditionInventory game) > 0
        haveMeds = Map.findWithDefault 0 Medicine (view expeditionInventory game) > 0

        mkAction label btn =
          if onCooldown
          then greyedButton label
          else dialogButton btn label

        attackRow = concat
          [ [mkAction "attack" AttackButton]
          , [mkAction "shoot" ShootButton    | Combat.hasShoot game]
          , [mkAction "tangle" TangleButton  | Combat.hasTangle game]
          , [mkAction "lob" LobButton        | Combat.hasLob game]
          , [mkAction "bayonet" BayonetButton | Combat.hasBayonet game]
          ]

        meat =
          if haveMeat
          then dialogButton EatMeatButton "eat meat"
          else greyedButton "eat meat"
        meds =
          [dialogButton UseMedsButton "use meds" | haveMeds]
        supportRow = meat : meds

        spacer = str "  "
        rowOf = foldr1 (\a b -> a <+> spacer <+> b)
    in vBox [ rowOf attackRow
            , str " "
            , rowOf supportRow
            ]
  CombatVictory ->
    dialogButton ClaimRewardsButton "take loot"
  CombatDefeat ->
    dialogButton WakeUpButton "wake up"

-- Player faces right; on a lunge, shift one column toward the enemy.
playerArt :: Bool -> [String]
playerArt lunging =
  let base = [ "  @  "
             , " /|\\ "
             , " / \\ "
             ]
  in if lunging then map (' ':) base else base

-- Enemy faces left; on a lunge, shift one column toward the player by
-- trimming a leading space from each row when present.
enemyArtFor :: Combat -> [String]
enemyArtFor c =
  let base = view enemyArt c
      stepLeft row = case row of
        ' ':rest -> rest ++ " "
        _        -> row
  in if view enemyAtkAnim c > 0 then map stepLeft base else base

hpLine :: Int -> Int -> String
hpLine cur mx =
  "hp " <> show cur <> "/" <> show mx
