module RandomEvents.EventType where

import System.Random
import GameTypes

data BeggarState
  = Start
  | Scales
  | Teeth
  | Cloth
  deriving (Show, Eq)

data BeggarEvent
 = Give50
 | Give100
 | Leave
 deriving (Show, Eq)

data Scene = Scene
  { text :: [String]
  , notification :: String
  -- , blink :: undefined
  , buttons :: [BeggarEvent]
  }

type FSM s e = s -> e -> Game -> StdGen -> (Game, s)
--
-- beggarScene :: FSM BeggarState BeggarEvent
-- beggarScene Start Give50 game random =
--   if view (stored . fur) game >= 50
--   then
--   game & over (stored . fur) (+ (-50))
--
--                                  text: [
--                                         _('a beggar arrives.'),
--                                         _('asks for any spare furs to keep him warm at night.')
--                                 ],
--                                 notification: _('a beggar arrives'),
--                                 blink: true,
--                                 buttons: {
--                                         '50furs': {
--                                                 text: _('give 50'),
--                                                 cost: {fur: 50},
--                                                 nextScene: { 0.5: 'scales', 0.8: 'teeth', 1: 'cloth' }
--                                         },
--                                         '100furs': {
--                                                 text: _('give 100'),
--                                                 cost: {fur: 100},
--                                                 nextScene: { 0.5: 'teeth', 0.8: 'scales', 1: 'cloth' }
--                                         },
--                                         'deny': {
--                                                 text: _('turn him away'),
--                                                 nextScene: 'end'
--                                         }
--                                 }
--                         },
--
--
-- data RandomEvent = RandomEvent
--   { title :: String
--     isAvailable :: Game -> Bool
--
--   }
--
-- { /* The Beggar  --  trade fur for better good */
--                 title: _('The Beggar'),
--                 isAvailable: function() {
--                         return Engine.activeModule == Room && $SM.get('stores.fur');
--                 },
--                 scenes: {
--                         start: {
--                                  text: [
--                                         _('a beggar arrives.'),
--                                         _('asks for any spare furs to keep him warm at night.')
--                                 ],
--                                 notification: _('a beggar arrives'),
--                                 blink: true,
--                                 buttons: {
--                                         '50furs': {
--                                                 text: _('give 50'),
--                                                 cost: {fur: 50},
--                                                 nextScene: { 0.5: 'scales', 0.8: 'teeth', 1: 'cloth' }
--                                         },
--                                         '100furs': {
--                                                 text: _('give 100'),
--                                                 cost: {fur: 100},
--                                                 nextScene: { 0.5: 'teeth', 0.8: 'scales', 1: 'cloth' }
--                                         },
--                                         'deny': {
--                                                 text: _('turn him away'),
--                                                 nextScene: 'end'
--                                         }
--                                 }
--                         },
--                         scales: {
--                                 reward: { scales: 20 },
--                                 text: [
--                                         _('the beggar expresses his thanks.'),
--                                         _('leaves a pile of small scales behind.')
--                                 ],
--                                 buttons: {
--                                         'leave': {
--                                                 text: _('say goodbye'),
--                                                 nextScene: 'end'
--                                         }
--                                 }
--                         },
--                         teeth: {
--                                 reward: { teeth: 20 },
--                                 text: [
--                                         _('the beggar expresses his thanks.'),
--                                         _('leaves a pile of small teeth behind.')
--                                 ],
--                                 buttons: {
--                                         'leave': {
--                                                 text: _('say goodbye'),
--                                                 nextScene: 'end'
--                                         }
--                                 }
--                         },
--                         cloth: {
--                                 reward: { cloth: 20 },
--                                 text: [
--                                         _('the beggar expresses his thanks.'),
--                                         _('leaves some scraps of cloth behind.')
--                                 ],
--                                 buttons: {
--                                         'leave': {
--                                                 text: _('say goodbye'),
--                                                 nextScene: 'end'
--                                         }
--                                 }
--                         }
--                 }
--         },
