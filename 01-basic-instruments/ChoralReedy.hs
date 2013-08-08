{- | These are some reedy tones embellished by a choral effect.  The
impression of several players is achieved by small differences in
frequency and time: up to several percent in frequency and up to
0.8 sec in time. 

For a more general use in orchestras, simple software routines
can generate the additional voices from one melody (formerly PLF
routines in Music 5). (Risset 1969: #250)
-}
module ChoralReedy where

import Csound
import qualified Data.List as L(transpose)

reedy :: (D, D, Tab) -> Sig
reedy (amp, cps, env) = once env * oscili (sig amp) (sig cps) wave
    where wave = sines [0.4, 0.3, 0.35, 0.5, 0.1, 0.2, 0.15, 0, 0.02, 0.05, 0.03]
            
envelope = elins [0, 1, 0.8, 0.6, 0.7, 0.6, 0]   -- smooth: reed

amp = 0.5

-- we render three detuned versions of the melody and then mix them together with different delays
notes = delay 1 $ chord $ zipWith delay [0, 0.03, 0.05] $ fmap line $ L.transpose $ fmap (\(dur, xs) -> [dur *| note x | x <- xs]) [
    (0.5,  [486, 492, 473]),
    (0.25, [615, 610, 629]),
    (0.25, [648, 660, 625]),
    (0.5,  [729, 719, 741]),
    (0.25, [972, 990, 950]),
    (0.25, [890, 880, 884]),
    (0.25, [820, 830, 809]),
    (0.25, [820, 835, 808]),
    (0.25, [820, 848, 800]),
    (2.0,  [729, 722, 743])]
    where note c = temp (amp, c, envelope)
            
            
res = sco reedy notes

main = dac $ runMix res

