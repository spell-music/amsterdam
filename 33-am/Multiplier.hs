{- | additional parameters: ifc, ifqm, ifm

The multiplier of this design functions as a general purpose ring modulator. 
The amplitude of the modulating unit is 1. In ring modulation it is safe to 
keep one signal close to unity, in order to limit the audio samples to the 
permitted amplitude range of +- 32000.

Three sample notes are played.

1) carrier 800 Hz, modulator 50 Hz: output 750, 850 Hz

2) carrier 800 Hz, modulator 111, 222, 333 Hz: output: 467, 578, 689 Hz

3) carrier 800, 1600, 2400 Hz, modulator 107 Hz: output: 693, 807, 1493, 1707, 2293, 2507 Hz
-}
module Multiplier where

import Csound.Base

instr :: (D, D, Tab, D, Tab) -> Sig
instr (amp, fqc, fc, fqm, fm) = sig amp * a1 * a2
    where env = once $ segs [0, 51, 1, 29, 0.9, 120, 0.8, 50, 0.6, 50, 0.5, 20, 0.3, 30, 0.1, 162, 0]
          a1  = env * oscBy fc (sig fqc)  
          a2  = oscBy fm (sig fqm)  

f1 = sine
f2 = sines [1, 1, 1]

note fc fqm fm = delay 2 $ temp (0.5, 800, fc, fqm, fm)

res = sco instr $ line [
    note f1  50 f1,
    note f1 111 f2,
    note f2 107 f1]

main = writeCsd "tmp.csd" res
-- main = totem res
