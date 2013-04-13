{- | additional parameters: iamp2, ifq2, iamp3, ifq3 ipcf, iatt1, idec1, 
iatt2, idec2, iatt3, idec3

This design emulates a pitched drum with control on the pitch contour.

One oscillator produces the fundamental wave (160Hz) and the other 
two oscillators create inharmonic partials: 225, 300, 375, 450 Hz 
for the second oscillator and 468, 549, 610, 671, 732, 915, 1037 
and 1098 Hz for the third oscillator.

The amplitude envelopes are given by three EXPSEG unit generators.
The rise time for the fundamental is 10 or 30 msec, its steady state 
is 0 or 30 msec, and its decay is set to 1.6 seconds. The remaining 
two EXPSEG modules ensure a fast decay of the inharmonic partials. 
In this example, the set of partials take .6 and .3 seconds respectively 
to decay to 1/1000th of their initial amplitude.

The pitch evolution is following the functions f31 (stays the same), 
f32 (increasing about a third), f1 (sinus with an amplitude of a third) 
and f33 (decreasing a third).

The examples are ordered in the same sequence. The last tone is to 
demonstrate the degenerate effect of a long rise paired with a very 
brief decay of the inharmonic contributors. (Risset 1969: #440)

The instrument can also be classified in main group 03. This would 
underline the functional difference of the oscillators. Nevertheless, 
we prefer to focus on the technical parallelism of the design.
-}
module SingingDrum where

import Csound.Base

instr :: ((Iamp, Icps), (Iamp, Icps), (Iamp, Icps), Tab, D, (D, D), (D, D), (D, D)) -> Sig
instr (str1, str2, str3, pcf, startval, attDec1, attDec2, attDec3) =
    mean (zipWith3 partial [str1, str2, str3] envelopes waves)
    where pc = ar (once pcf) + sig startval
          partial (amp, cps) env wave = sig amp * env * oscBy wave (pc * sig cps)
          env (att, dec) (start, end) = expseg [start, att, 0.99, idur - (att + dec), 0.99, dec, end]
          envelopes = zipWith env [attDec1, attDec2, attDec3] [(0.00097, 0.00097), (0.0039, 0.0000000059), (0.0039, 0.00000000000009)]
          waves = fmap partials [[1], [3, 4, 5], [8, 9, 10, 11, 12, 15, 17, 18]]
          
note a b = (=:= rest 2) $ phi a b
    where phi (dur, att1, dec2) (tab, start) = dur *| temp ((0.8, 160), (0.4, 75), (0.2, 61), tab, start, (att1, 1.6), (0.01, dec2), (0.01, dec2))
    
params1 = [
--   dur   att1  dec(2|3) 
    (1.63, 0.01, 1.61), 
    (1.7,  0.03, 1.65)]

params2 = zip waves [0, 0, 0, 0.9]

waves = [
    esegs [1, 1],       -- steady state
    esegs [0.85, 1],    -- increasing 3rd
    esegs [1, 0.85],    -- decreasing 3rd
    sines [1]]          -- up and down & start + 0.9

res = sco instr $ line [
   line [note p1 p2 | p2 <- params2, p1 <- params1 ],
   -- non-realistic parameters
   2 *| temp ((0.8, 160), (0.4, 75), (0.2, 61), waves !! 2, 0, (0.01, 1.95), (1.9, 0.1), (0.9, 0.8)),
   rest 1]

main = writeCsd "tmp.csd" res
-- main = totem res
            
