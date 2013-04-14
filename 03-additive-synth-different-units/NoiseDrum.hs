{- | additional parameters: if1, if2

In this realization of the drum instrument, the amplitudes of the three 
building blocks are fixed at the same level. The sinus wave has been 
replaced by a second inharmonic spectrum. The wave tables for the 
inharmonic fields are now controlled from the score file. F14 and f15 
include very high harmonics from 25 to 100, resulting in a more metallic 
tone for the second group of three notes. (Risset 1969: #410 sec 1 & 2)
-}
module NoiseDrum where

import Csound.Base

envOsc :: Tab -> Tab -> Cps -> Sig
envOsc env wave cps = once env * oscBy wave cps

instr :: (D, D, Tab, Tab) -> SE Sig
instr (amp, cps, f1, f2) = do
    r <- randi 1 400
    return $ (sig amp * ) $ mean $ zipWith3 envOsc [f52, f52, f51] [sine, f2, f1] (fmap sig [500, cps, cps])  
    where f51 = guardPoint $ eexps [4096, 1]
          f52 = guardPoint $ eexps [256, 1]  

f12 = sines2 [(10, 1), (16, 1.5), (22, 2.0), (23, 1.5)]
f13 = sines2 [(25, 1), (29, 0.5), (32, 0.2)]
f14 = sines2 [(16, 1), (20, 1.0), (22, 1), (34, 2), (38, 1), (47, 1)]
f15 = sines2 [(50, 2), (53, 1),   (65, 1), (70, 1), (77, 1), (100, 1)]

notes (cps, t1, t2) = delay 1 $ line $ fmap note [(0.8, 0.2), (2, 0), (4, 1)]
    where note (dur, restDur) = line [dur *| temp (0.5, cps, t1, t2), rest restDur] 

res = sco instr $ line $ fmap notes [(5, f13, f12), (5, f15, f14), (15, f13, f12)]

main = writeCsd "tmp.csd" res
-- main = totem res





