{- | additional parameters: imax, ifq2

This is the most basic instrument design for the FM synthesis 
of sounds (Chowning 1973).

The parameters imax and ifq2 are varied to try some different 
c:m ratios and modulation indexes on a set of short notes.
-}
module BasicFM where

import Csound.Base

envOsc :: Tab -> Tab -> Cps -> Sig
envOsc env wave cps = once env * oscBy wave cps

chown :: D -> D -> D -> Tab -> Tab -> (D, D) -> Sig
chown imax nfq1 nfq2 fenv fdyn (amp, pch) = kr amp * envOsc fenv sine (fq1 + kr imax * fq2 * envOsc fdyn sine fq2)
    where fq1  = kr $ nfq1 * cpspch pch  
          fq2  = kr $ nfq2 * cpspch pch  

instr :: (D, D, D, D) -> Sig
instr (amp, cps1, max, cps2) = 
    linen (kr amp) 0.1 idur 0.1 * osc (kr cps1 + kr max * osc (kr cps2))

note max cps2 = temp (0.5, 200, max, cps2)

maxs = [1 .. 5]
cps2s = [200, 400 .. 1000]

sec x = delay 1 $ line $ fmap (flip note x) maxs

sec4 = delay 1 $ line $ fmap (note 1) cps2s

res = sco instr $ line [sec 200, sec 400, sec 600, sec4]

main = writeCsd "tmp.csd" res
