{- | additional parameters: imax

The present design has an additional mechanism to vary the 
index between imax and imin, where imin can be different from 0.

This specific instrument emulates the sound of a clarinet. 
The c:m ratio of 3:2 produces odd harmonics, and the modulation 
index is kept between 4 and 2 to control the spectral bandwidth. 
(Chowning 1973; Vercoe 1993: morefiles/chowning.orc)
-}
module Clarinet where

import Csound

envOsc :: Tab -> Tab -> Cps -> Sig
envOsc env wave cps = once env * oscBy wave cps

instr :: (D, D, D) -> Sig
instr (amp, pch, imax) = sig amp * envOsc fenv sine (fq1 + amod)
    where adyn = fq2 * sig imin + sig (imax - imin) * fq2 * once fdyn
          amod = adyn * osc fq2  
          fq1  = sig $ 3 * cpspch pch  
          fq2  = sig $ 2 * cpspch pch  
          imin = 2  
          fenv = exps [0.0001, 200, 1, 674, 1, 150, 0.0001]
          fdyn = eexps [1, 0.0001]   

ns = [8.02, 8.04, 8.05, 8.07, 8.09, 8.11, 9.00]

note x = 0.6 *| temp (0.5, x, 4)

res = stretch 0.4 $ sco instr $ line $ fmap note ns

main = dac $ runMix res
