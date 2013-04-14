{- additional parameters: none

The timbre of a bell is obtained by an exponential index and amplitude 
envelope. The modulation index is high (10) and the c:m ratio is set 
at 5:7 to yield the typical inharmonic spectrum. A long duration is 
required. By varying the steepness of the exponentials one can control 
the timbre in a subtle manner (Chowning 1973).
-}
module Bell where

import Csound.Base
import BasicFM(chown)

envOsc :: Tab -> Tab -> Cps -> Sig
envOsc env wave cps = once env * oscBy wave cps

instr :: (D, D) -> Sig
instr = chown imax fq1 fq2 fenv fdyn
    where imax = 10
          fq1  = 5
          fq2  = 7
          fenv = guardPoint $ eexps [1, 0.0001]
          fdyn = guardPoint $ eexps [1, 0.001]          

res = sco instr $ 15 *| temp (0.5, 5.07)

main = writeCsd "tmp.csd" res
-- main = totem res



