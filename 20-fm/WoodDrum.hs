{- | additional parameters: none

Wood drum timbre. The modulation index is set very high (25) and decays 
rapidly to produce a burst of energy over a wide frequency band at the 
onset followed by a sinusoid. The latter creates the perceptual effect 
of a strong resonance. (Chowning 1973)
-}
module WoodDrum where

import Csound.Base
import BasicFM(chown)

instr :: (D, D) -> Sig
instr = chown imax fq1 fq2 fenv fdyn
    where imax = 25
          fq1  = 16
          fq2  = 11
          fenv = guardPoint $ exps [0.8, 113, 1, 10, 1, 390, 0.0001]
          fdyn = segs [1, 64, 0, 448, 0]

i1 = [3.02, 3.04, 3.05, 3.07, 3.09, 3.11, 4]
i2 = fmap pred i1

note x = 0.2 *| temp (0.5, x) 

res = sco instr $ line [
    line $ fmap note i1,
    rest 1,
    line $ fmap note i2]
    
main = writeCsd "tmp.csd" res


