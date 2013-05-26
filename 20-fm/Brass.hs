{- | additional parameters: none

The brass timbre emerges with parallel envelope shapes for index and 
amplitude, as shown in the figure. This means that overall spectral 
richness and amplitude vary in proportion to each other. The c:m 
ratio of 1:1 produces components falling in the harmonic series. 
By varying the envelope shapes or indices by small amounts a wide 
variety of timbres is possible. (Chowning 1973)
-}
module Brass where

import Csound
import BasicFM(chown)

instr :: (D, D) -> Sig
instr = chown imax fq1 fq2 fenv fdyn
    where imax = 5
          fq1  = 1
          fq2  = 1
          fenv = ft
          fdyn = ft
          ft   = guardPoint $ lins [0, 80, 1, 80, 0.85, 290, 0.8, 63, 0]  

ns = [8.02, 8.04, 8.05, 8.07, 8.09, 8.11, 9.00]

note x = 0.6 *| temp (0.5, x)

res = sco instr $ line $ fmap note ns

main = writeCsd "tmp.csd" res
-- main = totem res
