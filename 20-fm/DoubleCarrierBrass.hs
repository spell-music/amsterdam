{- | additional parameters: imax1, imax2

This instrument uses a second carrier oscillator to synthesize a formant 
region at a given frequency. Both carriers follow the same amplitude 
envelope and index envelope, though the formant carrier is scalable 
for both amplitude and index.

The equation 'ifq2=int((iform/ifq1)+.5)*ifq1' lets ifq2 wander about in close 
proximity of the formant frequency iform, while keeping ifq2 in a harmonic 
relation to ifq1. (Chowning 1973; Vercoe 1993: morefiles/chowning.orc)
-}
module DoubleCarrierBrass where

import Csound.Base

instr :: (D, D, D, D) -> Sig
instr (amp, fq1, max1, max2) = a1 + a2
    where ftab = guardPoint $ segs [0, 85, 1, 85, 0.75, 258, 0.59, 85, 0]
          fqmod = fq1
          min = 2
          form = 2100
          fq2 = intD ((form/fq1) + 0.5) * fq1          
        
          env = kr amp * once ftab
          dyn = (kr $ fq2 * min) + kr (fq2 * (max2 - min)) * once ftab
                    
          mod = dyn * osc (kr fqmod)  
          a1  = env * osc (kr fq1 + mod)  
          a2  = 2 * env * osc (kr fq2 + ((kr $ max2/max1) * mod))  
        
note x = line [temp (0.5, x, 3, 1.5), rest 1]

res = sco instr $ line $ fmap note [300, 200, 400]

main = writeCsd "tmp.csd" res




