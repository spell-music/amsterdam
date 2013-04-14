{- | additional parameters: none

This waveshaping instrument uses a ring modulation to produce a pitched 
noise instrument. There are separate envelopes for the distortion index 
(LINSEG) and the amplitude (f31).

The waveshaping part of the instrument runs at ifq*.7071, producing 
inharmonic partials. As LINSEG drops sharply, so does the richness 
of the spectrum. Short after the beginning of the tone, the spectral 
bandwidth of the waveshaper has reached zero and all that is left to 
hear is a simple sinus.

The instrument is designed for short durations (.2 sec).

In the low frequency range it sounds quite drum like (first section), 
while it gets more pitched in the high frequency range (second section). 
(Dodge 1985: pp. 141-142; Vercoe 1993: morefiles/risset1.orc)
-}
module PitchedNoise where

import Csound.Base

instr :: (D, D) -> Sig2
instr (amp, pch) = (a13, a13 * a2)
    where fq = sig $ cpspch pch

          f x = 1 + 0.841 * x - 0.707 * x ** 2 - 0.595*x**3 + 0.5*x**4 + 0.42*x**5 
                -0.354*x**6 - 0.279*x**7 + 0.25*x**8 + 0.21*x**9

          a13 = f a4

          a1  = sig amp * once f31
          a2  = a1 * osc fq
            
          a3  = linseg [1, 0.04, 0, idur - 0.04, 0]
          a4  = a3 * osc (fq * 0.7071)    

-- envelope for sinus at ifq
f31 = setSize 512 $ segs [0, 16, 0.2, 16, 0.38, 16, 0.54, 16, 0.68, 16, 0.8, 16, 0.9, 16, 0.98, 8, 1, 2, 1, 6, 0.96,
        64, 0.8313, 32, 0.5704, 80, 0.164, 48, 0.0521, 44, 0.0159, 20, 0.0092, 64, 0.005, 32, 0]

i1 dur pch = dur *| temp (0.5, pch)
    
res = sco instr $ line [
    --   idur   ipch
    i1   0.2    6.00,
    i1   0.25   6.05,
    i1   0.25   7.00,
    i1   0.2    7.05,
    i1   0.2    8.00,
    i1   0.2    8.05,
    i1   0.2    9.00,
    i1   0.2    8.07,
    i1   0.1    8.00,
    i1   0.25   7.07,
    i1   0.25   7.00,
    i1   0.25   6.07,
    i1   0.25   6.00,

    i1   0.2    8.00,
    i1   0.25   8.05,
    i1   0.25   9.00,
    i1   0.2    9.05,
    i1   0.2   10.00,
    i1   0.2   10.05,
    i1   0.2   11.00,
    i1   0.2   10.07,
    i1   0.1   10.00,
    i1   0.25   9.07,
    i1   0.25   9.00,
    i1   0.25   8.07,
    i1   0.25   8.00,
    i1   0.2   11.00,
    i1   0.2   10.07,
    i1   0.1   10.00,
    i1   0.25   9.07,
    i1   0.25   9.00,
    i1   0.25   8.07,
    i1   0.25   8.00]

main = writeCsd "tmp.csd" res
-- main = totem res



