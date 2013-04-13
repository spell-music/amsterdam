{-# Language TupleSections #-}
{- | additional parameters: ifr

Here the RANDH version of the most basic noise instrument: band-limited 
noise depending on ifr.

RANDH produces new random numbers at the rate specified by ifqr, and holds 
the chosen value till a new one is picked. RANDI would interpolate with a 
straight line of values between two successively chosen numbers.

The difference between RANDI and RANDH is clearly audible. Compare with 10_02_1...
-}
module Hold where

import Csound.Base

instr :: (D, D) -> SE Sig
instr (amp, cps) = randh (linen (sig amp) rise idur dec) (sig cps)
    where rise = 0.4
          dec  = 0.4

res = sco instr $ line $ fmap (temp . (0.5, )) [10000, 5000, 2500, 2000, 1000, 500, 250, 125, 50, 25]  
    
main = writeCsd "tmp.csd" res

