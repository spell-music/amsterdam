{-# Language TupleSections #-}
{- | additional parameters: ifr

RANDI produces band-limited noise. Here a sequence of 10 notes is played. 
The bandwidth of the signals is decreasing from 10000 to 25 Hz. The 
envelope gives .4 sec of rise and decay over the duration of the notes.

The soundfile 10_02_1.SF serves as input for the PLUCK unit generator 
in main group 15.
-}
module Limited where

import Csound.Base

instr :: (D, D) -> SE Sig
instr (amp, cps) = randi (linen (sig amp) rise idur dec) (sig cps)
    where rise = 0.4
          dec  = 0.4

res = sco instr $ line $ fmap (temp . (0.5, )) [10000, 5000, 2500, 2000, 1000, 500, 250, 125, 50, 25]  
    
main = writeCsd "tmp.csd" res
-- main = totem res
