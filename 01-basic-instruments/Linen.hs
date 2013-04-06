{- |  LINEN OSCIL implementation of the basic synthesis instrument. 
The waveform is variable, but in this particular example only a sinus wave is played.

We tested a couple of rise and decay values for LINEN. The last setting of irise/idec sounds like a string tone.
-}
module Linen where

import Csound.Base

instr :: (D, D, Tab, D, D) -> Sig
instr (amp, cps, wave, rise, dec) = linen (kr amp) rise idur dec * oscBy wave (kr cps)

note (cps, rise, dec) = delay 1 $ temp (0.5, cps, sines [1], rise, dec)

res = sco instr $ line $ fmap note [
    (440, 0.2, 0.3), (440, 0.1, 0.1), (220, 0.05, 0.2), (220, 0.3, 0.4)]
    
main = writeCsd "tmp.csd" res
-- main = totem res


