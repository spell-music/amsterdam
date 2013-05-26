{- | additional parameters: if1, ifqr

This instrument uses RANDI to produce a band-limited noise with 
control of center frequency ifqr. Actually RANDI is ring modulating 
a sinus oscillator, thereby translating the noise band to both 
sides of the center frequency. A good sensation of pitch is reached 
by setting the bandwidth to 20% of the center frequency.

Suggestions:
Add a parameter that specifies the pitched-ness (that is: the bandwidth) 
in % of the center frequency.

Noise glissandos are obtained by adding an LFO onto the frequency slot 
of the oscillator. J. Tenney has made use of this type of instrument 
in his 'Noise Study'. (Dodge 1985: p. 93)
-}
module Tenney where

import Csound

instr :: (D, D, Tab, D) -> SE Sig
instr (amp, cps, wave, fqr) = do
    rnd <- randi 1 (sig fqr)
    return $ linen (sig amp) rise idur dec * rnd * oscBy wave (sig cps)
    where rise = 0.2
          dec  = 0.3  

note fqr = line [temp (0.5, 400, sine, fqr), rest 1]

res = sco instr $ line $ fmap note [80, 40, 20]

main = writeCsd "tmp.csd" res
-- main = totem res



