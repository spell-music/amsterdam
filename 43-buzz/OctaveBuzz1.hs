{- additional parameters: none

This instrument simply shows the effect of varying fundamental frequency on 
the tone quality, when the number of harmonics is automatically calculated by

            inH = int (sr/2/ifqc)

The tone is played in six consecutive octaves. It turns out to be rather buzzy 
for low frequencies, which explains the name of this unit generator. The timbre 
becomes brighter in the high frequency area.
-}
module OctaveBuzz1 where

import Csound.Base

instr :: (D, D) -> Sig
instr (amp, fqc) = buzz env (sig fqc) nH sine
    where nH  = sig $ intD (sampleRate / (2 * fqc))
          env = linen (sig amp) 0.2 idur 0.2
              
fqcs = [55, 110, 440, 1760, 3520, 7040]

note fqc = 2 *| temp (0.5, fqc)

res = sco instr $ line $ fmap note fqcs

main = writeCsd "tmp.csd" res


