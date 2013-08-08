{- additional parameters: none

This time the number of harmonics is constant at 10 per note. The timbre of the 
tones is more steady, compared to 43_01_1. As the number of harmonics is not put 
in relation to the sampling rate, the last note contains objectionable, ugly 
foldover components.
-}
module OctaveBuzz1 where

import Csound

instr :: (D, D) -> Sig
instr (amp, fqc) = buzz env (sig fqc) nH sine
    where nH  = 10
          env = linen (sig amp) 0.2 idur 0.2
              
fqcs = [55, 110, 440, 1760, 3520, 7040, 14080]

note fqc = 2 *| temp (0.5, fqc)

res = sco instr $ line $ fmap note fqcs

main = dac $ runMix res

