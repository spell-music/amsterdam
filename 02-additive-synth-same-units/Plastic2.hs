{- | additional parameters: irise, idec

This design uses a LINSEG envelope which is different from the previous one. 
The envelope produces a large and fast increase of the amplitudes in the rise. 
Both for instruments 02_13_1 and 02_13_1B the rise time itself is 50 msec. 
It is somewhat larger than in most actual trumpet sounds because of the unusual 
way in which the harmonics enter.

Risset adds here that these sounds are not presented as good imitations of trumpet sounds. 
There is no formant structure, there are not enough components and there is no frequency 
control. Yet, he points out, this design is not limited to brass-like sounds, and can be 
useful in other contexts.

The figure shows that below an amplitude of 33 the output will contain no harmonics, 
while for values between 33 and 50 the number of harmonic components increases to seven.

Reaching 50 on the abscissa, all functions are .05. This can be used as a point of reference, 
to understand how the quantities are distributed in this instrument. After passing through 
the sloped linear scaling functions, the amplitudes are multiplied by different amounts, 
proportional to 1000. For the second harmonic: 1000x.05=50, for the third harmonic: 2000x.05=100 
and so on for the other harmonics. These amounts function as additional (or secondary) weighting 
factors of the harmonics.

While all following the LINSEG envelope, above an amplitude value of 50, the various harmonics 
increase 2,3 ... 7 times as fast as the fundamental (according to their harmonic number in this 
particular design). (Risset 1969: #210)
-}
module Plastic2 where

import Csound.Base
import Plastic1(plastic)

instr :: (D, D, D, D) -> Sig
instr (amp, cps, rise, dec) = plastic e (amp, cps)
    where e = linseg [0, rise' * 100, 0.99, rise' * 10, 0.65, rise' * 18, 0.8, idur - (rise' + dec'), 0.6, dec' * 44, 0.3, dec' * 45, 0]
          rise' = rise / 128
          dec'  = dec / 128  

amps = fmap ( / 120) [45, 50, 55, 80, 60, 65, 70, 75, 80]

res = sco instr $ line $ fmap note amps
    where note amp = delay 0.2 $ 0.3 *| temp (amp, 554, 0.05, 0.01)

main = writeCsd "tmp.csd" res
-- main = totem res


