{- | additional parameters: idec

Envelope generator ENVLPX and a 10 harmonics wave synthesize plucked notes. 
The exponential f51 merely provides the rise shape. The time of this rise 
is kept short (10 msec), while the decay time is the sole free variable. 
Setting iatss=1 keeps the amplitude stable during the steady state period 
of the envelope, and iatdec controls the attenuation rate during decay time. 
For iatdec, the value of 0.01 is ideal. An excessively small value (say 0.0001) 
is likely to produce an audible cutoff.

As a matter of fact, the envelopes in this example do not have a steady state 
period, because the variable idec is set to values that exceed the duration of 
the notes. In this case the decay period starts directly after the end of the rise period.

A comparison of this instrument with the previous one shows that the plucked sound 
quality solely stems from the characteristic pluck envelope: a short exponential rise 
and a long exponential decay. (Risset 1969: #250)
-}
module Plucked where

import Csound

plucked :: (D, D, D) -> Sig
plucked (amp, cps, dec) = oscili (envlpx (sig amp) 0.01 idur dec env 1 0.01) (sig cps) wave
    where wave = sines [0.4, 0.3, 0.35, 0.5, 0.1, 0.2, 0.15, 0, 0.02, 0.05, 0.03] --  complex waveform
          env  = guardPoint $ eexps [0.00195, 1]
       
amp = 0.5       
       
notes = delay 1 $ chord [
    line [
        line $ fmap note [(0.5,  486, 2), (0.25, 615, 1), (0.25, 648, 1)],
        chord [note (1, cps, dec) | (cps, dec) <- [(486, 2), (615, 1.5), (729, 1.5)]]],    
    delay 1.5 $ line [
        line $ fmap note [(0.5, 1944, 0.9), (0.5, 1728, 0.9), (0.25, 1640, 0.5), (0.25, 1458, 0.5), (0.5, 1640, 1)],
        chord [note (0.9, cps, 1) | cps <- [1458, 1230, 731]]]]    

note (dur, cps, dec) = dur *| temp (amp, cps, dec)
       
       
res = sco plucked notes

main = dac $ runMix res
