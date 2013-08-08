{- | additional parameters: none

This additive bell is built with the OSCIL1 unit generator, allowing 
individual durations to be controlled from within the orchestra file.

The module OSCIL1i has been created specifically for this type of 
instrument design. Csound enforces the use of a K-rate output argument 
for this unit generator.

Substitution of OSCIL1 by OSCIL can not be done for the following reason: 
the waveform oscillator is still 'on' after the end of the envelope has 
been reached. This leads to a series of chaotic sinus entries and clicks, 
depending on the duration ratios.

To avoid noise the envelope function f51 needs a minimum slope of 4096 
to 1 before rescaling. (Vercoe 1993: morefiles/risset3.orc)
-}
module Bell2 where

import Csound

instr :: (D, D) -> Sig
instr (amp, cps) = sum $ zipWith3 partial durs amps cpss
    where durs = [1, 0.9, 0.65, 0.55, 0.325, 0.35, 0.25, 0.2, 0.15, 0.1, 0.075]
          amps = fmap ( / 11) [1, 0.67, 1.35, 1.80, 2.67, 1.67, 1.46, 1.33, 1.33, 0.75, 1.33]  
          cpss = [(* 0.56), ( + 1) . (* 0.56), ( * 0.92), (+ 1.7) . ( * 0.92), (* 1.19), (* 1.7), (* 2), (* 2.74), (* 3), (* 3.75), (* 4.07)]
          
          partial d a c = env * osc (sig $ c cps)
            where env = oscil1i 0 (sig $ amp * a) (idur * d) (guardPoint $ eexps [5000, 1])
            
note dt amp cps = dt *| temp (amp, cps)

res = sco instr $ line [
    line $ fmap (note 4 0.5) [999, 633, 211, 999],
    note 20 0.4 633,
    line $ fmap (note 4 0.4) [211, 999, 80]]
    
main = dac $ runMix res
