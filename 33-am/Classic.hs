{- additional parameters: ifc, imod, ifqm, ifm

This is a 'classical' amplitude modulation design through which 
one can control the precise percentage of amplitude modulation 
of a signal. The first section plays notes with an amplitude 
modulation increasing from 0 to 1 in steps of 0.2 imod. The 
amplitude modulation is applied to two sinus signals, but this 
is extendable to the wide range of complex carriers and modulators.

The amplitude is kept to the same output level in function of a 
chosen imod factor. In other words: for whatever amount of modulation, 
the amplitude will equal the specified value iamp.

In the second section imod stays at 100% modulation. The effect of 
a changing modulator frequency is tested by varying ifqm from 
150 Hz to 1200 Hz. It should be noted that in the latter case, 
the output also contains the reflected "negative" frequencies.

Suggestions: Create an electronic "tremolo" with a small 'imod' 
and subaudio ifqm. For imod=1, subaudio ifqm leads to a strongly 
pulsating tone. As this concerns LFO modulation of amplitude, 
within the ACCCI an electronic tremolo instrument belongs to 
main group 01 and not to main group 33.
-}
module Classic where

import Csound.Base

instr :: (D, D, Tab, D, D, Tab) -> Sig
instr (amp, fqc, fc, imod, fqm, fm) = a2
    where mod = kr (imod * amp) * oscBy fm (kr fqm)
          a1  = mod + kr (amp * (2 - imod))
          
          env = a1 * once (guardPoint $ exps [1, 50, 5000, 462, 1])    
          a2  = env * oscBy fc (kr fqc)       
            
n1 mod = temp (0.5, 400, sine, mod, 100, sine)
n2 fqm = temp (0.5, 400, sine, 1,   fqm, sine)

res = sco instr $ line [
    line $ fmap n1 [0, 0.2 .. 1],
    line $ fmap n2 [150, 200, 300, 400, 800, 1200]]

main = writeCsd "tmp.csd" res
