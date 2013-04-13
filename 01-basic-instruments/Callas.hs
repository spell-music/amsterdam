{- |  This instrument permits us to add a frequency vibrato to a sound. All notes 
have a duration of one second. In this special case the vibrato rate is neither 
duration dependent, nor duration independent.

The vibrato width is limited to subaudio frequencies. This run shows a quick 
experimentation with two sets of three notes each. The variable iwidth takes the 
values 8, 16 and 24 in turn. In the first set the rate of the vibrato is 5 Hz, in 
the second set it is 2 Hz.

The use of an adder in the instrument allows to switch off the vibrato by setting 
iwidth to 0.
-}
module Callas where

import Csound.Base

instr :: (Iamp, Icps, Tab, Iamp, Icps, Tab) -> Sig
instr (amp, cps, wave, width, rate, lfoShape) = env * oscBy wave lfo
    where env = linen (sig amp) 0.1 idur 0.1
          lfo = sig cps + sig width * oscBy lfoShape (sig rate)
          
note width rate = line [temp (0.5, 800, sine, width, rate, sine), rest 2]

widths = [8, 16, 24]
rates  = [5, 2]

res = sco instr $ line [ note w r | r <- rates, w <- widths ]

-- main = writeCsd "tmp.csd" res
main = totem res

