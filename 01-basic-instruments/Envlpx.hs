{- | In this variation of 01_01_2B all input arguments of ENVLPX are directed 
from within the score file. This allows an exploration of the functioning of 
this somewhat more complex unit generator of the Csound language.

In particular, the variables iatss and iatdec are new. Both specify normalized 
target values: final amplitude values are obtained by multiplication with the iamp variable.

Here are some results:

    1      growth during steady state
    < 1      small decrease (.9) works well

iatdec:  = .01    ideal
         = .1/.2  compared for two durations:
                  idur=2 sec better than idur=4 sec.
         > 1      amplitude grows louder during decay

Examples:

   iatdec = .01   ends at 1/100th of the max amplitude
   iatss = 2      exponentially strives to a point iamp*2
-}
module Envlpx where

import Csound.Base

instr :: (D, D, Tab, D, D, Tab, D, D) -> Sig
instr (amp, cps, wave, rise, dec, attack, atss, atdec) = 
    envlpx (sig amp) rise idur dec attack atss atdec * oscBy wave (sig cps)

note (dur, cps) (atss, atdec) = delay 1 $ dur *| temp (0.5, cps, sines [1], 0.2, 0.3, gp $ eexps [0.00195, 1], atss, atdec)
   
durFreqs = [(4, 440), (2, 330)]
params = [(1, 0.01), (1, 0.1 ), (1, 0.2 ), (1, 1.5 ), (2, 0.01), (0.9, 0.01)]

res = sco instr $ line [ note df ps | df <- durFreqs, ps <- params ]

main = writeCsd "tmp.csd" res
-- main = totem res
