{- | additional parameters: none

Variation on the preceding instrument: a percussive (instantaneous) attack and 
an exponential decay replace the parabolic attack and decay envelope. Everything 
else remains unchanged. (Risset 1969: #501)
-}
module Spectral2 where

import Csound.Base
import Spectral1 hiding (main, instr, res)

instr :: (D, D) -> Sig
instr (amp, cps) = kr amp * fadeOut * env * osc (kr cps)
    where env = once $ guardPoint $ eexps [1, 0.0001]
          fadeOut = linen 1 0 idur 0.25  
            
res = sco instr notes

main = totem res


