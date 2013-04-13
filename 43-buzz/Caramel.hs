{- additional parameters:

One hears the BUZZ with an EXPSEG envelope. The instrument plays 
the same sequence as the two instruments before.

-}
module Caramel where

import Csound.Base

instr :: (D, D, D) -> Sig
instr (amp, fqc, nH) = buzz env (sig fqc) (sig nH) sine
    where env = expseg [0.1, 0.1 * idur, amp, 0.9 * idur, 0.1]

              
nHs = [5, 10, 15, 20, 40]

note nH = 2 *| temp (0.5, 125, nH)

res = sco instr $ line $ fmap note nHs

main = writeCsd "tmp.csd" res

