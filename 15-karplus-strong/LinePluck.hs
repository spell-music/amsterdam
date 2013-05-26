{- | additional parameters:

This differs from 15_02_1 only by the type of envelope used: LINSEG. Linear 
decay has a distorting effect: it seems to decay like an event with a longer 
duration and then disappears all too suddenly.

As in the previous example, we apply a number of noises tables with different 
bandwidths to PLUCK's table input.

In the third section, we used a larger internal buffer: ibuf is 1024.
-}
module LinePluck where

import Csound

instr :: (D, D, D, Tab) -> Sig
instr (amp, cps, buf, ft) = sig amp * linseg [1, 0.8 * idur, 1, 0.2 * idur, 0.0001] 
    * pluck (sig amp) (sig cps) buf ft meth
    where meth = 1


-- ??? how to read tables from files
tables = undefined

note buf ft = line [temp (0.5, 220, buf, ft), rest 2]

res = sco instr $ line [
    line $ fmap (note 128)  tables,
    line $ fmap (note 1024) tables]

main = writeCsd "tmp.csd" res

