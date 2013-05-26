{- | additional parameters: ibuf, if1

The EXPSEG envelope adds a decay pattern to the tones. The first section plays 
two PLUCK-made tones. In the second and third section, we use 10 different noise 
tables. The noises' bandwidth descends from 10000 Hz to 25 Hz in each section.
-}
module ExpoPluck where

import Csound

instr :: (D, D, D, Tab) -> Sig
instr (amp, cps, buf, ft) = sig amp * expseg [1, 0.8 * idur, 1, 0.2 * idur, 0.0001] 
    * pluck (sig amp) (sig cps) buf ft meth
    where meth = 1


-- ??? how to read tables from files
tables = undefined

note buf ft = line [temp (0.5, 220, buf, ft), rest 2]

res = sco instr $ line [
    line $ fmap (note 128)  tables,
    line $ fmap (note 1024) tables]

main = writeCsd "tmp.csd" res

