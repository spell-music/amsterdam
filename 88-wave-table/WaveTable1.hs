{- additional parameters: none

Even though csound only provides a limited number of stock GEN subroutines, it 
includes one (GEN 1) which can read in an external file. Hence, one can use 
a standalone program to generate functions that are not standard (with the 
limitation that files need to be written in a format understandable for GEN 1). 
In fact, as this example shows, one can use csound itself to create them.

The run will produce 513 values for the specified function. (Risset 1969: #513)
-}
module WaveTable1 where

import Csound

instr :: () -> Sig
instr () = a1 * oscBy (setSize 513 $ elins [1, 1]) 1
    where a1 = linseg [1, idur, 513]
          a2 = exp (-4.8283 * (1 - cos (2 * pi * (((a1 - 1) - 256) / 512))))
            
res = sco instr $ temp ()

-- main = writeCsd "tmp.csd" res
main = totem res
