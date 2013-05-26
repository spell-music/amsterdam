{- | additional parameters: irate, ioff1-ioffn

This instrument allows the movement of a cluster of frequencies in a linear fashion. 
The breakpoint function f35 specifies the trace all frequencies will follow. On that 
trajectory the partials maintain the same absolute distance from each other. The exact 
intervals are given by the set of ioffn variables: in this scheme half of the frequency 
components are within LFO range and the other half is just outside that range 
(4.5, 9.4, 23, 39 and 84 Hz offset).

The variable irate controls the time needed for one complete scanning of this function: 
in our example it takes 20 seconds.

The tone gives the impression of a starship during take off.
-}
module Starship where

import Csound

instr :: (D, D, D, (D, D, D, D, D)) -> Sig
instr (amp, cps, rate, (off1, off2, off3, off4, off5)) = 
    sig amp * mean [ osc (e + sig off) | off <- [0, off1, off2, off3, off4, off5]]
    where e = sig cps * oscBy pitchConvert (sig $ 1/rate) 
          pitchConvert = lins [0.25, 30, 0.25, 110, 0.5, 60, 0.25, 10, 0.25, 60, 0.5, 20, 0.75, 222, 0.5]  
    
res = sco instr $ 20 *| temp (0.5, 1000, 20, (4.5, 9.4, 23, 39, 84))

main = writeCsd "tmp.csd" res
-- main = totem res
    

