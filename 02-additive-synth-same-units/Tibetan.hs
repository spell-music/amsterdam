{- | additional parameters: ioff, irise, idec

This remarkable tibetan harmonic chant like effect is created by nine sinusoidal 
oscillators, whose frequencies are almost identical: separated by a fraction of 
1 Hz from each other. Thus for each component, amplitude modulation leads to its 
enhancement or cancelling out in turn. In his composition 'Mutations', Risset 
gives the instrument two different envelopes: one with sharp rise and one is a 
more gradual rise.

02_43_1 has been modified to choose rise and decay times from the score file, 
instead of using an oscillator as envelope generator. A very brief rise sounds 
like the attack of a string instrument. The score fragment is from 'Mutations'. 
(Lorrain 1980: phase6; Vercoe 1993: morefiles/risset1.orc)
-}
module Tibetan where

import Csound.Base 

instr :: (D, D, D, D, D) -> Sig
instr (amp, cps, off, rise, dec) = mean $ fmap partial $ 0 : offs ++ (fmap negate offs)
    where offs = [1 .. 4]
          partial rat = linen (sig amp) rise idur dec * oscBy wave (sig $ cps + off * rat)   
          wave = sines [0.3, 0, 0, 0, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1]
          
res = sco instr $ chord $ fmap note [
    (0,       35,   8000,   110,     0.03,     0.07,   21),     -- 1st envelope
    (5,       20,   9600,    55,     0.02,     0.04,   12),     
    (20,      15,   8000,   220,     0.05,     1.5,    3),
    (20,      20,   9600,   110,     0.04,     2,      4),      
    (28,      30,   8000,   220,     0.04,     3,      6),      --  2nd env.
    (32,      26,   9600,   110,     0.025,    2.6,    5.2),    
    (32.1,    23,   8000,   110,     0.03,     2.3,    4.6),    
    (36,      22,   8000,    55,     0.01,     0.04,     13)]   -- 1st envelope
    where note (start, dur, amp, cps, off, rise, dec) = delay start $ stretch dur $ 
                temp (amp/15000, cps, off, rise, dec)

main = writeCsd "tmp.csd" res
-- main = totem res
                
