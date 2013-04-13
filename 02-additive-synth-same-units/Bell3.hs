{- | additional parameters: none

A different realization of an additive bell. The envelope is generated by EXPON. 
In contrast to the previous design, this unit generator allows the use of an A-rate 
output buffer. A longer performance of the instrument will use the last defined 
value of EXPON to continue on in the same direction (here: iamp = 1).
-}
module Bell3 where

import Csound.Base

instr :: (D, D) -> Sig
instr (amp, cps) = sum $ zipWith3 partial durs amps cpss
    where durs = [1, 0.9, 0.65, 0.55, 0.325, 0.35, 0.25, 0.2, 0.15, 0.1, 0.075]
          amps = fmap ( / 11) [1, 0.67, 1.35, 1.80, 2.67, 1.67, 1.46, 1.33, 1.33, 0.75, 1.33]  
          cpss = [(* 0.56), ( + 1) . (* 0.56), ( * 0.92), (+ 1.7) . ( * 0.92), (* 1.19), (* 1.7), (* 2), (* 2.74), (* 3), (* 3.75), (* 4.07)]
          
          partial d a c = env * osc (sig $ c cps)
            where env = expseg [a, idur, 0.00001]

note dt amp cps = dt *| temp (amp, cps)

res = sco instr $ line [
    note 20 0.6 440,
    line $ fmap (note 4 0.6) [633, 211, 999],
    note 20 0.2 633,
    line $ fmap (note 4 0.2) [211, 999, 80]]
    
main = writeCsd "tmp.csd" res
-- main = totem res
    
    


