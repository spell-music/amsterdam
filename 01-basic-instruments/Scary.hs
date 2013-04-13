{- | This instrument enables us to add a percentage of random variation and 
a vibrato to the amplitude value.

Here we show how three different percentages of amplitude increase the roughness 
of the tone. The experiment is repeated at a higher random UG frequency. This 
increases the noise. Due to the design of this instrument, the percentage added 
to the amplitude in the final output signal is half of the demanded value: 
i.e. 25% output where 50% is specified in the score.

The instrument appears in Risset's flute-like passage #100 which is actually produced 
by two instruments glued together in the manner of musique concrВЉte. In his comment 
to the first part of this twin design, Risset remarks that the contribution of RANDI 
to the overall sound is negligible. The second part of #100 is reproduced as 01_40_1 
and the complete flute-like instrument is classified as 80_01_1. (Risset 1969: #100)
-}
module Scary where

import Csound.Base 

instr :: (Iamp, Icps, Tab, Tab, Icps, D, Icps) -> SE Sig
instr (amp, cps, wave, env, lfoCps, perc, randCps) = do
    r <- randi (sig $ perc * amp / 100) (sig randCps)
    return $ (sig amp + r) * osc (sig lfoCps) * once env * oscBy wave (sig cps)
    
note perc frq = delay 1 $ 1.5 *| temp (0.5, 1109, wave, env, 4, perc, frq)
    where wave = sines [1, 0.4, 0.2, 0.1, 0.1, 0.05]
          env  = segs [0, 1, 0.8, 2, 0.9, 1, 0.7, 3, 0.2, 3, 0] 
          
res = sco instr $ line [ note perc frq | frq <- [40, 110], perc <- [1, 50, 80] ]

main = writeCsd "tmp.csd" res
-- main = totem res


