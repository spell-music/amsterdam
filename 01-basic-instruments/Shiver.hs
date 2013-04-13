{- | This instrument permits to add a noise band to a pitched sound with variable 
envelope and waveform. The basic instrument is modified to include a RANDI unit 
generator. The variable iperc is a percentage of iamp and varies from 1% to 300%. 
The effect of roughness/noise introduced by RANDI is clearly perceived in this 
design, in contrast to instrument Scary.hs, where the LFO had complicated the soundscape.
-}
module Shiver where

import Csound.Base

instr :: (D, D, Tab, Tab, D, D) -> SE Sig
instr (amp, cps, wave, env, perc, randCps) = do
    r <- randi (sig $ perc * amp / 100) (sig randCps) 
    return $ (r + sig amp) * once env * oscBy wave (sig cps)
    
note perc = delay 1 $ 1.5 *| temp (0.5, 1109, wave, env, perc, 40) 
    where wave = sines [1, 0.4, 0.2, 0.1, 0.1, 0.05]
          env  = segs [0, 1, 0.8, 2, 0.9, 1, 0.7, 3, 0.2, 3, 0] 

res = sco instr $ line $ fmap note [1, 50, 80, 200, 300]

main = writeCsd "tmp.csd" res
-- main = totem res    
    





