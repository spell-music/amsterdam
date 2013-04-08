{- | additional parameters: amplitudes, rates and frequencies of 
four different building blocks

We have here four different building blocks to create this chaotic 
sinus field. In all cases an LFO function controls the pitch evolution; 
the scanning rate varies with irate.

The unit at the left should have a feedback added to correspond to 
the original. The second unit produces a variable pitched noise band 
and the third a variable pitched inharmonic field (f12). The last 
module gives a sinus glissando contour. (Risset 1969: #510)
-}
module Chaos where

import Csound.Base

modOsc :: D -> D -> D -> Tab -> Sig
modOsc amp cps rate wave = kr amp * osc (kr cps * oscBy wave (kr rate))

type D3 = (D, D, D)

instr :: (D3, D, D3, D3, D3) -> SE Sig
instr (a1, fqr, a2, a3, a4) = do
    r <- randi 1 (kr fqr)
    return $ mean $ zipWith3 phi [1, r, 1, 1] [a1, a2, a3, a4] envelopes  
    where phi m (amp, cps, rate) tab = m * modOsc amp cps rate tab
          
envelopes = fmap segs [
    [0.99, 25, 0.99, 206, 0.318, 50, 0.318, 206, 0.99, 25, 0.99],
    [0.377, 256, 0.99, 256, 0.377],
    [0.5, 15, 0.5, 226, 0.9, 30, 0.9, 226, 0.5, 15, 0.5],
    [0.333, 8, 0.333, 240, 0.999, 16, 0.999, 240, 0.333, 8, 0.333]]

res = sco instr $ 24 *| temp ((0.5, 880, 0.12), 200, (0.4, 1660, 0.17), (0.03, 200, 0.05), (0.08, 2400, 0.33))

main = writeCsd "tmp.csd" res







