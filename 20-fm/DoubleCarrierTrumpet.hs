{- | additional parameters: imax1, ifq2, imax2

This is a very sophisticated FM instrument imitating a trumpet tone. There is 
a vibrato generator consisting of random amplitude deviation, a slow amplitude 
vibrato and a portamento pitch deviation. All these units combine to give an 
oscillating value in the proximity of 1.

The general design is a double carrier FM instrument, using a single envelope 
function for amplitude and modulation index. Implemented by LINSEG, the rise 
and decay portions keep their absolute values for different durations. 
As in 20_20_4, the index and the amplitude envelopes of the second carrier 
are scaled before being applied to their targets. (Morrill 1977)
-}
module DoubleCarrierTrumpet where

import Control.Applicative(liftA2)

import Csound.Base

instr :: (D, D, D, D, D) -> SE Sig
instr (amp, fq1, max1, fq2, max2) = do
    v   <- vibrato
    let mod = dyn * (sig $ fqm * max1) * osc (sig fqm * v)
        a1  = amp1 * (sig amp) * osc (mod + (sig fq1 * v))
        a2  = amp2 * (sig amp) * 0.2 * osc ((mod * sig ratio) + (sig fq2 * v))
    return $ 0.5 * (a1 + a2)
    where 
        fqm = fq1
        ratio = max2 / max1
        
        vibwidth = 0.007
        randev = 0.007
        fqr = 125
        vibrate = 7
        portdev = 0.03           

        -- vibrato signal
        width = linseg [0, 0.6, vibwidth, idur - 0.6 - 0.2, vibwidth, 0.2, 0]        
        v1 = randi randev fqr
        v2 = width * osc vibrate
        v3 = portdev * once (skipNorm $ segs [-1, 150, 0.1, 110, 0, 252, 0]) 
        vibrato  = fmap (\x -> (1 + x) * (1 + v2)  * (1 + v3)) v1        
        
        -- double-carrier, envelopes
        dyn  = linseg [0, 0.03, 1, idur - 0.03 - 0.01, 0.9, 0.01, 0]
        amp1 = linseg [0, 0.03, 1, idur - 0.03 - 0.15, 0.9, 0.15, 0]
        amp2 = linseg [0, 0.03, 1, idur - 0.03 - 0.3, 0.9, 0.3, 0]
 
        
n = temp (0.5, 250, 2.66, 1500, 1.8)

res = sco instr $ line [n, 2 *| n] 

-- main = writeCsd "tmp.csd" res
main = totem res

