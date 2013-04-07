{- | additional parameters: irat

This instrument is similar to the previous design (Gong.hs): f51 generates an exponential 
decaying envelope. By applying decreasing durations for the note statements 
belonging to one additive event, envelope f51 shapes the sinus waves non-synchronously.

The frequency of the individual components is expressed by a ratio to the fundamental.

The bell-like timbre is obtained by 7 components (at 1, 2, 2.4, 3, 4.5, 5.33 and 6 times 
the fundamental frequency) with variable durations.

The second tone is obtained by only four additive components (1, 2, 2.5, 3.36).

The durations are 3 sec for the first tone, and 4 sec for the second tone. (Risset 1969: #410)
-}
module Bell where

import Csound.Base

instr :: (D, D, D) -> Sig
instr (amp, cps, rat) = kr amp * fadeOut * once (guardPoint $ eexps [256, 1]) * osc (kr $ cps * rat)
    where fadeOut = linen 1 0 idur 0.25

note cps dt amp rat = dt *| temp (amp, cps, rat)

ch dts amps rats = delay 1 $ chord $ zipWith3 (note 329) dts amps rats

dt1 = [3, 2.8, 2.7, 2.4, 2.2, 1.5, 1.5]
dt2 = [4, 3.5, 3.2, 2.9]

rescale = fmap (* 0.25)
amp1 = rescale $ replicate 4 0.5 ++ replicate 3 0.75
amp2 = rescale $ replicate 4 1

rat1 = [1, 2, 2.4, 3, 4.5, 5.33, 6]
rat2 = [1, 2, 2.5, 3.36]

res = sco instr $ line [
    ch dt1 amp1 rat1,
    ch dt2 amp2 rat2]

main = writeCsd "tmp.csd" res
-- main = totem res
