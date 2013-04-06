{- | additional parameters: none

These percussive gong-like sounds are realized with additive synthesis. 
An exponentially decaying envelope is set onto a number of sinusoid waveforms.

In the first sound, all frequency components decay synchronously. The spectrum 
is invariant and recalls an electronic chime.

For the second tone, the same frequency components have a decay time approximately 
inversely proportional to their frequency: the principle is followed in a flexible 
manner to come to a more intricate decay pattern. Compared to the first sound, we 
find more life and naturalness here.

The next tone features a different set of frequencies, again with non-synchronous decay.

The last four tones overlap and their frequency components are sufficiently close to 
each other to produce beats. (Risset 1969: #420)
-}
module Gong where

import Csound.Base

instr :: (D, D) -> Sig
instr (amp, cps) = kr amp * fadeOut * once (guardPoimt $ eexps [128, 1]) * osc (kr cps)
    where fadeOut = linen 1 0 idur 0.25

note dt amp cps = dt *| temp (amp, cps)

ch dts amps cpss = delay 1 $ chord $ zipWith3 note dts amps cpss

dt1 = repeat 10
dt2 = [10, 9.6, 8.8, 1.6, 8.0, 5.2, 4.0]
dt3 = [8, 7.6, 6.8, 4.8, 3.6, 2.8]
dt4 = [8.8, 8.4, 4, 5.2, 4.4, 7.6, 6.4]
dt5 = [11.6, 10.8, 10.4, 6.4, 4.8, 4.4]
dt6 = [13.6, 12.8, 12.0, 8.4, 3.2, 6.4, 4.4]

rescale = fmap ( * 0.2)
amp1 = rescale [1, 0.75, 0.5, 1, 0.4, 0.4, 0.4]
amp2 = rescale [1, 0.75, 0.4, 0.5, 0.4, 0.45]
amp3 = rescale $ [0.5, 0.45, 0.75] ++ repeat 0.4
amp4 = rescale [0.45, 0.42, 0.45, 0.2, 0.4, 0.2, 0.2]

cps1 = [240, 277, 385, 605, 340, 670, 812]
cps2 = [242.5, 307.5, 340, 384, 521, 802]
cps3 = [241.25, 262.25, 357.5, 302.5, 315, 385, 482.5]


res = sco instr $ line [
    ch dt1 amp1 cps1,
    ch dt2 amp1 cps1,
    ch dt3 amp2 cps2,
    chord [
        ch dt2 amp1 cps1, 
        1.8 +| ch dt4 amp3 cps3,
        2.2 +| ch dt5 amp2 cps2,
        7.8 +| ch dt6 amp4 cps1]]

main = totem res    

