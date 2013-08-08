{- | This instrument is derived from Risset's Linear and Exponential Decay Experiments. 
The object of the comparison here are two types of decay and four different durations: 
middle (2 sec), long (4 sec), short (1 sec) and shorter (.5 sec).

Linear decay seems to occur slowly at first and then suddenly disappears; 
exponential decay is more even and gives a resonance impression.

To avoid cutoff during exponential decay, one has to ensure that the amplitude controlling 
function decays to a final value not smaller than the inverse of the maximum amplitude. 
When the absolute amplitude becomes smaller than 1, the sound is lost in the quantizing noise. 
For instance, if the maximum amplitude is 8000, one should have a function 
decaying to 2(**-13)=1/8192. (Risset 1969: #300)
-}
module Decay where

import Csound

instr :: (D, D, Tab, Tab) -> Sig
instr (amp, cps, wave, env) = once env * oscili (sig amp) (sig cps) wave

envelopes = [
    elins [1, 0],
    guardPoint $ eexps [8192, 1]]

waves = fmap sines [
    [1, 0.5, 0.3, 0.2, 0.15, 0.12],
    [1, 0.2, 0.05]]    
    
durs = [2, 4, 1, 0.5]

amp = 0.5
cps = 440

res = sco instr $ line [ delay 1 $ d *| temp (amp, cps, w, e) |  w <- waves, d <- durs, e <- envelopes ]

main = dac $ runMix res




