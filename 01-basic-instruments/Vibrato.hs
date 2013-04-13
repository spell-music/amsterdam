{- | An LFO embellishes the design of the basic instrument by adding a loudness vibrato to the tones.

The amplitude of the LFO is directly set to the value of iamp. This can be rendered more 
flexible by inserting an adder between the LFO and the envelope.

The frequencies of an LFO are by definition restricted to the subaudio range (0-20 Hz). 
Since the vibrato rate is duration independent here, the rate is directly specified by 
the value of ifq3.

The duration dependence (or not) of certain control functions addresses a fairly complicated, 
but common problem encountered in sound synthesis. Recently P.Desain and H.Honing have proposed 
some interesting solutions that could be implemented in Csound. Score file generating software 
for instruments that make extensive use of vibrato could well take advantage of their method. 
(Desain et al.: 1992)

The multiplier just before OUT serves to scale the signal. In general this is the way to adjust 
a signal's overall amplitude to a desired level: the multiplier is the volume button.

The first section displays 3 different waveforms: fundamental solo, 4 and 6 weighted harmonics.

The second section plays a sinus waveform with five different amplitude envelopes.

In the third section, the LFO's frequency is varied from 1 to 5 Hz.
-}
module Vibrato where

import Csound.Base

instr :: (D, D, Tab, Tab, D) -> Sig
instr (amp, cps, wave, env, lfoCps) = sig amp * once env * oscBy wave (sig cps) * (kr $ osc (sig lfoCps))

note (wave, env, lfoCps) = delay 1.5 $ 1.5 *| temp (0.5, 800, wave, env, lfoCps)

waves = fmap sines [
    [1],                                -- fundamental
    [1, 0.2, 0.08, 0.07],               -- four harmonics
    [1, 0.4, 0.2, 0.1, 0.1, 0.05]]      -- six harmonics

envelopes = fmap segs [  
    [0, 1, 0, 49, 0.2, 90,  0.6, 40, 0.99, 25, 0.9, 45, 0.5, 50, 0.25, 50, 0.12, 50, 0.06, 50, 0.02, 62, 0],
    [0, 1, 0, 49, 0.2, 100, 0.6, 50, 0.99, 150, 0.2, 162, 0],
    [0, 1, 0, 49, 0.2, 200, 0.5, 100, 0.2, 162, 0],
    [0, 1, 0, 79, 0.5, 60,  0.5, 20, 0.99, 120, 0.4, 140, 0.6, 92, 0],
    [0, 1, 0, 149, 0.4, 200, 0.99, 50, 0.5, 50, 0.24, 62, 0]]

lfoFreqs = [1 .. 5]

theWave = waves !! 0
theEnvelope1 = envelopes !! 1
theEnvelope2 = envelopes !! 4
theCps = 4

res = sco instr $ line [
    line [ note (w, theEnvelope1, theCps) | w <- waves ],
    line [ note (theWave, e, theCps) | e <- envelopes ],
    line [ note (theWave, theEnvelope2, c) | c <- lfoFreqs]]
    
main = writeCsd "tmp.csd" res
-- main = totem res
