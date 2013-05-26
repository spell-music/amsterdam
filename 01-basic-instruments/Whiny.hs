{- | Three oscillators are connected in a manner such as to form 
a basic instrument with LFO control of pitch.

The LFO function is variable and a few linear functions exemplify
how the pitch parameter can be managed in a continuous fashion. 
The instrument gives one opportunities for any pitch envelopes that 
one can imagine. These contours are scanned at the rate of the LFO, 
which is irate times per note. In this design, irate is duration 
dependent. Since it is an LFO, frequencies =< 20 Hz are required. 
Above that limit begins the domain of FM.

For f32 and f33, the instrument 01_40_1 coincides with part 2 of #100, 
see also 1_11_1.

One function leaves the frequency unmodified, while the other function 
furnishes a frequency rise from 90% to 100%. This type of design enables 
us to model subtle glissandos and to drive them from within the score file 
by defining the pitch time function for each note.

Next we display two other linear functions steering frequency. F37 and f40 
are selected from #511 (Glissandi with constant frequency differences). 
Risset applied very long note durations to this instrument. Also, the 
composer lets the glissandoing sounds enter with ca. 1 sec delay from each 
other. This way the tones follow the same pitch envelope, the pitch of the 
first tone is ahead of the pitch of the second while their frequency difference 
remains constant. We did not repeat the chase.

F38 descends two octaves, f39 ca. a sixth. (Risset 1969: #100, Risset 1969: #511)
-}
module Whiny where

import Csound

instr :: (Icps, Tab, D, Tab, Iamp, Tab) -> Sig
instr (cps, wave, lfoRate, lfoShape, amp, env) = 
    sig amp * once env * oscBy wave (sig cps * several lfoShape (sig lfoRate))
    
waves = [
    sines [1, 0.4, 0.2, 0.1, 0.1, 0.05],        -- six harmonics
    sines [1],                                  -- sinus
    lins [0, 0.25, 1, 0.5, -1, 0.25, 0]]        -- sawtooth

envelope = lins [0, 1, 0, 79, 0.5, 60, 0.5, 20, 0.99, 120, 0.4, 140, 0.6, 92, 0]

-- pitch control functions (LFO)
lfos = fmap elins [
    [0.895, 0.99],                                  -- 10% rise
    [1, 1],                                         -- 0% rise
    [0.99, 0.99],                                   -- 0% rise
    [1, 0.5],                                       -- oct down
    [1, 1, 0.8, 0.85, 0.7, 0.2]]                    -- more complex
   ++ fmap lins [
    [0.999, 1, 0.999, 8,  0.85,  1, 0.85],          -- third down
    [0.999, 1, 0.999, 20, 0.235, 1, 0.235],         -- two octs down
    [0.999, 1, 0.999, 20, 0.06,  1, 0.06],          -- sixth down
    [0.25,  30, 0.25, 110, 0.5, 60, 0.25, 10, 0.25, 60, 0.5, 20, 0.75, 222, 0.5]]

section1 = line $ zipWith note [0.5, 0.5, 0.5, 3, 3] lfos
    where note dt lfoTab = delay 0.5 $ dt *| temp (988, waves !! 0, 1, lfoTab, 0.5, envelope)

section2 = line $ fmap note $ drop 5 lfos
    where note lfoTab = delay 2 $ 18 *| temp (208, waves !! 1, 400, lfoTab, 0.5, envelope)

section3 = line $ fmap note $ drop 5 lfos 
    where note lfoTab = delay 2 $ 18 *| temp (1864, waves !! 2, 2.2, lfoTab, 0.5, envelope)


res = sco instr $ line [section1, section2, section3]

main = writeCsd "tmp.csd" res
-- main = totem res

