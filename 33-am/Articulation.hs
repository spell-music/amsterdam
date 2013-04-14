{- additional parameters:

This instrument attempts to prolong harmony into timbre: a chord, 
played by ring modulation instrument i1 is followed by a gong-like 
tone, whose components are the fundamentals of the chord.

The latter sound is perceived as one, yet its tone quality is related 
to the chord's harmony. Here is the passage:

[score fragment]

Instrument 1 produces the ring modulation chord, by combining a sinus 
with a block wave. The dominant frequencies are the differences between 
ifqc and ifqm: -576 +1424 / -273 +1727 / -458 +3542 / -864 +3136 / -658 +3342 Hz. 
These are the fundamentals of the modulated block wave.

The envelope controlling the sinus wave is also controlling the general 
envelope of the note and the spectral evolution. At first the attack 
and duration are short (A), then the attack switches to a cresc-decresc 
type with a medium duration (B). Finally, the frequencies are repeated 
(273, 455, 576, 648, 864 Hz) as components of a gong timbre with short 
attack and long duration (C).

Instrument 2 is equal to 02_01_3. An additive synthesis tone through 
overlapping score calls. (Risset 1969: #550; Vercoe 1993: morefiles/risset4.orc)
-}
module Articulation where

import Csound.Base

instr1 :: (D, D, D, D, D) -> Sig
instr1 (amp, rise, dec, fqc, fqm) = a1 * a2
    where env = envlpx (sig amp) rise (1 / idur) dec (guardPoint $ eexps [0.01, 1]) 1 0.01
          a1  = env * osc (sig fqc)  
          a2  = oscBy (segs [0, 42, 1, 172, 1, 84, -1, 172, -1, 42, 0]) (sig fqm)  
            
instr2 :: (D, D) -> Sig
instr2 (amp, fq) = sig amp * once (guardPoint $ eexps [512, 1]) * osc (sig fq)

i11 start fqc fqm = delay start $ stretch 0.6 $ temp (0.2, 0.01, 0.6, fqc, fqm)

i12 start dur rise fqc fqm = delay start $ stretch dur $ temp (0.2, rise, 1.2, fqc, fqm)

sec1 = sco instr1 $ chord [         
    i11   0.5   424   1000,             -- -576   +1424 
    i11   0.6   727   1000,             -- -273   +1727
    i11   0.9   1542  2000,             -- -458   +3542
    i11   1.1   1136  2000,             -- -864   +3136
    i11   1.4   1342  2000,             -- -658   +3342 

    i12   0.9   3.6   2.3   424   1000, -- same frequencies,
    i12   1     3.5   3.2   727   1000, -- but longer durations
    i12   1.3   3.2   1.9  1542   2000, -- and long, slow rise  
    i12   1.5   3     1.6  1136   2000,      
    i12   1.8   2.7   1.4  1342   2000]  


i2 dur amp fq = delay 4 $ stretch dur $ temp (amp, fq)

sec2 = sco instr2 $ chord [
    i2   10    0.5    273,   -- fundamental
    i2   7.5   0.2    455,   -- 1.6667
    i2   4.5   0.2    576,   -- 2.10989
    i2   6.5   0.15   648,   -- 2.37363
    i2   4     0.15   864]   -- 3.16484

res = chord [sec1, sec2]

main = writeCsd "tmp.csd" res
-- main = totem res


