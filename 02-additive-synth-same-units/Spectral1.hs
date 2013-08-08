{- | additional parameters: none

This is the ACCCI implementation of Risset's 'Spectral Analysis of a Chord': 
for each note of the chord successive harmonics are introduced gradually. 
Originally, the score file has been created with the help of a PLF sub routine 
(Mathews 1969: pp.78-86). Slightly different routines were used on each of the 
three two note groups, as is shown in brackets in the figure below.


... pic  of harmonic series goes here ...

8 harmonics for group 2 and
10 harmonics for group 3 are generated in turn.

The durations of the successive harmonics are related to the fundamental note 
duration D by D-DD.

If DD = 0, they have the same duration as the fundamental, as shown to the right. 
The total duration of the sound for N harmonics is then given by: D(total) = D + N * TS.

... well imagine an another pic ...

For generation routine 2 the pattern is different:

The envelope has a parabolic attack and decay. This shape is created by the linear 
f31 multiplied by itself. The general design is additive. (Risset 1969: #500)
-}
module Spectral1 where

import Csound 

instr :: (D, D) -> Sig
instr (amp, cps) = sig amp * env * env * osc (sig cps)
    where env = once (elins [0, 1, 0])
    

ch totalDur dts cs = chord $ zipWith note dts cs
    where note dt cps = delay dt $ totalDur *| temp (0.03, cps)

dt1 = [0, 2.5  ..]
dt2 = [0, 1.25 ..]
dt3 = [0, 1    ..]

cps11 = [1865, 3730, 5595, 7460, 9325]
cps12 = [988, 1976, 2964, 3952, 4940]

cps21 = [659, 1318, 1977, 2636, 3295, 3954, 4613, 5272, 5931]
cps22 = [392, 784, 1176, 1568, 1960, 2352, 2744, 3136, 3528]

cps31 = [294, 588, 882, 1176, 1470, 1764, 2058, 2352, 2641, 2940, 3234]
cps32 = [208, 416, 624, 832, 1040, 1248, 1456, 1664, 1872, 2080, 2288]

chs total dts a b = chord [ch total dts $ reverse a, delay 0.01 $ ch total dts $ reverse b]

notes = chord [
    1    +| chs 7.5  dt1 cps11 cps12,
    4.75 +| chs 3.75 dt2 cps21 cps22,
    6.5  +| chs 2.0  dt3 cps31 cps32]
    
res = sco instr notes    
    
main = dac $ runMix res
        



