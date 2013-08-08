{- | additional parameters: none

In instrument 1 PLUCK uses a set of random numbers, made by PLUCK itself, 
while in instrument 2 the table f77 is created from the soundfile "Sflib/10_02_1.SF". 
This soundfile also contains random numbers, but gives additional control over the 
bandwidth of the noise. For both instruments the cyclic buffer of PLUCK contains 
128 numbers and the chosen decay method is simple averaging. From each instrument 
two notes with a duration of a second are played.

NB: The instrument has no envelope to give the net impression of this unit generator.

Suggestions:

Use other audio soundfiles as well.
-}
module Comparison where

import Csound

instr1 :: (D, D) -> Sig
instr1 (amp, cps) = pluck (sig amp) (sig cps) buf ft meth
    where buf = 128
          ft  = undefined -- zero ftable ???
          meth = 1

instr2 :: (D, D) -> Sig
instr2 (amp, cps) = pluck (sig amp) (sig cps) buf ft meth
    where buf = 128
          ft  = undefined -- how to read tables from files ???
          meth = 1

notes = line $ fmap (\x -> delay 1 $ temp (0.5, x)) [220, 440]
    
res = line [sco instr1 notes, sco instr2 notes]

main = dac $ runMix res
