{- additional parameters: none

This instrument uses the duration of a note to choose between two 
different envelopes. For notes exceeding .75 sec in duration, the 
right-hand envelope in the figure is applied.

The amount of distortion, i.e. the harmonicity of the spectrum 
is dependent on the maximum amplitude value specified for LINEN: 
for 56 or less there is no distortion, and the transfer function 
will simply return a sinus.

Here the odd linear transfer function produces odd harmonics, 
increasing in strength and number as the LINEN value exceeds 56. 
(Risset 1969: #150; Dodge 1985: pp. 141-142; Vercoe 1993: morefiles/risset1.orc)
-}
module Clarinet where

import Csound

instr :: (D, D) -> Sig
instr (amp, pch) = sig amp * tablei (256 + a1) (setSize 512 $ lins [-1, 200, -0.5, 112, 0.5, 200, 1])
    where env = linen 255 0.085 idur dec
          dec = ifB (idur >* 0.75) 0.64 (idur - 0.085)
          a1  = env * osc (sig $ cpspch pch)  

-- | Takes a duration and a pitch and makes a track with that one note.
i1 :: (Num a, Fractional c) => a -> b -> Track a (c, b)
i1 dur pch = dur *| temp (0.5, pch)

res = sco instr $ line [
    i1   0.750   7.04,
    i1   0.250   7.07,
    i1   1.000   8.00,
    i1   0.200   8.02,
    i1   0.200   8.04,
    i1   0.200   8.05,
    i1   0.200   9.00,
    i1   0.200   9.04,
    i1   0.250   9.05,
    i1   0.250   9.00,
    i1   0.250   8.05,
    i1   0.250   8.00,
    i1   1.000   7.04,
    i1   0.125   7.07,
    i1   0.125   8.00,
    i1   0.125   8.02,
    i1   0.125   8.04,
    i1   0.125   8.05,
    i1   0.125   9.00,
    i1   0.125   9.04,
    i1   0.125   9.05]

main = dac $ runMix res
