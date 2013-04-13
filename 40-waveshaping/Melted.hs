{- additional parameters: itf

The sinus which is applied to the transfer function oscillates between 
the values of +255 and -255. The maximum amplitude value is in general 
half the table length of the transfer function.

The incoming sinus is translated according to the transfer function. 
For a strict 1:1 linear mapping, the GEN functions in this example 
should have been set to maximum ordinate value of 256 (instead of 1), 
plus (not to forget) inhibiting rescaling of the table. Preceding 
the GEN routine number by a minus sign inhibits normalization of the 
values. In this example, the ordinate is in a range +-1, so the translation 
will produce a normalized signal. Finally, the multiplier will scale the 
signal to the desired maximum amplitude.

The first section plays 5 different transfer functions at the same 
pitch and amplitude.

The three linear ones are shown in the flow chart, a parabolic 
(diabolic ?) and a cubic function follow.

The most interesting tone is produced by f33. In the end of the notes, 
one hears how the timbre melts. The remaining two section play a dominant 
7th chord 'on' that transfer function. (Dodge 1985: pp. 130-132)
-}
module Melted where

import Csound.Base

instr :: (D, D, Tab) -> Sig 
instr (amp, pch, tf) = sig amp * tablei (a1 + 256) tf
    where a1 = linen 255 0.1 idur 0.5 * osc (sig $ cpspch pch)

segs' = setSize 512 . skipNorm . segs

tfms = [
    -- f(x) = x
    segs' [-1, 512, 1], 
    -- linear discontinuous
    segs' [-1, 256, 0, 50, 0.2, 1, 0, 205, 1],
    -- linear with sharp points
    segs' [0, 103, -0.2, 103, -1, 100, 1, 206, 0],
    -- even: f(x) = x**2
    setSize 513 $ polys (-1) 1 [0, 0, 1],
    -- odd: f(x) = x**3
    setSize 512 $ polys (-1) 1 [0, 0, 0, 1]]

pchs = [7.00, 8.00, 9.04, 8.07, 8.11]

i1 tfm = 2 *| temp (0.5, 7.00, tfm)
i2 pch = temp (0.4, pch, tfms !! 2)
i21 = (0.5 *| ) . i2
i22 = (2   *| ) . i2

res = sco instr $ line $ fmap line [
    fmap i1  tfms,
    fmap i21 pchs,
    fmap i21 pchs]

main = writeCsd "tmp.csd" res
