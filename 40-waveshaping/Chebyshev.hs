{- additional parameters:

We have implemented GEN 13 in this simple waveshaping instrument. 
GEN 13 will generate a polynomial weighted transfer function table 
in accordance with a series of Chebychev coefficients. In this 
example, one can listen to the results produced by f88, f89 and f90.

For linear mapping, it is necessary to use the same table sizes 
for the input (here: 8193 for sinus f1) to the waveshaper and 
the transfer function (here: 8193 for the tables f88-f90).

For GEN 13, the parameter p6 of the f-statement needs to be 
set to int(table length/2).
-}
module Chebyshev where

import Csound

instr :: (D, D, Tab) -> Sig
instr (amp, pch, tf) = a1 * env
    where a1  = tablei (4096 * osc (sig $ cpspch pch)) tf
          env = linen (sig amp) 0.085 idur 0.04
            
chebs = setSize 8193 . chebs1 4096 1

f88 = chebs [1, 1]
f89 = chebs [1, 1, 1, 2]
f90 = chebs [1, 1, 0, 0, 0, 0, 0, 6, 5, 4]  

note tab dur pch = dur *| temp (0.5, pch, tab)

i1 = note f88
i2 = note f89
i3 = note f90

res = sco instr $ line [
    i1   0.750   7.04,
    i1   0.250   7.07,
    i1   1.000   8.00,
    i2   0.200   8.02,
    i2   0.200   8.04,
    i2   0.200   8.05,
    i2   0.200   9.00,
    i2   0.200   9.04,
    i2   0.250   9.05,
    i2   0.250   9.00,
    i2   0.250   8.05,
    i2   0.250   8.00,
    i3   1.000   7.04,
    i3   0.125   7.07,
    i3   0.125   8.00,
    i3   0.125   8.02,
    i3   0.125   8.04,
    i3   0.125   8.05,
    i3   0.125   9.00,
    i3   0.125   9.04,
    i3   0.125   9.05]

main = dac $ runMix res

