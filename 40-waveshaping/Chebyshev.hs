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

-- | Takes an amplitude (between 0 and 1), pitch (as 8ve.pc. i.e. 8.00 is middle C, 8.01 is D) and table describing the sound and makes a signal matching Chebyshev coefficients.
instr :: (D, D, Tab) -> Sig
instr (amp, pch, tf) = a1 * env  -- put the envelope and amplitude together
    where 
          -- make a signal matching the pitch 
          a1 :: Sig
          a1  = tablei (4096 * osc (sig $ cpspch pch)) tf
          -- a sound envelope based on the amplitde (amp) and duration (idur)
          env :: Sig
          env = linen (sig amp) 0.085 idur 0.04
            
-- | Takes a list of Chebyshev coefficients and produces a table to generate a signal. 
chebs :: [Double] -> Tab            
chebs = setSize 8193 . chebs1 4096 1

-- | Produces a table for Chebyshev f88.
f88 :: Tab
f88 = chebs [1, 1]
-- | Produces a table for Chebyshev f89.
f89 :: Tab
f89 = chebs [1, 1, 1, 2]
-- | Produces a table for Chebyshev f90.
f90 :: Tab
f90 = chebs [1, 1, 0, 0, 0, 0, 0, 6, 5, 4] 


-- | Takes a table, duration and pitch and makes a track with that one note.
note :: Tab -> D -> D -> Sco (D, D, Tab)
note tab dur pch = dur *| temp (0.5, pch, tab)

-- | Takes a duration and a pitch and makes a track with that one note with the Chebyshev f88.
i1 :: D -> D -> Sco (D, D, Tab)
i1 = note f88
-- | Takes a duration and a pitch and makes a track with that one note with the Chebyshev f89.
i2 :: D -> D -> Sco (D, D, Tab)
i2 = note f89
-- | Takes a duration and a pitch and makes a track with that one note with the Chebyshev f90.
i3 :: D -> D -> Sco (D, D, Tab)
i3 = note f90

-- | A sample list of notes, played by the Chebyshev instrument, to be rendered by the main function.
res :: Sco (Mix a)
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

-- | Takes the res sample above and renders it to the soundcard in real time.
main :: IO ()
main = dac $ runMix res

