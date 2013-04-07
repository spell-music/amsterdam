{- | additional parameters: irise, idec, ifundr

This additive instrument shows individual an amplitude envelope and frequency for each 
partial. Rise and decay times are specified by the variables irise and idec. It is a 
simplification of instrument DataTrumpet2.hs and based on physical analysis data of trumpet tones.

The frequency of the partials is subject to small random fluctuations. The range of these 
fluctuations is set to 6% of the fundamental frequency for all partials. (Risset 1969: #200)
-}
module DataTrumpet1 where

import Data.List(splitAt)
import Csound.Base

instr :: (D, D, D, D, D) -> SE Sig
instr (amp, cps, fundr, rise, dec) = do
    rnd <- randi (kr $ fundr * 0.06) 10
    return $ linen (kr amp) rise idur dec * osc (kr cps + rnd)


note fundr = chord . fmap (phi fundr)
    where phi fundr (dur, amp, cps, rise, dec) = dur *| temp (amp/8000, cps, fundr, rise, dec)

-- first note, 9 harmonics
ch1 = [
    (0.17,    200,  554,    0.0006,  0.0112), 
    (0.17,    160, 1108,    0.0006,  0.0088),
    (0.17,    350, 1662,    0.00096, 0.0068),
    (0.15,    310, 2216,    0.00112, 0.0064),
    (0.14,    160, 2770,    0.00192, 0.0052),
    (0.14,    200, 3324,    0.00216, 0.0048),
    (0.14,     99, 3878,    0.00256, 0.0048),
    (0.14,    200, 4432,    0.0024,  0.0048),
    (0.14,     80, 4986,    0.0028,  0.00480)]

-- second note, 14 harmonics
ch2 = [
    (0.15,     50,  293,    0.0008,  0.0112),
    (0.15,     80,  586,    0.0008,  0.0112),
    (0.15,    100,  879,    0.00096, 0.0068),
    (0.15,    175, 1172,    0.00136, 0.0064),
    (0.15,    180, 1465,    0.002,   0.0052),
    (0.15,    150, 1758,    0.0024,  0.0048),
    (0.15,    100, 2051,    0.0028,  0.0048),
    (0.13,     80, 2344,    0.0028,  0.0048),
    (0.14,     50, 2637,    0.0032,  0.008),
    (0.14,     80, 2930,    0.0032,  0.008),
    (0.14,    140, 3223,    0.0036,  0.008),
    (0.13,     90, 3516,    0.0036,  0.008),
    (0.13,     45, 3809,    0.0032,  0.008),
    (0.13,     25, 4102,    0.0032,  0.0072)]

res = sco instr $ chord [
    0    +| note 554 ch1,
    2    +| note 293 ch21,
    2.01 +| note 293 ch22]
    where (ch21, ch22) = splitAt 5 ch2

main = writeCsd "tmp.csd" res
-- main = totem res

