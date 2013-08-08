{- |  additional parameters: irise, idec

This run, again translated from the original Music 5 instrument of Risset, 
presents an economical way to synthesize brass tones, or in more general 
terms, sounds whose spectra depend upon the amplitude of one component 
(Mathews and Risset: 1969).

The design is as follows: LINSEG produces one amplitude envelope (0 < iamp < 120) 
which leads straight to the first sinus oscillator and serves to calculate 
relative amplitude values for the harmonics (in this particular case). The 
modification imposed by iratio and iconst will lead to an increase of the 
higher harmonics, as a function of amplitude.

For example, for iamp = 100, the seventh harmonic is seven times as strong 
as at iamp = 60. During the rise time of the tone, the higher harmonics 
will increase more rapidly.

On the other hand they will die away sooner during the decay period.

The linear scheme that governs this behaviour is laid out in the figure on 
page 46. (Risset 1969: #210)  
-}
module Plastic1 where

import Csound


plastic :: Sig -> (D, D) -> Sig
plastic e (amp, cps) = sum $ zipWith (\env fq -> env * osc (sig $ fq * cps)) envs fqs
    where ratios = [1, 0.2/67, 0.3/60, 0.4/57, 0.5/55, 0.6/54, 0.7/54]
          consts = [0.054, 0.049, 0.3, 0.409, 0.511, 0.596]  
          fqs    = [1 .. 7]
          strs   = [0.2, 0.4, 0.37, 0.23, 0.2, 0.18]         
          envs   = zipWith3 (\str rat con -> str * (e * sig amp * rat + con)) strs ratios consts
       
instr :: (D, D, D, D) -> Sig
instr (amp, cps, rise, dec) = plastic e (amp, cps)
    where e = ar $ linseg [0, rise', 45/88, idur - (rise' + dec), 1, dec, 0]
          rise' = rise*100/128         
         
res = sco instr $ line [
    5 *| temp (0.4, 554, 0.05, 0.25),
    rest 1,
    line [ chord [rest 1, 0.6 *| temp (amp, 682, 0.05, 0.15)] | amp <- [0.1, 0.15, 0.2, 0.25]],
    0.6 *| temp (0.3, 682, 0.05, 0.2)]

main = dac $ runMix res
