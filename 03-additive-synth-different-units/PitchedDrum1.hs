{- | additional parameters: none

This is a more efficient implementation of Snare.hs: some constant parameters 
have been moved into the orchestra, and the amplitude of the three building 
blocks is expressed as function of the main iamp variable. (Vercoe 1993: 
morefiles/risset2.orc; Vercoe 1993: morefiles/drum.orc)
-}
module PitchedDrum1 where

import Csound
import qualified Snare as S(instr)

instr :: (D, D) -> SE Sig
instr (amp, cps) = S.instr (amp, cps, amp * 0.8, amp * 0.3, cps * 0.1, 1500)

note a = temp (0.5, a)

sec1 = line $ fmap note $ fmap (* 100) [1 .. 10]
sec2 = line $ fmap note $ fmap (* 100) [10, 9 .. 1]

notes = line [sec1, sec2]

res = sco instr notes

main = dac $ runMix res

