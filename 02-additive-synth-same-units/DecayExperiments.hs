{- | additional parameters: if1, if2

This is the additive part of Risset's 'Linear and Exponential Decay Experiments'. 
In his score the composer uses only three parallel building blocks, but this can 
be extended to any number, if there is a wish to do so.

In section 1 the waves with the higher harmonic content decay faster. This situation 
is often encountered in natural vibrating systems. The example of section 2 adds a 
slight detuning to add liveliness. In section 3, the waves with lower harmonic content 
decay first (not often found in nature), and section 4 adds a small detuning once more. 
The noise in sections 3 and 4 stems from the foldover components of the square wave f31. 
Section 5 repeats section 1, and the last section detunes the three oscillators just a 
little more than was the case earlier on. (Risset 1969: #300)
-}
module DecayExperiments where

import Csound

instr :: (D, Tab, D, Tab) -> Sig
instr (amp, env, cps, wave) = sig amp * once env * oscBy wave (sig cps)

waves = [
    lins [0, 10, 1, 20, 1, 20, -1, 20, -1, 10, 0],
    sines [1, 0.5, 0.3, 0.2, 0.15, 0.12],
    sines [1, 0.2, 0.05]]
    
linEnv = elins [1, 0]
expEnv = guardPoint $ eexps [256, 1]

ch env cs waves = line [chord $ zipWith3 (note env) [(0.1, 0.5), (1.8, 0.2), (3, 0.1)] cs waves, rest 1]
    where note env (dt, amp) cps wave = dt *| temp (amp, env, cps, wave)
    
ch1 = ch linEnv
ch2 = ch expEnv    
    
cps1 = repeat 440
cps2 = [443, 440, 441]
cps3 = [448, 440, 444]
    
res = sco instr $ line [
        line $ fmap (uncurry ch1) qs,
        line $ fmap (uncurry ch2) qs]
    where qs = [(cps1, waves), (cps2, waves), (cps1, reverse waves), 
                (cps2, reverse waves), (cps1, waves), (cps3, waves)]
                
main = writeCsd "tmp.csd" res
-- main = totem res
