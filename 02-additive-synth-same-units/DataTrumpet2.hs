{- | additional parameters: if2, ifundr

In this example Risset used data from real trumpet tones to 
synthesize brass-like sounds. The amplitude envelopes of the 
partials are individually specified as breakpoint functions. 
(Mathews and Risset 1969)

Additionally, the frequencies of the components are random 
modulated at a rate of 10 Hz with an amplitude of 4% of the 
fundamental frequency. Both values are low; the random frequency 
fluctuation plays a minor part in this particular tone quality. 
(Risset 1969: #200)

Data for other analysis-based additive synthesis instruments is 
published by Moorer: cello, trumpet and clarinet (Moorer 1985).    
-}

module DataTrumpet2 where

import Csound

instr :: (D, D, Tab, D) -> SE Sig
instr (amp, cps, env, fundr) = do
    rnd <- randi (sig $ fundr * 0.04) 20
    return $ sig amp * once env * osc (sig cps + rnd)

envelopes = fmap lins [
    [0, 20, 0.001,  32,  0.282,  28,  0.112, 778,  0.178, 88,  0.159, 54,  0.008, 24,  0.001],
    [0, 38, 0.001, 830,  0.5,    40,  0.355,  72,  0.016, 44,  0.001],
    [0, 46, 0.001, 824,  0.56,  144,  0.001],
    [0, 20, 0.001,  18,  0.005, 798,  0.224,  26,  0.224, 54,  0.178, 108,  0.001],
    [0, 20, 0.001,  22,  0.009,  24,  0.089,  24,  0.022, 56,  0.022, 306,  0.112, 76,
                       0.178, 162,  0.071, 246,  0.062,  88,  0.001]]

note dur amp fundr = ( =:= rest 1) . chord . fmap phi . flip zip envelopes
    where phi (cps, env) = dur *| temp (amp, cps, env, fundr)

cps1 = [784, 1568, 2352, 3136, 3920]
cps2 = [830, 1660, 2490, 3320, 4150]

res = sco instr $ line [
   note 0.4 (0.5/5) 784 cps1,
   note 0.7 (0.6/5) 830 cps2]

main = dac $ runMix res

