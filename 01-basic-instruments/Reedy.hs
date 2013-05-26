{- | This design features a constant waveform with 10 weighted
harmonics.  The envelope is variable and demonstrates well how the
sole influence of the amplitude envelope can influence the sound
quality and the perceived timbre.  The envelope functions f31 and
f32 insure long attacks and decays ( > 50 msec ) and legato
transitions between successive notes.

01_01_2A is the translation of a part of Risset's "Reedy and
Plucked Tones, Choral Effect": only the reedy tones appear.  The
same Britannic folk melody is repeated, but each time with a
different envelope.  The leading tone is a bit lower than in the
equally tempered system. (Risset 1969: #250)
-}
module Reedy where

import Csound

reedy :: (D, D, Tab) -> Sig
reedy (amp, cps, env) = once env * oscili (sig amp) (sig cps) wave
    where wave = sines [0.4, 0.3, 0.35, 0.5, 0.1, 0.2, 0.15, 0, 0.02, 0.05, 0.03]

envelopes = [
    elins [0, 1, 0.8, 0.6, 0.7, 0.6, 0],
    elins [0, 0.7, 0.8, 1, 0.5, 0.5, 0],
    skipNorm $ lins [0, 1, 0.6, 100, 0.9, 150, 0.7, 100, 0.8, 80, 0.6, 72, 0],
    skipNorm $ lins [0, 1, 0.5, 209, 0.6, 230, 0.5, 72, 0],
    skipNorm $ lins [0, 1, 0.6, 19,  0.9, 300, 0.3, 192, 0],
    skipNorm $ lins [0, 1, 0.5, 39,  0.8, 260, 0.2, 212, 0]]

amp = 0.5

note tab dur cps = dur *| temp (amp, cps, tab)

melody dt tab = delay dt $ line $ fmap (uncurry $ note tab) [
    (0.5,   486), (0.25,  615), (0.25,  648), (0.5,   729),
    (0.25,  972), (0.25,  890), (0.25,  820), (0.25,  729),
    (0.5,  820), (2.0,   729)]             

res = sco reedy $ chord $ zipWith melody [0, 6 ..] envelopes

main = writeCsd "tmp.csd" res
-- main = totem res
