{- | additional parameters: iamp2, iamp4, ifq5, ifq7

This instrument forms the sum of a noise band, sine wave and 
an inharmonic spectrum.

Noise band. Parameter ifq5 tells us the center frequency of 
the noise band, ifq7 gives half of its bandwidth and its 
envelope is controlled by the exponential f52 (1-2-12).

Fundamental. The wave of function 12 plays the 10th harmonic 
of the fundamental frequency ifq1. Setting ifq1 to 20Hz will 
produce a sinus at 200 Hz.

Inharmonic spectrum. The parameter ifq1 is used again to build 
an inharmonic spectrum from a wave that contains only high 
frequency components (harmonics 10,16,22 & 23).

Section 1 plays a snare-like tone, with a noise centered at 
4000 Hz and a bandwidth of 3000 Hz.

Section 2 turns the snares off (iamp7 = 0) and plays four 
pitches: 120, 140, 150 and 160 Hz.

In section 3 the snares return with a rhythmic pattern. 
(Risset 1969: #400)
-}
module Snare where

import Csound

envOsc :: Tab -> Tab -> Cps -> Sig
envOsc env wave cps = once env * oscBy wave cps

instr :: (D, D, D, D, D, D) -> SE Sig
instr (amp7, fq1, amp2, amp4, fq5, fq7) = do
    r <- randi (sig amp7) (sig fq7) 
    return $ mean $ zipWith (*) [r, sig amp4, sig amp2] $ 
        zipWith3 envOsc [f52, f52, f51] [f11, f13, f12] (fmap sig [fq5, fq1, fq1])  
    where f51 = guardPoint $ eexps [4096, 1]
          f52 = guardPoint $ eexps [256, 1]  

          f11 = sine
          f12 = partials [10]  
          f13 = partials [10, 16, 22, 23]  

          
note ps (start, dur) = delay start $ stretch dur $ temp ps

sec11 = chord $ fmap (note (0.5, 20, 0.4, 0.2,  4000, 1500)) [
    (0.4,   0.2),   
    (0.8,   0.2),
    (1.1,   0.15),
    (1.2,   0.2),
    (1.6,   0.2),
    (1.9,   0.15),
    (2.0,   0.2),
    (2.4,   0.2),
    (2.8,   0.2),
    (3.1,   0.15),
    (3.2,   0.2),
    (3.6,   0.2),
    (3.9,   0.15),
    (4,     0.2),
    (4.4,   0.2),
    (4.8,   0.2),
    (5.2,   0.2),
    (5.6,   0.2)]

sec12 = note (0.6, 20, 0.4, 0.2,  4000, 1500) (6, 0.2)

sec1 = chord [sec11, sec12]

note2 (amp2, amp4, fq5, fq7) (start, dur, fq1) = note (0, fq1, amp2, amp4, fq5, fq7) (start, dur)

sec2 = chord $ fmap (note2 (0.5,  0.3, 0, 2.5)) [
    (0.4,    0.3,       12),
    (0.8,    0.2,       16),
    (1.07,   0.2,       12),
    (1.2,    0.2,       16),
    (1.6,    0.3,       12),
    (2.0,    0.25,      14),
    (2.4,    0.23,      15),
    (2.6,    0.27,      15),
    (3.07,   0.23,      15),
    (3.2,    0.23,      15),
    (3.6,    0.23,      15),
    (4,      0.23,      15)]

note3 (fq1, amp2, amp4, fq5, fq7) (start, dur, amp) = note (amp / 2000, fq1, amp2, amp4, fq5, fq7) (start, dur)

sec3 = chord $ fmap (note3 (20, 0.4, 0.2, 4000, 1500)) [
    (0.4,   0.15,   1000),
    (0.6,   0.20,   1000),
    (1.07,  0.20,   1000),
    (1.2,   0.20,   1000),
    (1.6,   0.20,   1000),  
    (2.0,   0.20,   1000),
    (2.4,   0.25,   1000),
    (2.9,   0.15,   1000),
    (3,     0.15,   1000),
    (3.1,   0.15,   1000),
    (3.2,   0.20,   1000),
    (3.55,  0.15,   700),
    (3.6,   0.20,   700),
    (4,     0.15,   800),
    (4.06,  0.15,   800),
    (4.13,  0.15,   800),
    (4.20,  0.15,   800),
    (4.27,  0.15,   800),
    (4.33,  0.15,   800),
    (4.40,  0.22,   900)]

res = sco instr $ line [sec1, sec2, sec3]

main = writeCsd "tmp.csd" res
-- main = totem res

