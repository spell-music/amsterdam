{- | This run plays the "Manhattan blues on tape M1485" with a sound
reminiscent of an electric piano.
The design is a subtle variation of WaveAndEnvelope.hs, with both envelope
and wave controllable on a per note level. Note that the tempo
statement alters the meaning of durations in the score file. The
envelope varies with duration and the harmonic richness with
pitch. This means that we distinguish four kind of notes:

1) brief and low (<.2 sec, <250 hz) 

The function f31 will give a sharp attack. While the first two decay 
fragments approximate an exponential shape, the third tries to imitate 
the effect of a damper. The waveform has ten harmonics. 

2) brief and high (<.2 sec, >250 Hz)
 
The same envelope f31, but now coupled with a waveform with only
seven harmonics.

(2 different types of envelopes: brief and long notes)

3) long and low (>.2 sec, <250hz) 

Exponential envelope f51 teams up with a 10 harmonics wave. The envelope 
minimum of 2(**-6)="1/64" produces 6/10 of "reverberation time" 
(time for the level to drop to 60 dB). The durations of the longer notes 
range typically from .4 to .8 sec. Compared to a real piano (1s at 2000 Hz, 
up to 10s at 200 Hz), the reverberation times are somewhat shorter here. 
On the other hand, the discrepancy is lessened by the fact that the initial 
decay rate in a real piano is higher. 

4) long and high (>.2 sec, >250 Hz)

The last option is covered by the exponential envelope f51 and a
7 harmonics wave. (Risset 1969: #301)
-}

module ManhatanBlues where

import Csound.Base

piano :: (D, D) -> Sig
piano (amp, cps) = once envelope * oscili (kr amp) (kr cps) timbre
    where envelope = ifB (idur <* 0.2) sharpAttack exponEnv
          timbre   = ifB (cps <* 250) sin10 sin7 
              
          sharpAttack = segs [0, 1, 0.99, 20, 0.4, 20, 0.2, 10, 0]
          exponEnv    = guardPoint $ exps [0.0156, 1, 1, 350, 0.0156] 
          
          sin7  = sines [1, 0.282, 0.089, 0.1, 0.071, 0.089, 0.050]
          sin10 = sines [0.158, 0.316, 1, 1, 0.282, 0.112, 0.063, 0.079, 0.126, 0.071]

-- chords:          
c1 = [104, 175, 233, 277, 330]
c2 = [207, 349, 440, 554]
c3 = [104, 147, 165, 196, 233]
c4 = [207, 294, 330, 392, 494]    
    
section = stretch 0.4 $ line [
    chord [note 1.66 0.45 c1, delay 0.5 $ note 1.16 0.4 c2],
    note 0.34 0.24 c1,
    note 1 0.5 c2,
    note 1 0.6 c3,
    note 1 0.45 c4]    
    where note dur amp xs = chord [dur *| temp (amp, x) | x <- xs]
      
res = sco piano $ loop 4 section

main = writeCsd "tmp.csd" res
-- main = totem res


