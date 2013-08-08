{- | additional parameters: ibuf, if1

One noise has been selected for further investigation: f73 (bandwidth is 2500 Hz). 
This noise yields the best quality of a plucked string tone.

Notes of various duration and frequency.
-}
module FiveFrequencies where

import Csound



main = dac $ runMix res


