{- | additional parameters: none

This instrument generates 4 seconds of white noise.
-}
module White where

import Csound.Base

instr :: D -> SE Sig
instr amp = rand (linen (kr amp) rise idur dec)
    where rise = 0.4
          dec  = 0.4  

res = sco instr $ 4 *| temp 0.5

main = writeCsd "tmp.csd" res

