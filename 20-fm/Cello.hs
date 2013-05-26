{- | additional parameters: irise, idec, ivibdel, ivibwdth, ivibrte

A realistic string tone emulation achieved by complex wave FM modulation. 
The three components of the complex wave are independent in their modulation 
indices and c:m ratios, thus allowing great and precise control over the 
emerging spectra. An attack noise portion and a vibrato add to the realism 
of the design. The timeout flow-control statement of the csound language 
is used to mix the attack, vibrato and normal portions of the instrument. 
The parameters 'inoisdur' and 'ivibdel' control the duration time of the 
attack noise and the delay time of the vibrato respectively. This design 
can doubtlessly be put to refined use in other contexts. 
(Schottstaedt 1977; Vercoe 1993: morefiles/string.orc)
-}
module Cello where

import Csound

instr :: (D, D, D, D, D, D) -> Sig
instr (amp, pch, rise, dec, vibwth, vibrte) = undefined -- timout  ???

main = writeCsd "tmp.csd" res


