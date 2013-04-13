{- additional parameters: several functions, ampfac

Soprano timbre achieved through a double carrier FM design. The amplitudes 
of the two carriers are controlled by different envelopes, the index envelope 
is the same for both carriers. The table lookup is used to get pitch dependent 
values for the parameters icarhz, ifmthz, imax1 and imax2. Again, a vibrato 
generator adds realism to the construction. (Chowning 1980, 1989; Vercoe 1993: 
morefiles/chowning.orc; Vercoe 1993: morefiles/ sopink.orc)
-}
module DoubleCarrierSoprano where

import Csound.Base

instr :: (D, D) -> SE Sig
instr (amp, pch) = do
    v <- vibrato
    let dev1   = sig pkdev1 * osc (sig modhz * v)       -- modulator
        dev2   = dev1 * sig (fmtndx' / fndndx')      -- rescale for formant
        fund   = fundEnv * osc ((sig carhz + dev1) * v)
        form   = formEnv * osc ((sig fmthz' + dev2) * v)
    return $ (fund + form) * sig amp       
    where
        ampfac = 0.5
        range  = 3
        base   = octpch 7.07
        caroct = octpch pch
        carhz  = cpspch pch

        -- ipoint points at p5-dependent location of a table with 512 locs
        point  = 511.999 * (caroct - base) / range
        segs'  = setSize 513 . segs
        lookAt = tableD point . segs'
        fenv   = segs' [0, 256, 0.2, 256, 1]
        fport  = segs' [-1, 200, 1, 100, 0, 212, 0]
        fmthz  = lookAt [1, 80, 1, 200, 0.9, 200, 0.6, 32]
        fmtfac = lookAt [0.4, 100, 0.2, 412, 1]
        fmtndx = lookAt [1, 100, 0.5, 80, 0.25, 132, 0.5, 100]
        fndndx = lookAt [1, 100, 1, 112, 0.4, 300, 0.15]
        
        fmthz'  = intD( 0.2 * fmthz / carhz + 0.5) * carhz
        fmtfac' = 0.1 * fmtfac
        fndfac' = 1 - fmtfac'
        fmtndx' = 5 * fmtndx
        fndndx' = 25 * fndndx     

        fndamp  = fndfac' * sqrt ampfac
        fmtamp  = fmtfac' * ampfac * sqrt ampfac

        modhz   = carhz
        pkdev1  = fndndx' * modhz

        -- vibrato paramterers
        ivibwth = 0.002 * logBase carhz 2
        vibhz   = 5
        randhz  = 125
        portdev = 0.05

        -- vibrato
        rnd     = randi (sig ivibwth) randhz     -- random contribution
        vib     = linen (sig ivibwth) 0.6 idur 0.1 * osc vibhz
        port    = portdev * kr (oscBy fport 0.2)
        vibrato = fmap ((1 + vib + port) + ) rnd         

        -- envelopes
        dyn     = linseg  [0, 0.03, 1, idur - 0.03 - 0.01, 0.9, 0.01, 0]
        fundEnv = linen (sig fndamp) 0.1 idur 0.08
        formEnv = envlpx (sig fmtamp) 0.1 idur 0.08 fenv 1 0.01

        
i1 start dur amp pch = delay start $ stretch dur $ temp (amp, pch)

res = sco instr $ stretch (40/60) $ chord [
    -- p6 = 0 < ampfac < 1                  
    --            idur      iamp    ipch  
    i1     0      0.5      12000    9.01,     
    i1     0.4    0.5       7000    8.09,  
    i1     0.8    0.4       6000    8.05,  
    i1     1.2    1.5      12000    8.08,  
    i1     3.0    0.5       8000    8.07,
    i1     3.4    1.0      18000    9.06,  
    i1     4.8    0.5      14000    9.07,  
    i1     5.2    0.45     10000    9.03,  
    i1     5.6    0.5       4000    8.11,  
    i1     6.0    1.4       8000    9.02,  
    i1     7.4    0.5      12000    8.10,  
    i1     7.8    2        14000    9.01] 

main = writeCsd "tmp.csd" res
