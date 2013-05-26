{- additional parameters: ifqm, imax, ibegkdyn, ibegae, imid, ibreakp, iend, irvt, ileft

The instrument has a variable amplitude envelope and a variable modulation 
index contour. Part of the sound is reverberated and mixed with the unreverberated 
portion.

For this instrument, a network score generator has been written (Spruit 1993). 
It provides unique sets of scores. The Prolog program defines a network of 
relationships between parameters. The user specifies the valid range of the 
individual parameter values and a start value, which is the requested total 
duration of the score in seconds.

Also, one can choose to generate more than one cycle during one run of the 
score generator. All score file cycles run parallel to each other.

The network constitutes a grammar and the generation process involves no 
randomness. A parameter can influence its own value (recursive) and/or 
the value of other parameters.

In this way all parameter values of the score are determined by specific 
mathematical relations between them. The result is written to a score file.
-}
module Network where

import Csound

instr :: (D, D, D, D, (D, D, D, D, D), D, D) -> Sig2
instr (amp, fqc, fqm, max, (bege, begdyn, breakp', mid, end), rtv, left) = (mixSig * sig left, mixSig * sig (1 - left))
    where breakp = idur / breakp'
          e   = sig amp * expseg [0.0001, breakp, mid, idur - breakp, 0.0001]
          dyn = sig (fqm * max) * linseg [begdyn, breakp, 1 - mid, idur - breakp, 0]
          mod = dyn * osc (sig fqm) 
          s   = e   * osc (sig fqc + mod)
            
          rev = reverb (s * sig rtv) (sig idur)
          drySig = s * (1 - sig rtv)
          mixSig = rev + drySig


i start dur amp fqc fqm max beg begdyn breakp mid end rtv left = delay start $ stretch dur $
    temp (amp/8000, fqc, fqm, max, (beg, begdyn, breakp, mid, end), rtv, left)

res = sco instr $ chord [
--        start     idur      iamp       ifqc      ifqm     imax     ibeg      ibegkdyn    ibreakp  imid      iend      irtv      ileft
    i      0.00      4.99      3066       259       360       0.1     0.000    0.004        46     0.866      0         0.23      0.18,
    i      4.99      2.58      3508       514        71       0.1     0.000    0.010        49     0.966      0         0.21      0.25,
    i      7.58      1.21      3695      4844       274       0.1     0.000    0.012         2     0.926      0         0.31      0.89,
    i      8.79      3.27      2098      2193        31       0.2     0.000    0.015        26     0.049      0         0.75      0.52,
    i     12.07      0.18      4493      3830       323       0.2     0.000    0.018        16     0.366      0         0.74      0.23,
    i     12.26      2.73      3099       205        27       0.3     0.000    0.022        47     0.889      0         0.23      0.12,
-- second cycle
    i      0.00      0.69      4509      1585       355       0.7     0.000    0.049         7     0.369      0         0.52      0.12,
    i      0.69      4.05      2504      1192        65       0.8     0.000    0.059        37     0.472      0         0.57      0.24,
    i      4.75      1.17      4601      2328       282       1.0     0.000    0.070        32     0.269      0         0.84      0.80,
    i      5.93      3.33      2056      2123        22       1.2     0.000    0.085        27     0.079      0         0.83      0.50,
    i      9.26      0.10      4557      3724       334       1.4     0.000    0.102        17     0.322      0         0.84      0.30,
    i      9.36      4.94      3194        47        40       1.7     0.000    0.122        48     0.956      0         0.25      0.25,
    i     14.30      0.69      3566       610       312       2.1     0.000    0.147        49     0.995      0         0.34      0.45]
    
main = writeCsd "tmp.csd" res
-- main = totem res


