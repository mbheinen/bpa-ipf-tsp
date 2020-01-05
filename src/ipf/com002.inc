C
        common /ctl003/ incsw,lctl,lolb,lrpt,lail,lltc,laic,ltrac,
     1   lopt,lan2,lsoli,ltol,llim,ldeb,lcon,numtxf,trapnt(7),
     2   itepnt(3),tolpnt(5),limpnt(10),debpnt(7),conpnt(5), lansel
 
        integer tolpnt,limpnt,debpnt,conpnt,itepnt,trapnt,numtxf
 
        common /ctl004/ ctldic(59), debdic(8), lstopt(5),
     1   anropt(4), anrdic(2), soldic(3),
     2   toldic(6),limdic(12),traced(7),condic(5),
     3   olbdic(2),rptdic(3),aildic(4),ltcdic(3),aicdic(3),effdic(3),
     4   selanl(23)
 
        character*10 ctldic, lstopt, anropt, anrdic, debdic,
     1     soldic, toldic, limdic, traced, condic, olbdic, rptdic,
     2     aildic, ltcdic, aicdic, effdic, selanl
 
