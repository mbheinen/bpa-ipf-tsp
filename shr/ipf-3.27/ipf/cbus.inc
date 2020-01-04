 
        common /cbus/  bctbl(12,MAXCBS), bctbl_nxt(0:MAXCBS)
        integer kbctbl(12,MAXCBS), bctbl_nxt
        equivalence (bctbl,kbctbl)
        real bctbl

