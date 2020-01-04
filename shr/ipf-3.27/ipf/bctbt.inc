C
        common /bctbt/ bctbt(11,MAXCBS)
        integer kbctbt(11,MAXCBS)
        equivalence (bctbt,kbctbt)
        common /bctblc/ custyp(MAXCBS), cbkyr(MAXCBS), cbown(MAXCBS)
        character custyp*1, cbkyr*2, cbown*3
C
