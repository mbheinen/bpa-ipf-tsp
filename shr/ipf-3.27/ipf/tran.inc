c
        common /tran/ tap(MAXLTC), phsflg(2,MAXLTC), tran(12,MAXLTC)
        double precision tap, phsflg
        real tran
        integer ltran(12,MAXLTC)
        equivalence (tran, ltran)
c
