
        parameter ( MXCNBR = 4*MAXBRN/10 )   ! contingency candidates
        parameter ( MXCPBS = MAXBUS )        ! contingency anal buses
        parameter ( MXOLBR = 4*MAXBRN/10 )   ! br checked for overloads
        parameter ( MXYBR  = 5*MXCNBR/2  )   ! reduced upper-y-matrix
        parameter ( MXCPWK = 2*MXYBR + 2*MXCPBS)
        parameter ( MXIBAD = 25*MXOLBR )

        common /apcom/ iq, ipfbp(MXCPBS+1), ipfbpp(MXCPBS+1),
     1                 jfbp(MXCPWK), jfbpp(MXCPWK), fbp(MXCPWK),
     2                 fbpp(MXCPWK), dgbp(MXCPBS), dgbpp(MXCPBS),
     3                 x1(MXCPBS), x2(MXCPBS), c1, c2, q(MXCPBS),
     4                 delp(MXCPBS), delq(MXCPBS), rpad1(2,MXYBR),
     5                 rpad2(2,MXYBR), rpad3(2,MXYBR), vi(2,MXCPBS),
     6                 ei(MXCPBS), thi(MXCPBS),
     &                 ipfbpp_save(MXCPBS+1), jfbpp_save(MXCPWK),
     &                 fbpp_save(MXCPWK), dgbpp_save(MXCPBS)

         common /apin/ nbus, nout, novl, igenq, ipqv(MXCPBS), 
     1                 ipyu(2,MXCPBS), mfaru(MXYBR), ipyl(2,MXCPBS),
     2                 mfarl(MXYBR), klnc(4,MXCNBR), klno(7,MXOLBR),
     3                 iqlim(MAXTBX), itmax, vmag(MXCPBS),
     4                 vangle(MXCPBS), vlow(MXCPBS), vhi(MXCPBS),
     5                 ykmu(2,MXYBR), ykml(2,MXYBR), cykk(2,MXCPBS),
     6                 pnet1(MXCPBS), qnet1(MXCPBS), qlow(MAXTBX),
     7                 qhi(MAXTBX), shunt(MAXTBX), clnc(7,MXCNBR),
     8                 clno(12,MXOLBR), cx1, cx2, cx4, ipqsln,
     9                 vhold(MAXTBX), qstate(MAXTBX), clnobase(MXOLBR)
        integer qstate

        common /aplis/ iic, ibrol(MXOLBR+1), ibrolp(2,MXIBAD),
     1                 brolp(MXIBAD), iid, ibsol(MXCPBS+1),
     2                 ibsolp(2,MXIBAD), bsolp(MXIBAD), iie,
     3                 ibrcon(MXCNBR/2), brcon(MXCNBR/2)

        common /apout/ iib, ipbad(MXCNBR+1), ibad(MXIBAD),
     1                 bad(MXIBAD), ibad_rs(MXCNBR+1)

        common /xcntgy/ isort(MXCNBR), xsort(MXCNBR)
        integer xsort

