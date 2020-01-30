
       parameter ( MXCNBR = 4*MAXBRN/10 )   ! contingency candidates
       parameter ( MXCPBS = MAXBUS )        ! contingency anal buses
       parameter ( MXOLBR = 4*MAXBRN/10 )   ! br checked for overloads
       parameter ( MXYBR  = 5*MXCNBR/2  )   ! reduced upper-y-matrix

       common /apin/ nbus, nout, novl, igenq, ipqv(MXCPBS),
     &               ipyu(2,MXCPBS), mfaru(MXYBR), ipyl(2,MXCPBS),
     &               mfarl(MXYBR), klnc(4,MXCNBR), klno(7,MXOLBR),
     &               iqlim(MAXTBX), itmax, vmag(MXCPBS),
     &               vangle(MXCPBS), vlow(MXCPBS), vhi(MXCPBS),
     &               ykmu(2,MXYBR), ykml(2,MXYBR), cykk(2,MXCPBS),
     &               pnet1(MXCPBS), qnet1(MXCPBS), qlow(MAXTBX),
     &               qhi(MAXTBX), shunt(MAXTBX), clnc(7,MXCNBR),
     &               clno(12,MXOLBR), cx1, cx2, cx4, ipqsln,
     &               vhold(MAXTBX), qstate(MAXTBX)
        integer qstate

