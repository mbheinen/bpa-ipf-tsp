
       parameter ( MXCNBR = 4*MAXBRN/10 )   ! contingency candidates
       parameter ( MXCPBS = MAXBUS )        ! contingency anal buses
       parameter ( MXOLBR = 4*MAXBRN/10 )   ! br checked for overloads
       parameter ( MXIBAD = 25*MXOLBR )

       common /alt_case/ flag, nout, vmag(MXCPBS), vangle(MXCPBS), 
     &                   novl, klno(8,MXOLBR), clno(14,MXOLBR), 
     &                   clnobase(MXOLBR), iic, ibrol(MXOLBR+1), 
     &                   ibrolp(2,MXIBAD), brolp(MXIBAD), iid, 
     &                   ibsol(MXCPBS+1), ibsolp(2,MXIBAD), 
     &                   bsolp(MXIBAD), iie, ibrcon(MXCNBR/2), 
     &                   brcon(MXCNBR/2), iib, ipbad(MXCNBR+1), 
     &                   ibad(MXIBAD), bad(MXIBAD), 
     &                   ibad_rs(MXCNBR+1), vbase(MXCPBS)

       character flag*10

       common /xcntgy/ isort(MXCNBR), xsort(MXCNBR)
       integer xsort


