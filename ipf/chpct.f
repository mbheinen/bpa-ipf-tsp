C    @(#)chpct.f	20.5 2/13/96
      subroutine chpct
c                                       
c     process all percent change cards:  pa,po,pz,pn,pb,pc,pd 
c                                  
      include 'ipfinc/parametr.inc'

      include 'ipfinc/blank.inc'
c	Global variables used:
c		kase1, ntot
      include 'ipfinc/lfiles.inc'
c	Global variables used:
c		dbug
      include 'ipfinc/prt.inc'
c	Global variables used:
c		outbuf
      include 'ipfinc/pctger.inc'
c	Global variables used:
c		pctzow, pctge, ipctno
      include 'ipfinc/bus.inc'
c	Global variables used:
c		kbsdta, zone, owner, bus, base, busdta
      include 'ipfinc/cbus.inc'
c	Global variables used:
c		bctbl_nxt, bctbl, kbctbl

      common /is_batch / is_batch

      dimension now(MAXPCT*12), zo(MAXPCT*12), ow(MAXPCT*12),
     1          pctchg(4,MAXPCT), ipctch(2,MAXPCT)
C                                        
      character ow * 3, om * 3, zo * 2, zm * 2, cbown * 3, cbtyp * 1,
     1          pcttyp * 1, cbkyr * 2, pctxxx * 4, tmpxxx * 4, tag * 3
c
c 	Get/Put char expects an i*4.  Pctge(1,*) a real variable is 
c	being used.  Use a temporary variable to get around this error.
c
 	integer temp_char
C                           
C     INITIALIZE COUNTERS       
C                    
      if (ipctno .le. 0) go to 1260
      do 1250 ipct = 1, ipctno
         call getchr (4, pctxxx, pctge(1,ipct))
         if (pctxxx(2:4) .ne. ' ') go to 1250
         pcttyp = pctxxx(1:1)
         write (tag, 90) ipct
   90    format (i3)
C
C        Flag all similar Px records with "TAG".
C
         do 110 i = ipct, ipctno
            call getchr (4, tmpxxx, pctge(1,i))
            if (tmpxxx(1:1) .eq. pcttyp .and. tmpxxx(2:4) .eq. ' ') then
               pctxxx(2:4) = tag
               call putchr (4, pctxxx, pctge(1,i))
               ipctch(1,i) = 0
               ipctch(2,i) = 0
               do 100 j = 1, 4
  100          pctchg(j,i) = 0.0
            endif
  110    continue
 
         if (pcttyp .eq. 'A') go to 130
         if (pcttyp .eq. 'Z') go to 190
         if (pcttyp .eq. 'O') go to 890
         if (pcttyp .eq. 'N') go to 1000
         if (pcttyp .eq. 'B') go to 510
         if (pcttyp .eq. 'C') go to 320
         if (pcttyp .eq. 'D') go to 700
 
         write (errbuf(1),120) pcttyp
  120    format ('0 ILLEGAL PERCENTAGE CHANGE SUBTYPE - (', a1, ')')
         if (is_batch .eq. 0) then
            call prterx ('E',1)
         else
            call prterx ('F',1)
         endif
         go to 1160
C                   
C        PA         
C                    
  130    if (kase1(27) .ne. 0) then
            write (dbug,140)
  140       format ('0 CHANGE DEBUG '/
     1              '0 TYPE       BUS              CARD ZONE OWNER  ',
     2              '-----ORIGINAL QUANTITIES -----   -------- FINAL ',
     2              'QUANTITIES ----'/
     3              '                                               ',
     3              '----LOAD---  - GENERATION --   ---- LOAD ----  ',
     4              '- GENERATION -'/
     5              '                                                ',
     6              '(MW)  (MVAR)   (MW)   (MVAR)    (MW)   (MVAR)   ',
     7              '(MW)   (MVAR)'/)
         endif
 
         plm = pctge(3,ipct)
         qlm = pctge(4,ipct)
         pgm = pctge(5,ipct)
         qgm = pctge(6,ipct)
         do 160 i = 1, ntot
            if (kase1(27) .ne. 0) then
               plo = busdta(3,i)
               qlo = busdta(4,i)
               pgo = busdta(8,i)
               qgo = busdta(9,i)
            endif
            ntyp = kbsdta(1,i)
C                         
C           EXEMPT DC BUSES         
C                             
            if (ntyp .eq. 5 .or. ntyp .eq. 12) go to 160
            if (ntyp .eq. 1 .or. ntyp .eq. 4 .or. ntyp .eq. 6 .or.
     1          ntyp .eq. 10) then
               if (busdta(10,i) .eq. 0) then
                  pctchg(4,ipct) = pctchg(4,ipct) + busdta(9,i)
                  busdta(9,i) = busdta(9,i) * qgm
               endif
            else
               pctchg(4,ipct) = pctchg(4,ipct) + busdta(9,i)
               busdta(9,i) = busdta(9,i) * qgm
               busdta(10,i) = busdta(10,i) * qgm
            endif
            pctchg(1,ipct) = pctchg(1,ipct) + busdta(3,i)
            pctchg(2,ipct) = pctchg(2,ipct) + busdta(4,i)
            pctchg(3,ipct) = pctchg(3,ipct) + busdta(8,i)
            busdta(3,i) = busdta(3,i) * plm
            busdta(4,i) = busdta(4,i) * qlm
            busdta(8,i) = busdta(8,i) * pgm
            ipctch(1,ipct) = ipctch(1,ipct) + 1
            if (kase1(27) .ne. 0) then
               write (dbug,150) pcttyp, '  ', '  ', bus(i), base(i),
     1            zone(i), owner(i), plo, qlo, pgo, qgo, busdta(3,i),
     2            busdta(4,i), busdta(8,i), busdta(9,i)
  150          format (1x, 'P', a1, 2x, a2, 2x, a3, 1x, a8, f6.1, 2x,
     1            'B ', 3x, a2, 3x, a3, 1x, 4f8.1, 2x, 4f8.1)
            endif
  160    continue
C  
C        PROCESS CUSTOMER BUSES   
C  
         do 180 nb = 1, ntot
            i = kbsdta(15,nb)
            do while (i .gt. 0)
               if (kase1(27) .ne. 0) then
                  plo = bctbl(2,i)
                  qlo = bctbl(3,i)
                  pgo = bctbl(6,i)
                  qgo = bctbl(11,i)
               endif
C 
C              Exclude type "A" code year "  " and "00" continuation 
C              bus data (equivalent or cut system data) 
C        
               call getchr(1,cbtyp,kbctbl(8,i))
               call getchr(2,cbkyr,kbctbl(9,i))
               if ((cbkyr .eq. '*I') .or.
     1             (cbtyp .eq. 'A' .and. cbkyr .ne. '01')) go to 180
               pctchg(1,ipct) = pctchg(1,ipct) + bctbl(2,i)
               pctchg(2,ipct) = pctchg(2,ipct) + bctbl(3,i)
               pctchg(3,ipct) = pctchg(3,ipct) + bctbl(6,i)
               pctchg(4,ipct) = pctchg(4,ipct) + bctbl(11,i)
               bctbl(2,i) = bctbl(2,i) * plm
               bctbl(3,i) = bctbl(3,i) * qlm
               bctbl(6,i) = bctbl(6,i) * pgm
               bctbl(11,i) = bctbl(11,i) * qgm
               ipctch(2,ipct) = ipctch(2,ipct) + 1
               if (kase1(27) .ne. 0) then
                  k = kbctbl(1,i)
                  call getchr(3,cbown,kbctbl(10,i))
                  write (dbug,170) pcttyp, '  ', '   ', bus(k), base(k),
     1                             cbtyp, cbown, plo, qlo, pgo, qgo, 
     2                             bctbl(2,i),bctbl(3,i),bctbl(6,i),
     3                             bctbl(11,i)
  170             format (1x, 'P', a1, 2x, a2, 2x, a3, 1x, a8, f6.1, 2x,
     1                    '+ ', 3x, a2, 3x, a3, 1x, 4f8.1, 2x, 4f8.1)
               endif
               i = bctbl_nxt(i)
            enddo
  180    continue
         go to 1160
C                                     
C        PZ           PERCENT CHANGE BY ZONE/OWNER    
C                                   
  190    if (kase1(27) .ne. 0) then
            write (dbug,140)
         endif
C                                    
C        BUILD ARAYS ZO,OW FOR DIRECT CONVERSION      
C                                  
         n = 0
         do 210 i = ipct, ipctno
            call getchr (4, tmpxxx, pctge(1,i))
            if (tmpxxx(2:4) .eq. tag) then
               j = pctge(2,i)
               if (j .gt. 0) then
                  do 200 l = 1, j
                     n = n + 1
                     zo(n) = pctzow(1,i)
                     ow(n) = pctzow(l+1,i)
                     now(n) = i
  200             continue
               else
                  n = n + 1
                  zo(n) = pctzow(1,i)
                  ow(n) = '000'
                  now(n) = i
               endif
            endif
  210    continue
         nzow = n
C                                  
C        NOW PROCESS ALL BUS/CUST BUS 
C                     
         do 310 i = 1, ntot
            ntyp = kbsdta(1,i)
            if (ntyp .eq. 5 .or. ntyp .eq. 12) go to 310
            zm = zone(i)
            om = owner(i)
            do 220 l = 1, nzow
               if (zm .eq. zo(l)) go to 230
  220       continue
            go to 310

  230       continue
            do 240 l = 1, nzow
               if (zm .eq. zo(l) .and.
     1            (om .eq. ow(l) .or. ow(l) .eq. '000')) go to 250
  240       continue
            go to 260
  250       continue
            j = now(l)
            if (kase1(27) .ne. 0) then
               plo = busdta(3,i)
               qlo = busdta(4,i)
               pgo = busdta(8,i)
               qgo = busdta(9,i)
            endif
            if (ntyp .eq. 1 .or. ntyp .eq. 4 .or. ntyp .eq. 6 .or.
     1          ntyp .eq. 10) then
               if (busdta(10,i) .eq. 0) then
                  pctchg(4,j) = pctchg(4,j) + busdta(9,i)
                  busdta(9,i) = busdta(9,i) * pctge(6,j)
               endif
            else
               pctchg(4,j) = pctchg(4,j) + busdta(9,i)
               busdta(9,i) = busdta(9,i) * pctge(6,j)
               busdta(10,i) = busdta(10,i) * pctge(6,j)
            endif
            pctchg(1,j) = pctchg(1,j) + busdta(3,i)
            pctchg(2,j) = pctchg(2,j) + busdta(4,i)
            pctchg(3,j) = pctchg(3,j) + busdta(8,i)
            busdta(3,i) = busdta(3,i) * pctge(3,j)
            busdta(4,i) = busdta(4,i) * pctge(4,j)
            busdta(8,i) = busdta(8,i) * pctge(5,j)
            ipctch(1,j) = ipctch(1,j) + 1
            if (kase1(27) .ne. 0) then
               write (dbug,150) pcttyp, '  ', '  ', bus(i), base(i),
     1            zone(i), owner(i), plo, qlo, pgo, qgo, busdta(3,i),
     2            busdta(4,i), busdta(8,i), busdta(9,i)
            endif
  260       continue
C           
C           WE HAVE ZONE MATCH ON BUS DO CUST BUSES    
C           
            icb = kbsdta(15,i)
            do while (icb .gt. 0)
C                                 
C              Exclude type "A" code year "  " and "00" continuation 
C              bus data (equivalent or cut system data) 
C        
               call getchr(1,cbtyp,kbctbl(8,icb))
               call getchr(2,cbkyr,kbctbl(9,icb))
               call getchr(3,cbown,kbctbl(10,icb))
               if ((cbkyr .eq. '*I') .or.
     1             (cbtyp .eq. 'A' .and. cbkyr .ne. '01')) go to 300
               do 280 l = 1, nzow
                  if (zm .ne. zo(l)) go to 280
                  if (cbown .eq. ow(l) .or. ow(l) .eq. '000') go to 290
  280          continue
               go to 300
C           
C           FOUND CUS BUS MATCH   
C           
  290          j = now(l)
               if (kase1(27) .ne. 0) then
                  plo = bctbl(2,icb)
                  qlo = bctbl(3,icb)
                  pgo = bctbl(6,icb)
                  qgo = bctbl(11,icb)
               endif
               pctchg(1,j) = pctchg(1,j) + bctbl(2,icb)
               pctchg(2,j) = pctchg(2,j) + bctbl(3,icb)
               pctchg(3,j) = pctchg(3,j) + bctbl(6,icb)
               pctchg(4,j) = pctchg(4,j) + bctbl(11,icb)
               bctbl(2,icb) = bctbl(2,icb) * pctge(3,j)
               bctbl(3,icb) = bctbl(3,icb) * pctge(4,j)
               bctbl(6,icb) = bctbl(6,icb) * pctge(5,j)
               bctbl(11,icb) = bctbl(11,icb) * pctge(6,j)
               ipctch(2,j) = ipctch(2,j) + 1
               if (kase1(27) .ne. 0) then
                  k = kbctbl(1,icb)
                  call getchr(3,cbown,kbctbl(10,icb))
                  write (dbug,170) pcttyp, '  ', '   ', bus(k), base(k),
     1                             cbtyp,cbown,plo,qlo,pgo,qgo,
     2                             bctbl(2,icb),bctbl(3,icb),
     3                             bctbl(6,icb),bctbl(11,icb)
               endif
  300          icb = bctbl_nxt(icb)
            enddo
  310    continue
         go to 1160
C                         
C        PC           Percent change by zone/owner         
C                            
  320    if (kase1(27) .ne. 0) then
            write (dbug,330 )
  330       format ('0 CHANGE DEBUG '/
     1              '0TYPE ZONE BUS          CARD OWNER CODE  ----- ',
     2              'ORIGINAL QUANTITIES -----   -------- FINAL ',
     2              ',QUANTITIES ----'/
     3              '                                         - ',
     3              'CONSTANT I -- CONSTANT Z --   - CONSTANT I -  ',
     4              '- CONSTANT Z -'/
     5              '                                          (MW)  ',
     5              '(MVAR) (MW)   (MVAR)    (MW)   (MVAR)   (MW)   ',
     6              '(MVAR)'/)
         endif
C  
C        Build arays ZO,OW for direct conversion   
C  
         n = 0
         do 350 i = ipct, ipctno
            call getchr (4, tmpxxx, pctge(1,i))
            if (tmpxxx(2:4) .eq. tag) then
               j = pctge(2,i)
               if (j .gt. 0) then
                  do 340 l = 1, j
                     n = n + 1
                     zo(n) = pctzow(1,i)
                     ow(n) = pctzow(l+1,i)
                     now(n) = i
  340             continue
               else
                  n = n + 1
                  zo(n) = pctzow(1,i)
                  ow(n) = '000'
                  now(n) = i
               endif
            endif
  350    continue
         nzow = n
C                            
C        Now process all + continuation bus records.  
C                   
         do 430 nb = 1, ntot
            ntyp = kbsdta(1,nb)
            if (ntyp .eq. 5 .or. ntyp .eq. 12) go to 430
            zm = zone(nb)
            do 360 l = 1, nzow
               if (zm .eq. zo(l)) go to 370
  360       continue
            go to 430
  370       continue
C                          
C           We have zone match on bus. Do + continuation buses  
C                        
            icb = kbsdta(15,nb)
            do while (icb .gt. 0)
C                              
C              Only +A01 and +A02 records are eligible.   
C          
               call getchr(1,cbtyp,kbctbl(8,icb))
               call getchr(2,cbkyr,kbctbl(9,icb))
               call getchr(3,cbown,kbctbl(10,icb))
               if ((cbkyr .eq. '*I' .or. cbkyr .eq. '*P') .or.
     1             (cbtyp .eq. 'A' .and.
     2             (cbkyr .eq. '01' .or. cbkyr .eq. '02'))) then
                  do 390 l = 1, nzow
                     if (zm .ne. zo(l)) go to 390
                     if (cbown .eq. ow(l) .or. ow(l) .eq. '000')
     1                  go to 400
  390             continue
                  go to 420
C              
C                 Matched + continuation bus.       
C                        
  400             j = now(l)
                  if (kase1(27) .ne. 0) then
                     pio = bctbl(2,icb)
                     qio = bctbl(3,icb)
                     pzo = bctbl(4,icb)
                     qzo = bctbl(5,icb)
                  endif
C         
C                 Constant current appears only on +A01 records.
C       
                  if ((cbkyr .eq. '*I') .or.
     1                (cbtyp .eq. 'A' .and. cbkyr .eq. '01')) then
                     pctchg(1,j) = pctchg(1,j) + bctbl(2,icb)
                     pctchg(2,j) = pctchg(2,j) + bctbl(3,icb)
                     bctbl(2,icb) = bctbl(2,icb) * pctge(3,j)
                     bctbl(3,icb) = bctbl(3,icb) * pctge(4,j)
                  endif
                  pctchg(3,j) = pctchg(3,j) + bctbl(4,icb)
                  pctchg(4,j) = pctchg(4,j) + bctbl(5,icb)
                  bctbl(4,icb) = bctbl(4,icb) * pctge(5,j)
                  bctbl(5,icb) = bctbl(5,icb) * pctge(6,j)
                  ipctch(2,j) = ipctch(2,j) + 1
                  if (kase1(27) .ne. 0) then
                     write (dbug,410 ) pcttyp, zone(nb), bus(nb),
     1                  base(nb), cbtyp, cbown, cbkyr, pio, qio, pzo,
     2                  qzo, bctbl(2,icb), bctbl(3,icb), bctbl(4,icb),
     3                  bctbl(5,icb)
  410                format (1x, 'P', a1, 2x, a2, 2x, a8, f6.1, 2x, '+'
     1                  , a1, 3x, a2, 2x, a2, 1x, 4f8.1, 2x, 4f8.1)
                  endif
               endif
  420          continue
               icb = bctbl_nxt(icb)
            enddo
  430    continue
         na = 0
         do 440 i = ipct, ipctno
            call getchr (4, tmpxxx, pctge(1,i))
            if (tmpxxx(2:4) .eq. tag) then
               na = na + ipctch(1,i) + ipctch(2,i)
            endif
  440    continue
         write (outbuf,450 ) pctge(1,ipct)
  450    format ('0     SUMMARY OF PERCENTAGE CHANGES TYPE = "P', a1,
     1           '"')
         call prtout (1)
         write (outbuf,460 )
  460    format (
     1           '0 CATEGORY   NO. OF CHANGES   ----------- ORIGINAL ',
     2           'QUANTITIES ----------    ------------ FINAL ',
     3           'QUANTITES --------------')
         call prtout(1)
         write (outbuf,470 )
  470    format (
     1           '             BUSES  CUST/BUS  -- CONSTANT I LOAD - ',
     2           '-- CONSTANT Z LOAD --    -- CONSTANT I LOAD -  -- ',
     3           'CONSTANT Z LOAD --')
         call prtout (1)
         write (outbuf,480 )
  480    format (
     1           '                                 (MW)       ',
     2           '(MVAR)    (MW)      (MVAR)       (MW)       (',
     3           'MVAR)      (MW)        (MVAR)')
         call prtout (1)
         call space (1)
         do 500 i = ipct, ipctno
            call getchr (4, tmpxxx, pctge(1,i))
            if (tmpxxx(2:4) .eq. tag) then
               pim = pctge(3,i) * pctchg(1,i)
               qim = pctge(4,i) * pctchg(2,i)
               pzm = pctge(5,i) * pctchg(3,i)
               qzm = pctge(6,i) * pctchg(4,i)
               write (outbuf,490 ) pctzow(1,i), (ipctch(j,i),j=1,2)
     1                         ,(pctchg(j,i),j=1,4), pim, qim, pzm, qzm
  490          format (4x, a3, 7x, i4, 6x, i4, 3x, 2f9.1, 3x, 2f9.1, 3x
     1                 , 2f9.1, 3x, 2f9.1)
               call prtout (1)
            endif
  500    continue
         go to 1240
C             
C        PB           Percent change by owner/zone    
C             
  510    if (kase1(27) .ne. 0) then
            write (dbug,520 )
  520       format ('0 CHANGE DEBUG '/
     1              '0TYPE  ZONE  BUS        CARD OWNER CODE  -----',
     2              'ORIGINAL QUANTITIES -----   -------- FINAL ',
     3              'QUANTITIES ---'/
     4              '                                         - ',
     5              'CONSTANT I- - CONSTANT Z --   - CONSTANT I -  - ',
     6              'CONSTANT Z -'/
     7              '                                          (MW)  (',
     8              'MVAR) (MW)   (MVAR)    (MW)   (MVAR)   (MW)   (',
     9              'MVAR)'/)
         endif
C 
C        Build arays OW, ZO for direct conversion  
C 
         n = 0
         do 540 i = ipct, ipctno
            call getchr (4, tmpxxx, pctge(1,i))
            if (tmpxxx(2:4) .eq. tag) then
               j = pctge(2,i)
               if (j .gt. 0) then
                  do 530 l = 1, j
                     n = n + 1
                     ow(n) = pctzow(1,i)
                     zo(n) = pctzow(l+1,i)
                     now(n) = i
  530             continue
               else
                  n = n + 1
                  ow(n) = pctzow(1,i)
                  zo(n) = '00'
                  now(n) = i
               endif
            endif
  540    continue
         nzow = n
C                    
C        Now process all + continuation bus records which reside in 
C        optional zones.  
C 
         do 620 nb = 1, ntot
            ntyp = kbsdta(1,nb)
            if (ntyp .eq. 5 .or. ntyp .eq. 12) go to 620
            zm = zone(nb)
            om = owner(nb)
C 
C           Use only zone. Skip ownership test on bus because only 
C           + records are of interest.   
C 
            do 550 l = 1, nzow
               if (zm .eq. zo(l) .or. zo(l) .eq. '00') go to 560
  550       continue
            go to 620

  560       continue
C                   
C           We have owner/zone match on bus. Do + continuation buses 
C                 
            icb = kbsdta(15,nb)
            do while (icb .gt. 0)
C                 
C              Only +A01 and +A02 records are eligible. 
C      
               call getchr(1,cbtyp,kbctbl(8,icb))
               call getchr(2,cbkyr,kbctbl(9,icb))
               call getchr(3,cbown,kbctbl(10,icb))
               if ((cbkyr .eq. '*I' .or. cbtyp .eq. '*P') .or.
     1             (cbtyp .eq. 'A' .and.
     2             (cbkyr .eq. '01' .or. cbkyr .eq. '02'))) then
                  do 580 l = 1, nzow
                     if (cbown .ne. ow(l)) go to 580
                     if (zo(l) .eq. zm .or. zo(l) .eq. '00') go to 590
  580             continue
                  go to 610
C           
C                 Matched + continuation bus.  
C                     
  590             j = now(l)
                  if (kase1(27) .ne. 0) then
                     pio = bctbl(2,icb)
                     qio = bctbl(3,icb)
                     pzo = bctbl(4,icb)
                     qzo = bctbl(5,icb)
                  endif
C                     
C                 Constant current appears only on +A01 records.
C          
                  if ((cbkyr .eq. '*I') .or. (cbtyp .eq. 'A' .and.
     1               cbkyr .eq. '01')) then
                     pctchg(1,j) = pctchg(1,j) + bctbl(2,icb)
                     pctchg(2,j) = pctchg(2,j) + bctbl(3,icb)
                     bctbl(2,icb) = bctbl(2,icb) * pctge(3,j)
                     bctbl(3,icb) = bctbl(3,icb) * pctge(4,j)
                  endif
                  pctchg(3,j) = pctchg(3,j) + bctbl(4,icb)
                  pctchg(4,j) = pctchg(4,j) + bctbl(5,icb)
                  bctbl(4,icb) = bctbl(4,icb) * pctge(5,j)
                  bctbl(5,icb) = bctbl(5,icb) * pctge(6,j)
                  ipctch(2,j) = ipctch(2,j) + 1
                  if (kase1(27) .ne. 0) then
                     write (dbug,600 ) pcttyp, zone(nb), bus(nb),
     1                  base(nb), cbtyp, cbown, cbkyr, pio, qio, pzo,
     2                  qzo, bctbl(2,icb), bctbl(3,icb), bctbl(4,icb),
     3                  bctbl(5,icb)
  600                format (1x, 'P', a1, 2x, a2, 2x, a8, f6.1, 2x, '+'
     1                  , a1, 3x, a3, 2x, a2, 1x, 4f8.1, 2x, 4f8.1)
                  endif
               endif
  610          continue
               icb = bctbl_nxt(icb)
            enddo
  620    continue
         na = 0
         do 630 i = ipct, ipctno
            call getchr (4, tmpxxx, pctge(1,i))
            if (tmpxxx(2:4) .eq. tag) then
               na = na + ipctch(1,i) + ipctch(2,i)
            endif
  630    continue
         write (outbuf,640 ) pctge(1,ipct)
  640    format ('0     SUMMARY OF PERCENTAGE CHANGES TYPE = "P', a1,
     1           '"')
         call prtout (1)
         write (outbuf,650 )
  650    format ('0 CATEGORY   NO. OF CHANGES   ----------- ',
     1           'ORIGINAL QUANTITIES ----------    ------------ ',
     2           'FINAL QUANTITES --------------')
         call prtout(1)
         write (outbuf,660 )
  660    format ('             BUSES  CUST/BUS  -- CONSTANT I LOAD - ',
     1           '-- CONSTANT Z LOAD --    -- CONSTANT I LOAD -  -- ',
     2           'CONSTANT Z LOAD --')
         call prtout (1)
         write (outbuf,670 )
  670    format ('                                 (MW)        (',
     1           'MVAR)  (MW)     (MVAR)       (MW)       (MVAR',
     2           ')      (MW)        (MVAR')
         call prtout (1)
         call space (1)
         do 690 i = ipct, ipctno
            call getchr (4, tmpxxx, pctge(1,i))
            if (tmpxxx(2:4) .eq. tag) then
               pim = pctge(3,i) * pctchg(1,i)
               qim = pctge(4,i) * pctchg(2,i)
               pzm = pctge(5,i) * pctchg(3,i)
               qzm = pctge(6,i) * pctchg(4,i)
               write (outbuf,680 ) pctzow(1,i), (ipctch(j,i),j=1,2)
     1                        ,(pctchg(j,i),j=1,4), pim, qim, pzm, qzm
  680          format (4x, a3, 7x, i4, 6x, i4, 1x, f9.1, 4x, f9.1, 1x,
     1                 f9.1, 4x, f9.1, 1x, f9.1, 4x, f9.1, 1x, f9.1, 4x,
     2                 f9.1)
               call prtout (1)
            endif
  690    continue
         go to 1240
C                 
C        PD      Percent change of non-industrial loads by zone/owner
C                                   
  700    if (kase1(27) .ne. 0) then
            write (dbug,710 )
  710       format ('0 CHANGE DEBUG '/
     1              '0TYPE ZONE BUS          CARD OWNER CODE  ----- ',
     1              'ORIGINAL QUANTITIES -----   -------- ',
     2              'FINAL QUANTITIES ----'/
     3              '                                         - ',
     4              'CONSTANT I -- CONSTANT Z --   - CONSTANT I -  - ',
     5              'CONSTANT Z -'/
     6              '                                          (MW)  (',
     7              'MVAR) (MW)   (MVAR)    (MW)   (MVAR)   (MW)   (',
     8              'MVAR)'/)
         endif
C 
C        Build arays ZO,OW for direct conversion
C 
         n = 0
         do 730 i = ipct, ipctno
            call getchr (4, tmpxxx, pctge(1,i))
            if (tmpxxx(2:4) .eq. tag) then
               j = pctge(2,i)
               if (j .gt. 0) then
                  do 720 l = 1, j
                     n = n + 1
                     zo(n) = pctzow(1,i)
                     ow(n) = pctzow(l+1,i)
                     now(n) = i
  720             continue
               else
                  n = n + 1
                  zo(n) = pctzow(1,i)
                  ow(n) = '000'
                  now(n) = i
               endif
            endif
  730    continue
         nzow = n
C 
C        Now process all + continuation bus records.
C 
         do 810 nb = 1, ntot
            ntyp = kbsdta(1,nb)
            if (ntyp .eq. 5 .or. ntyp .eq. 12) go to 810
            zm = zone(nb)
            do 740 l = 1, nzow
               if (zm .eq. zo(l)) go to 750
  740       continue
            go to 810
  750       continue
C                      
C           We have zone match on bus. Do + continuation buses 
C          
            icb = kbsdta(15,nb)
            do while (icb .gt. 0)
C             
C              Only +A*I, +N*I, +A*P, and +N*P records are eligible.
C          
               call getchr(1,cbtyp,kbctbl(8,icb))
               call getchr(2,cbkyr,kbctbl(9,icb))
               call getchr(3,cbown,kbctbl(10,icb))
               if ((cbtyp .eq. 'N' .or. cbtyp .eq. 'A') .and.
     1             (cbkyr .eq. '*I' .or. cbkyr .eq. '*P')) then
                  do 770 l = 1, nzow
                     if (zm .ne. zo(l)) go to 770
                     if (cbown .eq. ow(l) .or. ow(l) .eq. '000')
     1                  go to 780
  770             continue
                  go to 800
C              
C                 Matched + continuation bus.  
C           
  780             j = now(l)
                  if (kase1(27) .ne. 0) then
                     pio = bctbl(2,icb)
                     qio = bctbl(3,icb)
                     pzo = bctbl(4,icb)
                     qzo = bctbl(5,icb)
                  endif
C           
C                 Constant current appears on +A*I and +N*I records. 
C      
                  if (cbkyr .eq. '*I') then
                     pctchg(1,j) = pctchg(1,j) + bctbl(2,icb)
                     pctchg(2,j) = pctchg(2,j) + bctbl(3,icb)
                     bctbl(2,icb) = bctbl(2,icb) * pctge(3,j)
                     bctbl(3,icb) = bctbl(3,icb) * pctge(4,j)
                  endif
                  pctchg(3,j) = pctchg(3,j) + bctbl(4,icb)
                  pctchg(4,j) = pctchg(4,j) + bctbl(5,icb)
                  bctbl(4,icb) = bctbl(4,icb) * pctge(5,j)
                  bctbl(5,icb) = bctbl(5,icb) * pctge(6,j)
                  ipctch(2,j) = ipctch(2,j) + 1
                  if (kase1(27) .ne. 0) then
                     write (dbug,790 ) pcttyp, zone(nb), bus(nb),
     1                  base(nb), cbtyp, cbown, cbkyr, pio, qio, pzo,
     2                  qzo, bctbl(2,icb), bctbl(3,icb), bctbl(4,icb),
     3                  bctbl(5,icb)
  790                format (1x, 'P', a1, 2x, a2, 2x, a8, f6.1, 2x, '+'
     1                  , a1, 3x, a2, 2x, a2, 1x, 4f8.1, 2x, 4f8.1)
                  endif
               endif
  800          continue
               icb = bctbl_nxt(icb)
            enddo
  810    continue
         na = 0
         do 820 i = ipct, ipctno
            call getchr (4, tmpxxx, pctge(1,i))
            if (tmpxxx(2:4) .eq. tag) then
               na = na + ipctch(1,i) + ipctch(2,i)
            endif
  820    continue
         write (outbuf,830 ) pctge(1,ipct)
  830    format ('0     SUMMARY OF PERCENTAGE CHANGES TYPE = "P',a1,'"')
         call prtout (1)
         write (outbuf,840 )
  840    format ('0 CATEGORY   NO. OF CHANGES   ----------- ',
     1           'ORIGINAL QUANTITIES ----------    ------------ ',
     2           'FINAL QUANTITES --------------')
         call prtout(1)
         write (outbuf,850 )
  850    format ('             BUSES  CUST/BUS  -- CONSTANT I LOAD - ',
     1           '-- CONSTANT Z LOAD --    -- CONSTANT I LOAD -  ',
     2           '-- CONSTANT Z LOAD --')
         call prtout (1)
         write (outbuf,860 )
  860    format ('                                 (MW)       (',
     1           'MVAR)    (MW)      (MVAR)       (MW)       (',
     2           'MVAR)      (MW)        (MVAR)')
         call prtout (1)
         call space (1)
         do 880 i = ipct, ipctno
            call getchr (4, tmpxxx, pctge(1,i))
            if (tmpxxx(2:4) .eq. tag) then
               pim = pctge(3,i) * pctchg(1,i)
               qim = pctge(4,i) * pctchg(2,i)
               pzm = pctge(5,i) * pctchg(3,i)
               qzm = pctge(6,i) * pctchg(4,i)
               write (outbuf,870 ) pctzow(1,i), (ipctch(j,i),j=1,2)
     1            ,(pctchg(j,i),j=1,4), pim, qim, pzm, qzm
  870          format (4x, a3, 7x, i4, 6x, i4, 3x, 2f9.1, 3x, 2f9.1, 3x
     1            , 2f9.1, 3x, 2f9.1)
               call prtout (1)
            endif
  880    continue
         go to 1240
C                   
C        PERCENT CHANGE BY OWNER/ZONE 
C          
  890    if (kase1(27) .ne. 0) then
            write (dbug,140)
         endif
C           
C        BUILD ARAYS OW,ZO FOR DIRECT CONVERSION   
C     
         n = 0
         do 910 i = ipct, ipctno
            call getchr (4, tmpxxx, pctge(1,i))
            if (tmpxxx(2:4) .eq. tag) then
               j = pctge(2,i)
               if (j .gt. 0) then
                  do 900 l = 1, j
                     n = n + 1
                     ow(n) = pctzow(1,i)
                     zo(n) = pctzow(l+1,i)
                     now(n) = i
  900             continue
               else
                  n = n + 1
                  ow(n) = pctzow(1,i)
                  zo(n) = '00'
                  now(n) = i
               endif
            endif
  910    continue
         nzow = n
C      
C        NOW PROCESS ALL BUS/CUST Bus
C         
         do 990 i = 1, ntot
            ntyp = kbsdta(1,i)
C     
C           Skip DC Busses     
C      
            if (ntyp .eq. 5 .or. ntyp .eq. 12) go to 990
            zm = zone(i)
            om = owner(i)
            do 920 l = 1, nzow
               if (om .eq. ow(l) .and.
     1            (zm .eq. zo(l) .or. zo(l) .eq. '00')) go to 930
  920       continue
            go to 940
  930       continue
            j = now(l)
            if (kase1(27) .ne. 0) then
               plo = busdta(3,i)
               qlo = busdta(4,i)
               pgo = busdta(8,i)
               qgo = busdta(9,i)
            endif
            if (ntyp .eq. 1 .or. ntyp .eq. 4 .or. ntyp .eq. 6 .or.
     1         ntyp .eq. 10) then
               if (busdta(10,i) .eq. 0) then
                  pctchg(4,j) = pctchg(4,j) + busdta(9,i)
                  busdta(9,i) = busdta(9,i) * pctge(6,j)
               endif
            else
               pctchg(4,j) = pctchg(4,j) + busdta(9,i)
               busdta(9,i) = busdta(9,i) * pctge(6,j)
               busdta(10,i) = busdta(10,i) * pctge(6,j)
            endif
            pctchg(1,j) = pctchg(1,j) + busdta(3,i)
            pctchg(2,j) = pctchg(2,j) + busdta(4,i)
            pctchg(3,j) = pctchg(3,j) + busdta(8,i)
            busdta(3,i) = busdta(3,i) * pctge(3,j)
            busdta(4,i) = busdta(4,i) * pctge(4,j)
            busdta(8,i) = busdta(8,i) * pctge(5,j)
            ipctch(1,j) = ipctch(1,j) + 1
            if (kase1(27) .ne. 0) then
               write (dbug,150) pcttyp, '  ', '  ', bus(i), base(i),
     1           zone(i), owner(i), plo, qlo, pgo, qgo, busdta(3,i),
     2           busdta(4,i), busdta(8,i), busdta(9,i)
            endif
  940       continue
C        
C           DO CUST BUSES   
C     
            icb = kbsdta(15,i)
            do while (icb .gt. 0)
C                
C           Exclude type "A" code year "  " and "00" continuation bus 
C           data (equivalent or cut system data)   
C     
            call getchr(1,cbtyp,kbctbl(8,icb))
            call getchr(2,cbkyr,kbctbl(9,icb))
            call getchr(3,cbown,kbctbl(10,icb))
            if ((cbkyr .eq. '*I') .or.
     1          (cbtyp .eq. 'A' .and. cbkyr .ne. '01')) go to 980
            do 960 l = 1, nzow
               if (cbown .ne. ow(l)) go to 960
               if (zo(l) .eq. zm .or. zo(l) .eq. '00') go to 970
  960       continue
            go to 980
C   
C           FOUND CUS BUS MATCH
C     
  970       j = now(l)
            if (kase1(27) .ne. 0) then
               plo = bctbl(2,icb)
               qlo = bctbl(3,icb)
               pgo = bctbl(6,icb)
               qgo = bctbl(11,icb)
            endif
            pctchg(1,j) = pctchg(1,j) + bctbl(2,icb)
            pctchg(2,j) = pctchg(2,j) + bctbl(3,icb)
            pctchg(3,j) = pctchg(3,j) + bctbl(6,icb)
            pctchg(4,j) = pctchg(4,j) + bctbl(11,icb)
            bctbl(2,icb) = bctbl(2,icb) * pctge(3,j)
            bctbl(3,icb) = bctbl(3,icb) * pctge(4,j)
            bctbl(6,icb) = bctbl(6,icb) * pctge(5,j)
            bctbl(11,icb) = bctbl(11,icb) * pctge(6,j)
            ipctch(2,j) = ipctch(2,j) + 1
            if (kase1(27) .ne. 0) then
               k = kbctbl(1,icb)
               call getchr(3,cbown,kbctbl(10,icb))
               write (dbug,170) pcttyp, '  ', '   ', bus(k), base(k),
     1            cbtyp, cbown, plo, qlo, pgo, qgo, bctbl(2,icb),
     2            bctbl(3,icb), bctbl(6,icb), bctbl(11,icb)
            endif
  980       continue
            icb = bctbl_nxt(icb)
            enddo
  990    continue
         go to 1160
C         
C        PN    NON-INDUSTRIAL PERCENT CHANGE BY ZONE  
C                EXCLUDES CUST BUSES +A +F +I +P
C       
 1000    if (kase1(27) .ne. 0) then
            write (dbug,140)
         endif
 
         if (pctge(2,ipct) .eq. 0 .and. pctzow(1,ipct) .eq. '00')
     1      go to 1130
C        
C        BUILD ARAYS ZO,OW FOR DIRECT CONVERSION  
C        
         n = 0
         do 1020 i = ipct, ipctno
            call getchr (4, tmpxxx, pctge(1,i))
            if (tmpxxx(2:4) .eq. tag) then
               j = pctge(2,i)
               if (j .gt. 0) then
                  do 1010 l = 1, j
                     n = n + 1
                     zo(n) = pctzow(1,i)
                     ow(n) = pctzow(l+1,i)
                     now(n) = i
 1010             continue
               else
                  n = n + 1
                  zo(n) = pctzow(1,i)
                  ow(n) = '000'
                  now(n) = i
               endif
            endif
 1020    continue
         nzow = n
 
         do 1120 i = 1, ntot
            ntyp = kbsdta(1,i)
C            
C           Skip DC Buses   
C         
            if (ntyp .eq. 5 .or. ntyp .eq. 12) go to 1120
            zm = zone(i)
            om = owner(i)
            do 1030 l = 1, nzow
               if (zm .eq. zo(l)) go to 1040
 1030       continue
            go to 1120
 1040       do 1050 l = 1, nzow
               if (zm .eq. zo(l) .and.
     1            (om .eq. ow(l) .or. ow(l) .eq. '000')) go to 1060
 1050       continue
            go to 1070
 1060       j = now(l)
 
            if (kase1(27) .ne. 0) then
               plo = busdta(3,i)
               qlo = busdta(4,i)
               pgo = busdta(8,i)
               qgo = busdta(9,i)
            endif
            if (ntyp .eq. 1 .or. ntyp .eq. 4 .or. ntyp .eq. 6 .or.
     1          ntyp .eq. 10) then
               if (busdta(10,i) .eq. 0) then
                  pctchg(4,j) = pctchg(4,j) + busdta(9,i)
                  busdta(9,i) = busdta(9,i) * pctge(6,j)
               endif
            else
               pctchg(4,j) = pctchg(4,j) + busdta(9,i)
               busdta(9,i) = busdta(9,i) * pctge(6,j)
               busdta(10,i) = busdta(10,i) * pctge(6,j)
            endif
            pctchg(1,j) = pctchg(1,j) + busdta(3,i)
            pctchg(2,j) = pctchg(2,j) + busdta(4,i)
            pctchg(3,j) = pctchg(3,j) + busdta(8,i)
            busdta(3,i) = busdta(3,i) * pctge(3,j)
            busdta(4,i) = busdta(4,i) * pctge(4,j)
            busdta(8,i) = busdta(8,i) * pctge(5,j)
            ipctch(1,j) = ipctch(1,j) + 1
            if (kase1(27) .ne. 0) then
               write (dbug,150) pcttyp, '  ', '  ', bus(i), base(i),
     1            zone(i), owner(i), plo, qlo, pgo, qgo, busdta(3,i),
     2            busdta(4,i), busdta(8,i), busdta(9,i)
            endif
 1070       continue
C             
C           WE HAVE ZONE MATCH ON BUS DO CUST BUSES   
C     
            icb = kbsdta(15,i)
            do while (icb .gt. 0)
            call getchr(1,cbtyp,kbctbl(8,icb))
            call getchr(2,cbkyr,kbctbl(9,icb))
            call getchr(3,cbown,kbctbl(10,icb))
            if (cbtyp .ne. 'N') go to 1110
            do 1090 l = 1, nzow
               if (zm .ne. zo(l)) go to 1090
               if (cbown .eq. ow(l) .or. ow(l) .eq. '000') go to 1100
 1090       continue
            go to 1110
 1100       continue
C         
C           FOUND CUS BUS MATCH    
C      
            if (kase1(27) .ne. 0) then
               plo = bctbl(2,icb)
               qlo = bctbl(3,icb)
               pgo = bctbl(6,icb)
               qgo = bctbl(11,icb)
            endif
            pctchg(1,j) = pctchg(1,j) + bctbl(2,icb)
            pctchg(2,j) = pctchg(2,j) + bctbl(3,icb)
            pctchg(3,j) = pctchg(3,j) + bctbl(6,icb)
            pctchg(4,j) = pctchg(4,j) + bctbl(11,icb)
            bctbl(2,icb) = bctbl(2,icb) * pctge(3,j)
            bctbl(3,icb) = bctbl(3,icb) * pctge(4,j)
            bctbl(6,icb) = bctbl(6,icb) * pctge(5,j)
            bctbl(11,icb) = bctbl(11,icb) * pctge(6,j)
            ipctch(2,j) = ipctch(2,j) + 1
            if (kase1(27) .ne. 0) then
               k = kbctbl(1,icb)
               call getchr(3,cbown,kbctbl(10,icb))
               write (dbug,170) pcttyp, '  ', '   ', bus(k), base(k),
     1            cbtyp, cbown, plo, qlo, pgo, qgo, bctbl(2,icb),
     2            bctbl(3,icb), bctbl(6,icb), bctbl(11,icb)
            endif
 1110       continue
            icb = bctbl_nxt(icb)
            enddo
 1120    continue
         go to 1160
C   
C        PROCESS ALL NON-INDUSTRIAL BUSES AND CONTINUATION BUSES 
C        
 1130    plm = pctge(3,ipct)
         qlm = pctge(4,ipct)
         pgm = pctge(5,ipct)
         qgm = pctge(6,ipct)
         do 1140 i = 1, ntot
            if (kase1(27) .ne. 0) then
               plo = busdta(3,i)
               qlo = busdta(4,i)
               pgo = busdta(8,i)
               qgo = busdta(9,i)
            endif
            ntyp = kbsdta(1,i)
C                 
C           Skip DC Buses  
C         
            if (ntyp .eq. 5 .or. ntyp .eq. 12) go to 1140
            if (ntyp .eq. 1 .or. ntyp .eq. 4 .or. ntyp .eq. 6 .or.
     1          ntyp .eq. 10) then
               if (busdta(10,i) .eq. 0) then
                  pctchg(4,ipct) = pctchg(4,ipct) + busdta(9,i)
                  busdta(9,i) = busdta(9,i) * qgm
               endif
            else
               pctchg(4,ipct) = pctchg(4,ipct) + busdta(9,i)
               busdta(9,i) = busdta(9,i) * qgm
               busdta(10,i) = busdta(10,i) * qgm
            endif
            pctchg(1,ipct) = pctchg(1,ipct) + busdta(3,i)
            pctchg(2,ipct) = pctchg(2,ipct) + busdta(4,i)
            pctchg(3,ipct) = pctchg(3,ipct) + busdta(8,i)
            busdta(3,i) = busdta(3,i) * plm
            busdta(4,i) = busdta(4,i) * qlm
            busdta(8,i) = busdta(8,i) * pgm
            ipctch(1,ipct) = ipctch(1,ipct) + 1
            if (kase1(27) .ne. 0) then
               write (dbug,150) pcttyp, '  ', '  ', bus(i), base(i),
     1            zone(i), owner(i), plo, qlo, pgo, qgo, busdta(3,i),
     2            busdta(4,i), busdta(8,i), busdta(9,i)
            endif
 1140    continue
C         
C        PROCESS CUSTOMER BUSES 
C   
         if (ntot2 .eq. 0) go to 1160
         do 1150 nb = 1, ntot
            i = kbsdta(15,nb)
            do while (i .gt. 0)

            call getchr(1,cbtyp,bctbl(8,i))
            if (index('AIFP',cbtyp) .ne. 0) go to 1150
C               
C           NON-INDUSTRIAL ONLY  
C     
            if (kase1(27) .ne. 0) then
               plo = bctbl(2,i)
               qlo = bctbl(3,i)
               pgo = bctbl(6,i)
               qgo = bctbl(11,i)
            endif
            pctchg(1,ipct) = pctchg(1,ipct) + bctbl(2,i)
            pctchg(2,ipct) = pctchg(2,ipct) + bctbl(3,i)
            pctchg(3,ipct) = pctchg(3,ipct) + bctbl(6,i)
            pctchg(4,ipct) = pctchg(4,ipct) + bctbl(11,i)
            bctbl(2,i) = bctbl(2,i) * plm
            bctbl(3,i) = bctbl(3,i) * qlm
            bctbl(6,i) = bctbl(6,i) * pgm
            bctbl(11,i) = bctbl(11,i) * qgm
            ipctch(2,ipct) = ipctch(2,ipct) + 1
            if (kase1(27) .ne. 0) then
               k = kbctbl(1,i)
               call getchr(3,cbown,kbctbl(10,i))
               write (dbug,170) pcttyp, '  ', '   ', bus(k), base(k),
     1            cbtyp, cbown, plo, qlo, pgo, qgo, bctbl(2,i),
     2            bctbl(3,i), bctbl(6,i) , bctbl(11,i)
            endif
            i = bctbl_nxt(i)
            enddo
 1150    continue
 1160    continue
         na = 0
         do 1170 i = ipct, ipctno
            call getchr (4, tmpxxx, pctge(1,i))
            if (tmpxxx(2:4) .eq. tag) then
               na = na + ipctch(1,i) + ipctch(2,i)
            endif
 1170    continue
         write (outbuf,1180) pctge(1,ipct)
 1180    format ('0     SUMMARY OF PERCENTAGE CHANGES TYPE = "P', a1,
     1           '"')
         call prtout (1)
         write (outbuf,1190)
 1190    format ('0 CATEGORY   NO. OF CHANGES   ------------ ',
     1           'ORIGINAL QUANTITIES ----------    ------------ ',
     2           'FINAL QUANTITES --------------')
         call prtout(1)
         write (outbuf,1200)
 1200    format ('             BUSES  CUST/BUS  ------- ',
     1           'LOAD ------------ GENERATION -----    ------- ',
     2           'LOAD -------  ---- GENERATION -----')
         call prtout (1)
         write (outbuf,1210)
 1210    format ('                                 (MW)        (',
     1           'MVAR)  (MW)     (MVAR)       (MW)       (',
     2           'MVAR)      (MW)        (MVAR')
         call prtout (1)
         call space (1)
         do 1230 i = ipct, ipctno
            call getchr (4, tmpxxx, pctge(1,i))
            if (tmpxxx(2:4) .eq. tag) then
               plm = pctge(3,i) * pctchg(1,i)
               qlm = pctge(4,i) * pctchg(2,i)
               pgm = pctge(5,i) * pctchg(3,i)
               qgm = pctge(6,i) * pctchg(4,i)
               write (outbuf,1220) pctzow(1,i), (ipctch(j,i),j=1,2)
     1            ,(pctchg(j,i),j=1,4), plm, qlm, pgm, qgm
 1220          format (4x, a3, 7x, i4, 6x, i4, 1x, f9.1, 4x, f9.1, 1x,
     1            f9.1, 4x, f9.1, 1x, f9.1, 4x, f9.1, 1x, f9.1, 4x,
     2            f9.1)
               call prtout (1)
            endif
 1230    continue
 1240    continue
 1250 continue
 1260 continue
      return
      end
