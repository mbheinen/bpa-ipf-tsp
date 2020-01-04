C    @(#)interc.f	20.6 10/13/99
      subroutine interc
 
C     ROUTINE TO WRITE THE INTERCHANGE MATRIX AND/OR THE TIE-FLOWS
 
      include 'ipfinc/parametr.inc'
 
      include 'ipfinc/anlys.inc'
      include 'ipfinc/arcntl.inc'
      include 'ipfinc/area.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/busanl.inc'
      include 'ipfinc/dc2t.inc'
      include 'ipfinc/dcmt.inc'
      include 'ipfinc/intchg.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/slnopt.inc'

      integer MAXTIESORT
      parameter (MAXTIESORT = 200)
      common /tiesrt/ tiesrt(MAXTIESORT)
      integer tiesrt
 
      character zn * 2, arnam1 * 10, arnam2 * 10, j1c * 1, j2c * 1

      external kmptie, swptie, find_zon
      integer find_zon
 
      if (ntotc .eq. 0 .or. jtie .eq. 0) go to 840
 
C     KSPARE(12) = 0    NO INTERCHANGE OUTPUT                          *
C                  1    AI MATRIX OUTPUT ONLY                          *
C                  2    TIE LINE FLOWS OUTPUT ONLY                     *
C                  3    BOTH MATRIX AND TIE FLOW OUTPUTS               *
 
      matlis = 0
C     Matrix list switch off
 
      linlis = 0
C     Tie flow list switch off
 
      if ( kspare(12) .eq. 1 ) then
C        Matrix only on
         matlis = 1
 
      else if ( kspare(12) .eq. 2 ) then
C        Tie Flow only on
         linlis = 1
 
      else if( kspare(12) .eq. 3 ) then
C        Both on
         matlis = 1
         linlis = 1
      endif
 
C     "INTOTM" DEFINES INTERCHANGE TYPE
C    
C            INTOTM   DESCRIPTION
C    
C               0      CONTROL "AC" DATA
C               1      MONITOR "AC" DATA
 
      intotm = 0
      if ( iopton(17) .eq. 2 ) intotm = 1
 
C     SET UP FICHE/PRINT SWITCHES AND CONTROLS
 
      lswt = 0
      lprtsw = min0(1,kspare(12))
      if(kspare(16).ge.0) fichsw = lprtsw
 
      if (lprtsw+fichsw .eq. 0) goto 820
 
      if (fichsw .eq. 1) then
         outbuf = '$MFCB      INTERCHANGE'
         call pfomf(1)
      endif
 
      jount = 0
      nmax = (MAXCAR*MAXCAR+MAXCAR) / 2
 
      do 100 k = 1, nmax
         tmat(k) = 0.0
  100 continue
 
C     CALCULATE ALL TIE LINE INTERCHANGES
 
      do 190 jt = 1, jtie
         k1 = tie(1,jt)
         k2 = tie(7,jt)
         ka1 = tie(2,jt)
         ka2 = tie(8,jt)
         kdc = tie(9,jt)
 
         if (kdc .eq. 0) go to 170
         if (mtdcbs .eq. 0) go to 130
 
         do 110 mdc = 1, mtdcbs
            m1 = dcmtln(1,mdc)
            m2 = dcmtln(2,mdc)
            if (min0(m1,m2) .ne. min0(k1,k2)) go to 110
            if (max0(m1,m2) .eq. max0(k1,k2)) go to 120
  110    continue
 
         go to 130
 
  120    if (m1 .eq. k1) then
            l1 = dcmtln(8,mdc)
            l2 = dcmtln(9,mdc)
 
         else
            l1 = dcmtln(9,mdc)
            l2 = dcmtln(8,mdc)
         endif
 
         v1 = dcmtbs(20,l1)
         v2 = dcmtbs(20,l2)
         pin = v1 * (v1-v2) / dcmtln(4,mdc)
         go to 180
 
  130    if (kdtot .eq. 0) call erexit
 
         do 140 mdc = 1, kdtot
            if (dc2t(1,mdc) .eq. k1 .and. dc2t(3,mdc) .eq. k2) goto 150
            if (dc2t(1,mdc) .eq. k2 .and. dc2t(3,mdc) .eq. k1) goto 160
  140    continue
 
         call erexit
 
  150    pin = 0.001 * dc2t(39,mdc) * dc2t(40,mdc)
         go to 180
 
  160    pin = -0.001 * dc2t(39,mdc) * dc2t(41,mdc)
         go to 180
 
  170    kt = inp2opt(k1)
         mt = inp2opt(k2)
         ek = e(kt)
         fk = f(kt)
         em = e(mt)
         fm = f(mt)
         gk11 = tie(3,jt)
         bk11 = tie(4,jt)
         gk12 = tie(5,jt)
         bk12 = tie(6,jt)
         aik = ek * gk11 - fk * bk11 + em * gk12 - fm * bk12
         bik = ek * bk11 + fk * gk11 + em * bk12 + fm * gk12
         pin = (ek*aik+fk*bik) * bmva
C                                                                      *
  180    tiesum(1,jt) = pin
C                                                                      *
C        COMPUTE AND STORE NET AREA EXPORT                             *
C                                                                      *
         nd1 = (ka1*ka1+ka1) / 2
         nd2 = (ka2*ka2+ka2) / 2
         tmat(nd1) = tmat(nd1) + pin
         tmat(nd2) = tmat(nd2) - pin
C                                                                      *
         if (ka2 .le. ka1) then
            nd = nd1 - ka1 + ka2
         else
            nd = nd2 - ka2 + ka1
            pin = -pin
         endif
C                                                                      *
         tmat(nd) = tmat(nd) + pin
C                                                                      *
  190 continue
C                                      OUTPUT RESULTS
      if ( matlis .eq. 1 ) then
         call forbtm
         iswt = 0
         ktot1 = 1
         nsave = 1
         mshift = 0
         n = 0
         m1 = 1
         m12 = 12
         m13 = 13
         m24 = 24
 
         if (ntotc .le. 12) then
            ktot2 = ntotc
            kosw = 1
         else
            ktot2 = 12
            kosw = 2
         endif
 
         if (intotm .eq. 1) then
            write (outbuf,200)
  200       format ('MONITORED MEGAWATT INTERCHANGE BETWEEN AREAS')
            call rpnlod
            call fortop
 
         else
            write (outbuf,210)
  210       format ('CONTROLLED MEGAWATT INTERCHANGE BETWEEN AREAS')
            call rpnlod
            call fortop
            write (outbuf,220)
  220       format ('0', 57x, '( SCHEDULED VALUES )')
            call prtout(1)
            write (outbuf,230) chase1(1), dte
  230       format ('0', 50x, 'CASE ', a10, ' DATE ', a10)
            call prtout(1)
         endif
 
C        PRINT INVERTED LOWER-DIAGONAL EXPORT MATRIX:                  *
 
         write (outbuf,240) (arcnam(k),k=ktot1,ktot2,2)
  240    format ('0', 12x, 6(a10, 8x))
         call prtout(1)
C                                                                      *
         ktot11 = ktot1 + 1
         write (outbuf,250) (arcnam(k),k=ktot11,ktot2,2)
  250    format ('0', 21x, 6(a10, 8x))
         call prtout(1)
C                                                                      *
  260    do 300 jl = ktot1, ktot2
            write (outbuf,270) arcnam(jl)
  270       format ('0', a10)
            na1 = (jl*jl+jl) / 2
            write (outbuf(12:),280) (tmat(jk),jk=nsave,na1)
  280       format (12(f7.1, 2x))
            anet = arcnet(jl) * bmva
            if ( intotm .ne. 1 ) then
               n = n + 1
               i = 10 + 9 * n
               write (outbuf(i+1:),290) anet
  290          format ('(', f7.1, ')')
            endif
            call prtout(1)
            nsave = na1 + mshift + 1
  300    continue
 
  310    if ( kosw .gt. 1 ) then
            if ( kosw .eq. 3) ktot2 = ktot2 + 12
            ktot3 = ktot2 + 1
            if ( (ntotc-ktot2) .gt. 12) then
               ktot4 = ktot2 + 12
               kosw = 3
            else
               iswt = 1
               ktot4 = ntotc
            endif
            do 340 jl = ktot3, ktot4
               lj = jl - 1
               kj = (lj*lj+lj) / 2 + m1
               kl = kj + 11
               write (outbuf,270) arcnam(jl)
               write (outbuf(12:),330) (tmat(jk),jk=kj,kl)
  330          format (12(f7.1, 2x))
               call prtout(1)
  340       continue
 
            if (iswt .eq. 0) go to 310
            nsave = (ktot3*ktot3+ktot3) / 2
            mshift = m12
            ktot1 = m13
            if (ntotc-m12 .gt. 12) then
               ktot2 = m24
               kosw = 2
            else
               ktot2 = ntotc
               kosw = 1
            endif
C    
C           BEGIN SECOND PAGE OF INTERCHANGE MATRIX
C    
            call forbtm
            call fortop
            write (outbuf,360) (arcnam(k),k=ktot1,ktot2,2)
  360       format ('0', 12x, 6(a10, 8x))
            call prtout(1)
            if ( ktot1 .ne. ktot2) then
               ktot11 = ktot1 + 1
               write (outbuf,370) (arcnam(k),k=ktot11,ktot2,2)
  370          format ('0', 21x, 6(a10, 8x))
               call prtout(1)
            endif
            iswt = 0
            n = 0
            m1 = m1 + 12
            m12 = m12 + 12
            m13 = m13 + 12
            m24 = m24 + 12
            go to 260
         endif
      endif
 
      if ( linlis .eq. 1 ) then
         call forbtm
C                            OUTPUT CHANGE IN SLACK BUS GENERATION
         write (outbuf,380)
  380    format ('TIE LINE SUMMARY OF AREA INTERCHANGE')
         call rpnlod
         outbuf = ' '
         do 390 i = 1, 5
            call shdlod(i)
  390    continue
         call fortop
         write (outbuf,400)
  400    format ('0', 39x, '(NEGATIVE FLOW DENOTES POWER ',
     1      'IMPORTED BY AREA1)')
         call prtout(1)
         write (outbuf,410)
  410    format ('0', 47x, ' (*)', 'INDICATES THE METERED END')
         call prtout(1)
         write (outbuf,420) chase1(1), dte
  420    format ('0', 44x, 'CASE ', a10, ' DATE ', a10)
         call prtout(1)
C                                                                      *
C        IF (INTOTM.NE.0) GO TO 680                                    *
         write (outbuf,430)
  430    format ('0', 6x, 'AREA NAME', '       SLACK BUS',
     1           '               SLACK BUS GENERATION',
     2           '            NET       ZONE COMPOSITION')
         call prtout(1)
C                                                                      *
         write (outbuf,440)
         call prtout(1)
  440    format (20x, 'NAME       BASE',
     1      '   BEGINNING   CHANGE   FINAL   MAXIMUM   INTERCHANGE ')
         write (outbuf,450)
  450    format (42x, 'MW        MW       MW      MW        MW ')
         call prtout(1)
         outbuf = ' '
         call prtout(1)
C                                                                      *
         do i = 1, ntotc
            anet = arcnet(i) * bmva
            pgold = area(7,i)
            pgnew = area(8,i)
            padj = pgnew - pgold
            write (outbuf,460) arcnam(i), arcbus(i), arcbas(i), pgold,
     1         padj, pgnew, area(6,i), anet, (arczns(j,i),j=1,MAXCAZR)
  460       format (7x, a10, 3x, a8, f7.1, 4x, f7.1, f9.1, f9.1, f9.1,
     1         2x, f9.1, 6x, 10(a2, 2x))
            call prtout(1)
            do j = 1, 9
               j1 = j * MAXCAZR + 1
               j2 = (j+1) * MAXCAZR
               if (arczns(j1,i) .ne. ' ') then
                  write (outbuf,462) (arczns(jk,i),jk=j1,j2)
  462             format (t91, 10(a2, 2x))
                  call prtout(1)
               endif
            enddo
         enddo
C                                                                      *
C        LOAD PAGE HEADERS                                             *
C                                                                      *
  540    call forbtm
         write (outbuf,550)
  550    format (t2, 'AREA 1', t16, 'AREA 2', t33,
     1      '/------------------- INTERTIE BRANCH -----------------/',
     2      t95, 'SCHEDULED', t106, '  ACTUAL', t117, 'CIRCULATING')
         call shdlod(1)
         write (outbuf,560)
  560    format (t34, 'ZONE BUS 1', t55, 'ZONE BUS 2', t79, 'LINE FLOW'
     1      , t95, 'INTERTIE', t106, ' INTERTIE', t117, '   FLOW')
         call shdlod(2)
         write (outbuf,570)
  570    format (t79, '   (MW)', t95, '   (MW)', t106, '   (MW)', t117,
     1       '   (MW)')
         call shdlod(3)
         outbuf = ' '
         call shdlod(4)
         call shdlod(5)
C                                                                      *
C        OUTPUT INDIVIDUAL TIE LINE FLOWS                              *
C                                                                      *
         do 810 kt = 1, ntotc
            totexp = 0.0
            ntie = 0
            do 590 i = 1, jtie
               if (tie(2,i) .eq. kt) then
                  ntie = ntie + 1
                  tiesrt(ntie) = i
               else if (tie(8,i) .eq. kt) then
                  ntie = ntie + 1
                  tiesrt(ntie) = -i
               endif
  590       continue
            call qiksrt (1,ntie,kmptie,swptie)
C                                                                      *
C           LOOP THROUGH EACH AREA                                     *
C                                                                      *
            arnam1 = arcnam(kt)
            ix = 0
  600       ix = ix + 1
            if (ix .gt. ntie) go to 680
            jt = iabs(tiesrt(ix))
            if (tie(2,jt) .eq. kt) then
               ka2 = tie(8,jt)
            else
               ka2 = tie(2,jt)
            endif
            arnam2 = arcnam(ka2)
C                                                                      *
C           FIND TOTAL AREA 1 - AREA 2 INTERTIE                        *
C                                                                      *
            subtot = 0.0
            do 610 jx = ix, ntie
               lt = iabs(tiesrt(jx))
               if (tie(2,lt) .eq. kt .and. tie(8,lt) .eq. ka2) then
                  subtot = subtot + tiesum(1,lt)
               else if (tie(2,lt) .eq. ka2 .and. tie(8,lt) .eq. kt)
     1            then
                  subtot = subtot - tiesum(1,lt)
               else
                  go to 620
               endif
  610       continue
            jx = ntie + 1
C                                                                      *
C           SEARCH FOR SCHEDULED INTERTIE FLOW.                        *
C                                                                      *
  620       jx = jx - 1
            sched = 0.0
            do 630 i = 1, ntotic
               if (arcint(1,i) .eq. arcnam(kt) .and. arcint(2,i) .eq.
     1            arcnam(ka2)) then
                  sched = arcinp(i)
                  go to 640
               endif
  630       continue
  640       circul = subtot - sched
            ksw = 0
            do 670 j = ix, jx
               lt = iabs(tiesrt(j))
               if (tie(2,lt) .eq. kt) then
                  k = tie(1,lt)
                  m = tie(7,lt)
                  j1c = '*'
                  j2c = ' '
                  export = tiesum(1,lt)
               else
                  k = tie(7,lt)
                  m = tie(1,lt)
                  j1c = ' '
                  j2c = '*'
                  export = -tiesum(1,lt)
               endif
               totexp = totexp + export
               if (ksw .eq. 0) then
                  write (outbuf,650) arnam1, arnam2, zone(k), j1c,
     1               bus(k), base(k), zone(m), j2c, bus(m), base(m),
     2               export, sched, subtot, circul
  650             format ('0', t2, a10, t16, a10, t34, a2, 1x, a1, 1x,
     1               a8, f7.1, t55, a2, 1x, a1, 1x, a8, f7.1, t79, f8.1
     2               , t95, f9.1, t106, f9.1, t117, f9.1)
                  call prtout(1)
                  arnam1 = ' '
                  arnam2 = ' '
                  ksw = 1
               else
                  write (outbuf,660) zone(k), j1c, bus(k), base(k),
     1               zone(m), j2c, bus(m), base(m), export
  660             format (t34, a2, 1x, a1, 1x, a8, f7.1, t55, a2, 1x,
     1               a1, 1x, a8, f7.1, t79, f8.1)
                  call prtout(1)
               endif
  670       continue
            ix = jx
            go to 600
  680       call space(1)
            write (outbuf,690) arcnet(kt) * bmva, totexp
  690       format (t16, 'AREA TOTAL', t46, '   (MW)', t58, '   (MVAR)'
     1         , t95, f9.1, t106, f9.1)
            call prtout(1)
 
            if (nztot .eq. 0) go to 810

            do 700 i = 1, 8
               z(i) = 0.0
  700       continue
            ii = 0
            do 710 i = 1, MAXCAZ
               zn = arczns(i,kt)
               if (zn .eq. '  ') then
                  if (i .eq. 1) then
                     ii = ii + 1
                     zname(ii) = zn
                  else
                     go to 720
                  endif
               else
                  ii = ii + 1
                  zname(ii) = zn
               endif
  710       continue
C                                                                      *
  720       do 740 i = 1, ii
               ij = find_zon(zname(i))
               if (ij .le. 0) go to 740
               do 730 jj = 1, 8
  730          z(jj) = z(jj) + zsum(jj,ij)
  740       continue
C                                                                      *
            write (outbuf,750) z(1), z(2)
  750       format (t34, 'GENERATION', t46, f9.1, t58, f9.1)
            call prtout(1)
            write (outbuf,760) z(3), z(4)
  760       format (t34, 'LOAD', t46, f9.1, t58, f9.1)
            call prtout(1)
            write (outbuf,770) z(5), z(6)
  770       format (t34, 'LOSSES', t46, f9.1, t58, f9.1)
            call prtout(1)
            write (outbuf,780) z(7), z(8)
  780       format (t34, 'SHUNT', t46, f9.1, t58, f9.1)
            call prtout(1)
            pcheck = z(1) - z(3) - z(5) - z(7)
            qcheck = z(2) - z(4) - z(6) + z(8)
            write (outbuf,790) pcheck, qcheck
  790       format (t34, 'NET EXPORT', t46, f9.1, t58, f9.1)
            call prtout(1)
            pcheck = pcheck - totexp
C                                                                      *
            if (abs(pcheck) .gt. option(5)*bmva .and.
     1         iopton(17) .eq. 1) then
               write (errbuf(1),800) arcnam(kt), pcheck
  800          format ('0 ACCOUNTING ERROR IN AREA ', a10,
     1            ' INTERCHANGE:', ' GENERATION - LOAD',
     2            ' - LOSSES - SHUNT - EXPORT = ', f10.1)
               call prterx ('W',1)
            endif
C                                                                      *
            call space(1)
C                                                                      *
  810    continue
      endif
 
  820 continue
      call forbtm
C                                                                      *
      do 830 i = 1, 5
         outbuf = ' '
         call shdlod(i)
  830 continue
C                                                                      *
  840 return
      end
