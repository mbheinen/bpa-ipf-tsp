C    @(#)inf100.f	20.4 11/11/97
      subroutine inf100
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/blank.inc'
      include 'ipfinc/branch.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/com013.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/merge.inc'
      include 'ipfinc/mrgtxt.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/qksrt.inc'
      include 'ipfinc/sort2.inc'
 
      dimension igrp1(8,50), igrp2(8,50), match(2,50), xbuf(50)
      character type * 1, bus1 * 8, bus2 * 8, id * 1, xbuf * 130,
     1          kown3 * 3, kown4 * 3, kown1 * 3, kown2 * 3, lown1 * 3,
     2          lown2 * 3, txpose * 130
      integer find_bus, sect, p, shift
 
      external kpintr, spintr, kpface, spface
C                                                                      *
C     SORT MASTER INTERFACE ARRAY                                      *
C                                                                      *
      key = 2
      call qiksrt (1, itface, kpface, spface)
C                                                                      *
C     FLAG LAST BUS                                                    *
C                                                                      *
      bus(ntot+1) = srtlst
      alf2inp(ntot+1) = ntot + 1
C                                                                      *
C     PROCESS INTERFACE TEXT DATA                                      *
C                                                                      *
      it = nifmrg
      do 180 i = 1, it
         if (kase1(33) .gt. 0) then
            write (dbug, 100) i, fcetxt(i)(1:80), fcetxt(i)(121:130)
  100       format (' INTERFACE BRANCH ', i2, ' (', a80, a10, ')')
         endif
         read (fcetxt(i),120,err=160) type, bus1, base1, bus2, base2,
     1      id, sect
  120    format (bz, a1, 5x, a8, f4.0, 1x, a8, f4.0, a1, i1)
C                                                                      *
C        Define interface sort index                                   *
C                                                                      *
         if (type .eq. 'R') then
            ktyp = 0
            id = char (0)
            isect = 0
         else
            ktyp = 1
         endif
         if (fcetxt(i)(1:2) .eq. 'LD' .or. 
     &       fcetxt(i)(1:2) .eq. 'LM') then
            id = char (0)
            isect = 0
         endif
         k1 = find_bus(bus1,base1)
         if (k1 .le. 0) k1 = ntot + 1
         k2 = find_bus(bus2,base2)
         if (k2 .le. 0) k2 = ntot + 1
         isect = sect
         if (k1 .ge. k2) isect = mod(10 - sect,10)

         ifsort(1,i) = ipack_2 (k1, k2)
         ifsort(2,i) = ipack_4 (ktyp, ichar(id), isect, 0)
C                                                                      *
C        Transpose branch data.  Note that "INF100" must               *
C        maintain the present (double-entry) branch data               *
C        file.  If an existing (system 2) branch is to be              *
C        replaced with an interface branch, both entries               *
C        must be deleted, and a single ascii-formatted branch          *
C        added.                                                        *
C                                                                      *
         fcetxt(i+it) = txpose (fcetxt(i))
         fcetxt(i+it)(121:130) = fcetxt(i)(121:130)
C                                                                      *
C        Change transpose switch                                       *
C                                                                      *
         if (fcetxt(i)(122:122) .eq. '0') then
            fcetxt(i+it)(122:122) = '1'
         else
            fcetxt(i+it)(122:122) = '0'
         endif
C                                                                      *
C        Transpose ownerships                                          *
C                                                                      *
         fcetxt(i+it)(123:125) = fcetxt(i)(126:128)
         fcetxt(i+it)(126:128) = fcetxt(i)(123:125)
         if (kase1(33) .gt. 0) then
            ix = i + it
            write (dbug, 100) ix, fcetxt(ix)(1:80), fcetxt(ix)(121:130)
         endif
C                                                                      *
C        Define transpose interface sort index                         *
C                                                                      *
         isect = sect
         if (k2 .ge. k1) then
            isect = 9 - isect
         endif
         ifsort(1,i+it) = ipack_2 (k2, k1)
         ifsort(2,i+it) = ipack_4 (ktyp, ichar(id), isect, 0)
         go to 180

  160    write (errbuf(1), 170) fcetxt(i)(1:80), fcetxt(i)(121:130)
  170    format ('ILLEGAL CHARACTER IN FIELD :(', a80, a10, ')')
         call prterx ('W', 1)
  180 continue
C                                                                      *
C     Sort "IFSORT"                                                    *
C                                                                      *
      it = 2 * it
      call qiksrt (1, it, kpintr, spintr)

      call fortop
      write (outbuf, 190)
  190 format (
     1   ' SUMMARY OF INTERFACE BRANCHES ENCOUNTERED AND SELECTED ')
      call prtout(1)
      write (outbuf, 200)
  200 format (
     1   '0S /---- SYSTEM "2" - SYSTEM "1" --- INTERFACE RECORD ',
     2   '---------------------------/  I OWNERSHIP DECISION ')
      call prtout(1)
      write (outbuf, 210) comcha(3), comcha(2)
  210 format (9x, '(', a10, ')(', a10, ')', 55x, 'B1    B2')
      call prtout(1)
      call space(2)
      assign 300 to isw
      assign 380 to ksw
      lsw = 1
C                                                                      *
C     LSW assignments:                                                 *
C                                                                      *
C       1 - normal                                                     *
C       2 - EOI group1                                         *
C       3 - EOI group2                                         *
C       4 - EOI group1 and group2                              *
C                                                                      *
      ix = 0
      kkx = 0
C                                                                      *
C     Find next block in group 1 (system 2)                            *
C                                                                      *
  220 n1 = 0
  230 ix = ix + 1
      if (ix .le. itface) then
         k1 = face(1,ix)
         k2 = face(2,ix)
         if (k1 .eq. 0) go to 230
         p = kbsdta(16,k1) 
         do while (p .gt. 0) 
            if (k2 .eq. ky(p)) then
               n1 = n1 + 1
               call bcdbrn(p, xbuf(n1))
               ltype = brtype(p)
               igrp1(1,n1) = ltype
               igrp1(2,n1) = ichar (brid(p))
               igrp1(3,n1) = brsect(p)
               nbr = iabs (brnch_ptr(p))
               igrp1(4,n1) = kbrnch(3,nbr)
               if (ltype .eq. 2 .or. ltype .eq. 4 .or. ltype .eq. 7) 
     &            then
                  igrp2(2,n1) = 0
                  igrp1(3,n1) = 0
               endif
               igrp1(5,n1) = ix
               igrp1(6,n1) = p
               igrp1(7,n1) = 0
               igrp1(8,n1) = 0
               write (xbuf(n1)(121:130), 260) facec(ix)(1:1), 
     &            facec(ix)(2:4), facec(ix)(5:7)
  260          format ('1', a1, 2a3, ' 2')
               if (kase1(33) .gt. 0) then
                  write (dbug, 270) ix, k1, k2, xbuf(n1)(1:80), 
     1               xbuf(n1)(121:130)
  270             format (' BASE ', 3i5, '  (', a80, a10, ')')
               endif
            endif
            p = brnch_nxt(p)
         enddo
      
      else

         if (lsw .eq. 1 .or. lsw .eq. 3) lsw = lsw + 1
 
      endif
      go to isw (300, 380)
C                                                                      *
C     Find next block in group 2 (system 1)                            *
C                                                                      *
  300 n2 = 0
  310 kkx = kkx + 1
      if (kkx .gt. it) go to 360
      if (ifsort(1,kkx) .eq. 0) go to 310
      m1 = shift (ifsort(1,kkx), -16)
      m2 = shift (shift (ifsort(1,kkx), 16), -16)
      ktype = shift (shift (ifsort(2,kkx), 16), -16)
      if (kase1(33) .gt. 0) then
         write (dbug, 350) kkx, m1, m2, fcetxt(kkx)(1:80),
     1      fcetxt(kkx)(121:130)
      endif
      n2 = n2 + 1
      igrp2(1,n2) = ktype
      read (fcetxt(kkx), 330) igrp2(4,n2), id, igrp2(3,n2)
  330 format (bz, 3x, a3, 25x, a1, i1)
      igrp2(2,n2) = ichar (id)
      if (fcetxt(kkx)(1:1) .eq. 'R') then
         igrp2(2,n2) = 0
         igrp2(3,n2) = 0
      endif
      if (fcetxt(kkx)(1:2) .eq. 'LM' .or. 
     &    fcetxt(kkx)(1:2) .eq. 'LD') then
         igrp2(2,n2) = 0
         igrp2(3,n2) = 0
      endif
      igrp2(5,n2) = 0
      igrp2(6,n2) = kkx
      igrp2(7,n2) = 0
      igrp2(8,n2) = 0
      j = kkx 
  340 j = j + 1
      if (j .gt. it) go to 370
      if (ifsort(1,j) .eq. 0) go to 340
      m1j = shift (ifsort(1,j), -16)
      m2j = shift (shift (ifsort(1,j), 16), -16)
      if (m1 .ne. m1j .or. m2 .ne. m2j) go to 370
      n2 = n2 + 1
      igrp2(1,n2) = shift (ifsort(2,j), -16)
      read (fcetxt(j), 330) igrp2(4,n2), igrp2(2,n2), igrp2(3,n2)
      igrp2(5,n2) = 0
      igrp2(6,n2) = j
      igrp2(7,n2) = 0
      igrp2(8,n2) = 0
      if (kase1(33) .gt. 0) then
         write (dbug, 350) j, m1j, m2j, fcetxt(j)(1:80), 
     &      fcetxt(j)(121:130)
  350    format (' MRGE ', 3i5, '  (', a80, a10, ')')
      endif
      go to 340
 
  360 if (lsw .eq. 1 .or. lsw .eq. 2) lsw = lsw + 2
  370 go to ksw (380, 460, 530)
  380 go to (390, 700, 680, 1030) lsw
  390 if (n1 .eq. 0 .and. n2 .gt. 0) go to 700
      if (n1 .gt. 0 .and. n2 .eq. 0) go to 680
C                                                                      *
C     Perform a merge-compare between groups 1 and 2                   *
C                                                                      *
      if (inp2alf(k1) - inp2alf(m1)) 680, 400, 700
  400 if (inp2alf(k2) - inp2alf(m2)) 680, 410, 700
C                                                                      *
C     Groups match-determine degree of match                           *
C                                                                      *
C     Weight - significance                                            *
C                                                                      *
C     10   - Parallels match (or both are type "R" records)            *
C      1   - sections match                                            *
C                                                                      *
C     Phase1 - find best match of group 1 with respect to group 2      *
C                                                                      *
  410 j = 1
  411 if( j .gt. n1 ) go to 481
         max = 0
         if (igrp1(1,j) .eq. 1) go to 480
         do 450 l = 1, n2
            match(1,l) = 0
            if (igrp1(1,j) .eq. 4) go to 420
            if (igrp2(1,l) .eq. 0) go to 450
            if (igrp1(2,j) .eq. igrp2(2,l)) then
               match(1,l) = match(1,l) + 10
            endif
            if (igrp1(3,j) .eq. igrp2(3,l)) then
               match(1,l) = match(1,l) + 1
            endif
            go to 430
  420       if (igrp2(1,l) .eq. 0) then
               match(1,l) = match(1,l) + 10
            endif
  430       if (max .eq. 0) go to 440
            if (match(1,l) .gt. match(1,max)) then
               max = l
            endif
            go to 450
  440       if (match(1,l) .gt. 0) max = l
  450    continue
C                                                                      *
C        Determine branch match                                        *
C                                                                      *
         if (max .eq. 0) go to 480
         igrp1(7,j) = match(1,max)
C                                                                      *
C        Compare branch J (group1) with branch max (group2)            *
C        Results are returned in variables "I1" and "I2"               *
C                                                                      *
         ii = j
         jj = igrp2(6,max)
         assign 460 to ksw
         go to 720
  460    igrp1(8,j) = i1
         if (kase1(33) .eq. 0) go to 480
         write (dbug, 470) j, max, igrp1(7,j), igrp1(8,j),
     1      xbuf(ii)(1:80), xbuf(ii)(121:130)
  470    format (' BASE-MERGE ', 4i4, ' (', a80, a10, ')')
  480 continue
      j = j + 1
      go to 411
  481 continue
C                                                                      *
C     Phase2 - find best match of group 2 with respect to group 1      *
C                                                                      *
      j = 1
  485 if ( j .gt. n2 ) go to 551
         max = 0
         do 520 l = 1, n1
            match(2,l) = 0
            if (igrp1(1,l) .eq. 1) go to 520
            if (igrp2(1,j) .eq. 0) go to 490
            if (igrp1(1,l) .eq. 4) go to 520
            if (igrp2(2,j) .eq. igrp1(2,l)) then
               match(2,l) = match(2,l) + 10
            endif
            if (igrp2(3,j) .eq. igrp1(3,l)) then
               match(2,l) = match(2,l) + 1
            endif
            go to 500
  490       if (igrp1(1,l) .eq. 4) then
               match(2,l) = match(2,l) + 10
            endif
  500       if (max .eq. 0) go to 510
            if (match(2,l) .gt. match(2,max)) then
               max = l
            endif
            go to 520
  510       if (match(2,l) .gt. 0) max = l
  520    continue
C                                                                      *
C        Determine branch match                                        *
C                                                                      *
         if (max .eq. 0) go to 550
         igrp2(7,j) = match(2,max)
C                                                                      *
C        Compare branch J (group2) with branch MAX (group1)            *
C                                                                      *
         ii = max
         jj = igrp2(6,j)
         assign 530 to ksw
         go to 720
  530    igrp2(8,j) = i2
         if (kase1(33) .eq. 0) go to 550
         write (dbug, 540) max, j, igrp2(7,j), igrp2(8,j),
     1      fcetxt(jj)(1:80), fcetxt(jj)(121:130)
  540    format (' MERGE-BASE ', 4i4, ' (', a80, a10, ')')
  550 continue
      j = j + 1
      go to 485
  551 continue
C                                                                      *
C     Pair matching items group1 and group2 for preferential qualifier *
C                                                                      *
      imax = 1
      imin = 1
      maxgp1 = igrp1(7,imax) * igrp1(8,imax)
      mingp1 = maxgp1
      if (n1 .eq. 1) go to 580
      do 570 i = 2, n1
         j = igrp1(7,i) * igrp1(8,i)
         if (j .le. maxgp1) go to 560
         imax = i
         maxgp1 = j
  560    if (j .gt. mingp1) go to 570
         imin = i
         mingp1 = j
  570 continue
  580 jmax = 1
      jmin = 1
      maxgp2 = igrp2(7,jmax) * igrp2(8,jmax)
      mingp2 = maxgp2
      if (n2 .eq. 1) go to 610
      do 600 i = 2, n2
         j = igrp2(7,i) * igrp2(8,i)
         if (j .gt. maxgp2) then
            jmax = i
            maxgp2 = j
         endif
         if (j .lt. mingp2) then
            jmin = i
            mingp2 = j
         endif
  600 continue
  610 if (maxgp1 .gt. maxgp2) go to 640
C                                                                      *
C     Group2 is selected - set flags                                   *
C                                                                      *
      do 620 i = 1, n1
  620 match(1,i) = 11
      i1 = 9
      i2 = igrp1(8,imin)
      if (iabs(i2) .ge. 10) then
         i1 = 8
      endif
      if (iabs(i2) .ge. 100) then
         i1 = 7
      endif
      match(1,imin) = i1
      do 630 i = 1, n2
         match(2,i) = 5
  630 continue
      i1 = 3
      i2 = igrp2(8,jmax)
      if (iabs(i2) .ge. 10) then
         i1 = 2
      endif
      if (iabs(i2) .ge. 100) then
         i1 = 1
      endif
      match(2,jmax) = i1
      go to 670
C                                                                      *
C     Group1 is selected - set flags                                   *
C                                                                      *
  640 do 650 i = 1, n1
         match(1,i) = 5
  650 continue
      i1 = 3
      i2 = igrp1(8,imax)
      if (iabs(i2) .ge. 10) i1 = 2
      if (iabs(i2) .ge. 100) i1 = 1
      match(1,imax) = i1
      do 660 i = 1, n2
         match(2,i) = 11
  660 continue
      i1 = 9
      i2 = igrp2(8,jmin)
      if (iabs(i2) .ge. 10) then
         i1 = 8
      endif
      if (iabs(i2) .ge. 100) then
         i1 = 7
      endif
      match(2,jmin) = i1
  670 assign 950 to isw1
      assign 1000 to ksw1
      go to 890
C                                                                      *
C     Group1 is solitary                                               *
C                                                                      *
  680 i1 = 4
      if (k1 .eq. ntot+1) then
         i1 = 12
      endif
      if (k2 .eq. ntot+1) then
         i1 = 13
      endif
      do 690 i = 1, n1
  690 match(1,i) = i1
      assign 1010 to isw1
      go to 890
C                                                                      *
C     Group2 is solitary                                               *
C                                                                      *
  700 i1 = 4
      if (m1 .eq. ntot+1) then
         i1 = 12
      endif
      if (m2 .eq. ntot+1) then
         i1 = 13
      endif
      do 710 i = 1, n2
  710 match(2,i) = i1
      assign 1020 to ksw1
      go to 950
C                                                                      *
C     Determine interface preference                                   *
C                                                                      *
C     Interface weighing factors:                                      *
C                                                                      *
C     INTFC - 100                                                      *
C     OWNER -  10                                                      *
C     SYSTEM -  1                                                      *
C                                                                      *
  720 i1 = 0
      i2 = 0
      read (xbuf(ii), 730, err=850) lown1, ifcsw1, ktrps1, kown1, kown2,
     1   isyst1
  730 format (bz, t4, a3, t121, 2i1, 2a3, i2)
      read (fcetxt(jj), 730, err=850) lown2, ifcsw2, ktrps2, kown3,
     1   kown4, isyst2
      if (isyst1 .eq. isyst2) go to 820
C                                                                      *
C     Test interface switch                                            *
C                                                                      *
      if (ifcsw1-ifcsw2) 750, 760, 740
  740 i1 = 100
      i2 = -100
      go to 880
  750 i1 = -100
      i2 = 100
      go to 880
C                                                                      *
C     Test ownership                                                   *
C                                                                      *
  760 j1 = 0
      j2 = 0
      if (lown1 .eq. '   ') go to 770
      if (lown1 .eq. kown1 .or. lown1 .eq. kown2) j1 = 1
      if (lown1 .eq. kown1 .and. ktrps1 .eq. 0) j1 = j1 + 1
      if (lown1 .eq. kown2 .and. ktrps1 .eq. 1) j1 = j1 + 1
  770 if (lown2 .eq. '   ') go to 780
      if (lown2 .eq. kown3 .or. lown2 .eq. kown4) j2 = 1
      if (lown2 .eq. kown3 .and. ktrps2 .eq. 0) j2 = j2 + 1
      if (lown2 .eq. kown4 .and. ktrps2 .eq. 1) j2 = j2 + 1
  780 if (j1-j2) 800, 810, 790
  790 i1 = 10
      i2 = -10
      go to 880
  800 i1 = -10
      i2 = 10
      go to 880
C                                                                      *
C     Test system preference                                           *
C                                                                      *
  810 if (isyst1-isyst2) 830, 820, 840
  820 call erexit
      go to 880
  830 i1 = 1
      i2 = -1
      go to 880
  840 i1= -1
      i2= 1
      go to 880
  850 write (errbuf(1), 860)
  860 format ('0 FATAL ERROR - ILLEGAL CHARACTER IN FIELD')
      write (errbuf(2), 870) ii, xbuf(ii)(1:80), xbuf(ii)(121:130)
  870 format (' TEXT ', i4, ' (', a80, a10, ')')
      write (errbuf(3), 870) jj, fcetxt(jj)(1:80), fcetxt(jj)(121:130)
      call prterx ('F', 3)
  880 go to ksw (380, 460, 530)
C                                                                      *
C     Process group1 (KTRPS1 = 0 for system 2 - system 1)              *
C                                                                      *
  890 if (ktrps1 .eq. 0) then
         outbuf = ' '
         call prtout(1)
      endif
      do 930 i = 1, n1
         i1 = match(1,i)
         j = igrp1(5,i)
         k = igrp1(6,i)
         if (igrp1(1,i) .eq. 1) go to 910
         read (xbuf(i), 730, err=920) lown1, ifcsw1, ktrps1, kown1,
     1      kown2, isyst1
         write (outbuf, 900) isyst1, xbuf(i)(1:80), ifcsw1, kown1,
     1      kown2, commnt(i1)(1:39)
  900    format (1x, i1, 1x, a80, 1x, i1, 1x, a3, 1x, a3, 1x, a39)
         if (ktrps1 .eq. 0) call prtout(1)
  910    if (i1 .le. 5) go to 930
c******* ky(k) = ntot + 1 *********** old PF method
         call del_brn_tp(k)
         go to 930
  920    write (errbuf(1), 860)
         write (errbuf(2), 870) i, xbuf(i)(1:80), xbuf(i)(121:130)
         call prterx ('F', 2)
  930 face(1,j) = 0
      go to isw1 (950, 1010)
C                                                                      *
C     Process group2 (KTRPS2 = 1 for system 2 - system 1)              *
C                                                                      *
  950 if (lsw .eq. 1) go to 960
      if (ktrps2 .eq. 1) then
         outbuf = ' '
         call prtout(1)
      endif
  960 do 980 i = 1, n2
         i1 = match(2,i)
         k = igrp2(6,i)
         read (fcetxt(k), 730, err=970) lown1, ifcsw1, ktrps2, kown1,
     1      kown2, isyst2
         write (outbuf, 900) isyst2, fcetxt(k)(1:80), ifcsw1, kown1,
     1      kown2, commnt(i1)(1:39)
         if (ktrps2 .eq. 1) call prtout(1)
         if (i1 .gt. 0 .and. i1 .le. 6 .and. ktrps2 .eq. 1) then
            call brntxt (fcetxt(k))
         endif
         go to 980
  970    write (errbuf(1), 860)
         write (errbuf(2), 870) k, fcetxt(k)(1:80), fcetxt(k)(121:130)
         call prterx ('F', 2)
  980 ifsort(1,k) = 0
      go to ksw1 (1000, 1020)
C                                                                      *
C     Advance group1 and group2                                        *
C                                                                      *
 1000 assign 300 to isw
      assign 380 to ksw
      go to 220
C                                                                      *
C     Advance group1 only                                              *
C                                                                      *
 1010 assign 380 to isw
      go to 220
C                                                                      *
C     Advance group2 only                                              *
C                                                                      *
 1020 assign 380 to ksw
      go to 300

 1030 continue
C                                                                      *
C     Interface processing loop completed                              *
C                                                                      *
      write (outbuf, 1040)
 1040 format ('0 NOTES:')
      call prtout(1)
      write (outbuf, 1050) comcha(3), comcha(2)
 1050 format ('0    1. THE INTERFACE BRANCH IMAGE (BUS1-BUS2) IS ',
     1   'ORIENTED SYSTEM "2" (', a10, ')  - SYSTEM "1" (', a10, ').')
      call prtout(1)
      write (outbuf, 1060)
 1060 format ('0    2. COLUMN "S" IDENTIFIES THE BASE SYSTEM FROM ',
     1        ' WHICH THE INTERFACE BRANCH IMAGE WAS OBTAINED.')
      call prtout(1)
      write (outbuf, 1070)
 1070 format ('0    3. SYSTEM "2" INTERFACE IMAGES ARE ORIENTED AS ',
     1   'LISTED- SUBSYSTEM "1" IMAGES ARE TRANSPOSED TO ENHANCE ',
     2   'READABILITY.')
      call prtout(1)
      write (outbuf, 1080)
 1080 format ('0    4. A BUS-BRANCH OWNERSHIP MATCH OCCURS IF: ')
      call prtout(1)
      write (outbuf, 1090)
 1090 format ('0       A. BUS1 OWNER MATCHES LINE OWNER, THEN SYSTEM',
     1        ' "2" IMAGE IS SELECTED, OR ')
      call prtout(1)
      write (outbuf, 1100)
 1100 format ('        B. BUS2 OWNER MATCHES LINE OWNER, THEN SYSTEM',
     1        ' "1" IMAGE IS SELECTED. ')
      call prtout(1)
      return
      end
