C    @(#)intfce.f	20.4 11/11/97
      subroutine intfce
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/amtrx.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/com013.inc'
      include 'ipfinc/ikk.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/merge.inc'
      include 'ipfinc/mrgsys.inc'
      include 'ipfinc/mrgtxt.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/sort2.inc'
 
      dimension igrp1(8,50), igrp2(8,50), match(2,50)

      character type*1,bus1*8,bus2*8,id*1,lown1*3,kown1*3,kown2*3,
     1          lown2*3, kown3*3, kown4*3, txpose*130, tempc*130,
     2          type2*1, id2*1, bus3*8, bus4*8

      integer find_bus, sect, status, rename_bus, sect2, shift
      logical finished
      external kpintr, spintr
 
      it = nifmrg
      if (it.eq.0) go to 900
C
C     COMBINE BUS DATA
C
      ntot = max0 (nbsys1,nbsys2)
      if (ntot .gt. MAXBUS) then
         write (errbuf(1),102) ntot,MAXBUS
  102    format (' Combined system has ',i4,' buses. Limit is ',i4,'.')
         call prterx ('W',1)
         kerrsw = 1
         go to 900
      endif
C
      do i = 1,ntot
         status = rename_bus (i, mrgbus(i), mrgbas(i))
      enddo
      call sortbus

      owner(ntot+1) = srtlst
      zone(ntot+1) = srtlst
c
c     Re-orient interface text data: system 1 - system 2
c
      i = 1
      do while (i .le. it)
        read (fcetxt(i), 103, err=108) type, bus1, base1, bus2, base2, 
     &    id, sect, ktrpos, isyst
  103   format (bz, a1, 5x, a8, f4.0, 1x, a8, f4.0, a1, i1, t122, i1,
     &     t129, i2)
        if (isyst .eq. 2 .and. sect .ne. 0) then
          j = i + 1
          finished = .false.
          do while (j .le. it .and. .not. finished)
            read (fcetxt(j), 103, err=104) type2, bus3, base3, bus4, 
     &        base4, id2, sect2, ktrpos2, isyst2
            if (bus1 .eq. bus3 .and. base1 .eq. base3 .and.
     &          bus2 .eq. bus4 .and. base2 .eq. base4 .and.
     &          id .eq. id2) then
              j = j + 1
            else
              finished = .true.
            endif
            go to 107

  104       write (errbuf(1), 105)
  105       format ('0 ILLEGAL CHARACTER IN FIELD')
            write (errbuf(2), 106) fcetxt(j)(1:80),fcetxt(j)(121:130)
  106       format(' (', a80, a10,')')
            call prterx ('F',2)
  107       continue

          enddo
          if (i .lt. j-1) then

c           Swap entities i   <--> j-1
c                         i+1 <--> j-2
c
            do k = 1, (j-i)/2
              tempc = fcetxt(i+k-1)
              fcetxt(i+k-1) = fcetxt(j-k)  
              fcetxt(j-k) = tempc
            enddo
          endif
          i = j
        else
          i = i + 1
        endif
        go to 109

  108   write (errbuf(1), 105)
        write (errbuf(2), 106) fcetxt(i)(1:80),fcetxt(i)(121:130)
        call prterx ('F',2)
  109   continue

      enddo
C
C     READ IN INTERFACE TEXT DATA
C
      do 320 i = 1,it
C
C       REARRANGE EACH BRANCH TO BE SYSTEM1 - SYSTEM2
C
        if (comcha(10) .ne. ' ' ) then
           write (dbug,112) i,fcetxt(i)(1:80),fcetxt(i)(121:130)
  112      format (' INTERFACE BRANCH ',i2,' (',a80,a10,')')
        endif

        read (fcetxt(i)(129:130), fmt='(bz,i2)', err=312) isyst
C
        if (isyst .eq. 2) then
           tempc = txpose (fcetxt(i))
           tempc(121:130) = fcetxt(i)(121:130)
           fcetxt(i) = tempc
C                                                                      *
C          Change transpose switch                                       *
C                                                                      *
           if (fcetxt(i)(122:122) .eq. '0') then
              fcetxt(i)(122:122) = '1'
           else
              fcetxt(i)(122:122) = '0'
           endif
C                                                                      *
C          Transpose ownerships                                          *
C                                                                      *
           fcetxt(i)(123:125) = tempc(126:128)
           fcetxt(i)(126:128) = tempc(123:125)
        endif
C
        read (fcetxt(i), 103, err=312) type, bus1, base1, bus2, base2, 
     &     id, sect, ktrpos
        if (ktrpos .ne. 0) sect = mod (10 - sect, 10)
C
C       DEFINE INTERFACE SORT INDEX
C
        k1 = find_bus (bus1,base1)
        if (k1 .le. 0) then
          k1 = 19999
        else
          k1 = inp2alf(k1)
        endif
        k2 = find_bus (bus2,base2)
        if (k2 .le. 0) then
          k2 = 19999
        else
          k2 = inp2alf(k2)
        endif 
        i1 = min0 (k1, k2)
        i2 = max0 (k1, k2)
        ktyp = 1
        if (type .eq. 'R') ktyp = 0
        ifsort(1,i) = ipack_2 (i1, i2)
        ifsort(2,i) = ipack_4 (isyst, ktyp, ichar(id), sect)
        go to 320
C
  312   write (errbuf(1), 105)
        write (errbuf(2), 106) fcetxt(i)(1:80),fcetxt(i)(121:130)
        call prterx ('F',2)
  320 continue
C
C     SORT "IFSORT"
C
      call qiksrt (1,it,kpintr,spintr)
C
      write (outbuf,330)
  330 format ('0 SUMMARY OF INTERFACE BRANCHES ENCOUNTERED AND ',
     1 'SELECTED')
      call prtout (1)
C
      write (outbuf,350)
  350 format ('0S ------------------------- BRANCH RECORD --------------
     1-------------------------- I OWNERSHIP DECISION ')
      call prtout(1)
C
      write (outbuf,352)
  352 format ( 58x,'                             B1    B2       ')
      call prtout(1)
C
      write (outbuf,832) comcha(2),comcha(3)
  832 format ('       (',a10,')       (',a10,')')
      call prtout(1)
C
      outbuf = ' '
      call prtout(1)
      assign 410 to isw
      assign 430 to ksw
      lsw=1
C
C   LSW ASSIGNMENTS:
C
C             1 - NORMAL
C             2 - EOI GROUP1
C             3 - EOI GROUP2
C             4 - EOI GROUP1 AND GROUP2
C
      ix = 0
      kx = 0
C
C     FIND NEXT BLOCK IN GROUP 1
C
  360 n1=0
  370 ix = ix + 1
      if (ix.gt.it) go to 394
      if (ifsort(1,ix).eq.0) go to 370
      k1 = shift (ifsort(1,ix), -16)
      k2 = shift (shift (ifsort(1,ix), 16), -16)
      isyst1 = shift (ifsort(2,ix), -16)
      if (isyst1.ne.1) goto 370
      ktype = shift (shift (ifsort(2,ix), 16), -24)
  390 n1=n1+1
      igrp1(1,n1)=ktype
      read (fcetxt(ix),416) igrp1(4,n1),igrp1(2,n1),igrp1(3,n1)
      igrp1(5,n1)=0
      igrp1(6,n1)=ix
      igrp1(7,n1)=0
      igrp1(8,n1)=0
      if (comcha(10) .eq. 'D') then
         write (dbug,771) ix,k1,k2,fcetxt(ix)(1:80),fcetxt(ix)(121:130)
  771    format(' BASE ',3i5,'  (',a80,a10,')')
      endif
      j=ix
  393 j=j+1
      if (j.gt.it) goto 400
      if (ifsort(1,j) .eq. 0) goto 393
      k1j = shift (ifsort(1,j), -16)
      k2j = shift (shift (ifsort(1,j), 16), -16)
      isyst1 = shift (ifsort(2,j), -16)
      if (isyst1.ne.1) goto 393
      if (k1.ne.k1j.or.k2.ne.k2j) goto 400
      n1=n1+1
      igrp1(1,n1) = shift (shift (ifsort(2,j), 16), -24)
      read (fcetxt(j),416) igrp1(4,n1),igrp1(2,n1),igrp1(3,n1)
      igrp1(5,n1)=0
      igrp1(6,n1)=j
      igrp1(7,n1)=0
      igrp1(8,n1)=0
      if (comcha(10) .eq. 'D') then
         write (dbug,771) j,k1j,k2j,fcetxt(j)(1:80),fcetxt(j)(121:130)
      endif
      goto 393

  394 if (lsw.eq.1.or.lsw.eq.3) then
         lsw=lsw+1
      endif
C
  400 goto isw (410,430)
C
C     FIND NEXT BLOCK IN GROUP 2
C
  410 n2=0
  412 kx = kx + 1
      if (kx.gt.it) go to 421
      if (ifsort(1,kx).eq.0) go to 412
      m1 = shift (ifsort(1,kx), -16)
      m2 = shift (shift(ifsort(1,kx), 16), -16)
      isyst2 = shift (ifsort(2,kx), -16)
      if (isyst2.ne.2) goto 412
      ktype = shift (shift (ifsort(2,kx), 16), -24)
      if (comcha(10) .eq. 'D') then
         write (dbug,772) kx,m1,m2,fcetxt(kx)(1:80),fcetxt(kx)(121:130)
  772    format(' MRGE ',3i5,'  (',a80,a10,')')
      endif
      n2=n2+1
      igrp2(1,n2)=ktype
      read (fcetxt(kx),416) igrp2(4,n2),igrp2(2,n2),igrp2(3,n2)
  416 format(bz, 3x, a3, 25x, a1, i1)
      igrp2(5,n2)=0
      igrp2(6,n2)=kx
      igrp2(7,n2)=0
      igrp2(8,n2)=0
      j=kx
  419 j=j+1
      if (j.gt.it) goto 420
      if (ifsort(1,j).eq.0) goto 419
      m1j = shift (ifsort(1,j), -16)
      m2j = shift (shift (ifsort(1,j), 16), -16)
      isyst2 = shift (ifsort(2,j), -16)
      if (isyst2.ne.2) goto 419
      if (m1.ne.m1j.or.m2.ne.m2j) goto 420
      n2=n2+1
      igrp2(1,n2) = shift (shift (ifsort(2,j), 16), -24)
      read (fcetxt(j),416) igrp2(4,n2),igrp2(2,n2),igrp2(3,n2)
      igrp2(5,n2)=0
      igrp2(6,n2)=j
      igrp2(7,n2)=0
      igrp2(8,n2)=0
      if (comcha(10) .eq. 'D') then
         write (dbug,772) j,m1j,m2j,fcetxt(j)(1:80),fcetxt(j)(121:130)
      endif
      goto 419

  421 if (lsw.eq.1.or.lsw.eq.2) then
         lsw=lsw+2
      endif
  420 goto ksw (430,450,492)
C
  430 goto (432,510,10508,820) lsw
C
C     PERFORM A MERGE-COMPARE BETWEEN GROUPS 1 AND 2
C
  432   if (n1.eq.0.and.n2.gt.0) go to 510
        if (n1.gt.0.and.n2.eq.0) go to 10508
        if (k1-m1) 10508,434,510
  434   if (k2-m2) 10508,436,510
C
C      GROUPS MATCH-DETERMINE DEGREE OF MATCH
C
C      WEIGHT - SIGNIFICANCE
C
C      10   -PARALLELS MATCH (OR BOTH ARE TYPE "R" RECORDS)
C       1   -SECTIONS MATCH
C
C      PHASE1 - FIND BEST MATCH OF GROUP 1 WITH RESPECT TO GROUP 2
C
  436   j = 1
 4361   if ( j .gt. n1 ) goto 465
        max=0
        do 440 l=1,n2
        match(1,l)=0
        if (igrp1(1,j).eq.0) goto 437
        if (igrp2(1,l).eq.0) goto 440
        if (igrp1(2,j).eq.igrp2(2,l)) then
           match(1,l)=match(1,l)+10
        endif
        if (igrp1(3,j).eq.igrp2(3,l)) then
           match(1,l)=match(1,l)+1
        endif
        goto 438
  437   if (igrp2(1,l).eq.0) then
           match(1,l)=match(1,l)+10
        endif
  438   if (max .eq. 0) goto 439
        if (match(1,l).gt.match(1,max)) then
           max=l
        endif
        goto 440
  439   if (match(1,l).gt.0) max = l
  440   continue
C
C       DETERMINE BRANCH MATCH
C
        if (max.eq.0) goto 460
        igrp1(7,j)=match(1,max)
C
C       COMPARE BRANCH J (GROUP1) WITH BRANCH MAX (GROUP2)
C       RESULTS ARE RETURNED IN VARIABLES "I1" AND "I2"
C
        ii=igrp1(6,j)
        jj=igrp2(6,max)
        assign 450 to ksw
        goto 514
 
  450   igrp1(8,j)=i1
        if (comcha(10) .eq. 'D') then
           write (dbug,773) j,max,igrp1(7,j),igrp1(8,j),
     &        fcetxt(ii)(1:80), fcetxt(ii)(121:130)
  773      format(' BASE-MERGE ',4i4,'  (',a80,a10,')')
        endif
  460   continue
        j = j + 1
        goto 4361
  465   continue
C
C       PHASE2 - FIND BEST MATCH OF GROUP 2 WITH RESPECT TO GROUP 1
C
        j = 1
  468   if ( j .gt. n2 ) goto 495
        max=0
        do 490 l=1,n1
        match(2,l)=0
        if (igrp2(1,j).eq.0) goto 470
        if (igrp1(1,l).eq.0) goto 490
        if (igrp2(2,j).eq.igrp1(2,l)) then
           match(2,l)=match(2,l)+10
        endif
        if (igrp2(3,j).eq.igrp1(3,l)) then
           match(2,l)=match(2,l)+1
        endif
        goto 480
  470   if (igrp1(1,l).eq.0) then
           match(2,l)=match(2,l)+10
        endif
  480   if (max.eq.0) goto 482
        if (match(2,l).gt.match(2,max)) then
           max=l
        endif
        goto 490
 
  482   if (match(2,l).gt.0) max = l
  490   continue
C
C       DETERMINE BRANCH MATCH
C
        if (max.eq.0) goto 494
        igrp2(7,j)=match(2,max)
C
C       COMPARE BRANCH J (GROUP2) WITH BRANCH MAX (GROUP1)
C
        ii=igrp1(6,max)
        jj=igrp2(6,j)
        assign 492 to ksw
        goto 514
 
  492   igrp2(8,j)=i2
        if (comcha(10) .eq. 'D') then
           write (dbug,774) max,j,igrp2(7,j),igrp2(8,j),
     &        fcetxt(jj)(1:80),fcetxt(jj)(121:130)
  774      format(' MERGE-BASE ',4i4,'  (',a80,a10,')')
        endif
  494   continue
        j = j + 1
        goto 468
  495   continue
C
C       PAIR MATCHING ITEMS GROUP1 AND GROUP2 FOR PREFERENTIAL QUALIFIER
C
        imax=1
        imin=1
        maxgp1=igrp1(7,imax)*igrp1(8,imax)
        mingp1=maxgp1
        if (n1.eq.1) goto 499
        do 498 i=2,n1
        j=igrp1(7,i)*igrp1(8,i)
        if (j.le.maxgp1) goto 497
        imax=i
        maxgp1=j
  497   if (j.gt.mingp1) goto 498
        imin=i
        mingp1=j
  498   continue
  499   jmax=1
        jmin=1
        maxgp2=igrp2(7,jmax)*igrp2(8,jmax)
        mingp2=maxgp2
        if (n2.eq.1) goto 502
        do 501 i=2,n2
           j=igrp2(7,i)*igrp2(8,i)
           if (j .gt. maxgp2) then
              jmax=i
              maxgp2=j
           endif
           if (j .lt. mingp2) then
              jmin=i
              mingp2=j
           endif
  501   continue
  502   if (maxgp1.gt.maxgp2) goto 505
C
C       GROUP2 IS SELECTED - SET FLAGS
C
        do 503 i=1,n1
  503   match(1,i)=11
        i1=9
        i2=igrp1(8,imin)
        if (iabs(i2).ge.10) then
           i1=8
        endif
        if (iabs(i2).ge.100) then
           i1=7
        endif
        match(1,imin)=i1
        do 504 i=1,n2
  504   match(2,i)=5
        i1=3
        i2=igrp2(8,jmax)
        if (iabs(i2).ge.10) then
           i1=2
        endif
        if (iabs(i2).ge.100) then
           i1=1
        endif
        match(2,jmax)=i1
        goto 508
C
C       GROUP1 IS SELECTED - SET FLAGS
C
  505   do 506 i=1,n1
  506   match(1,i)=5
        i1=3
        i2=igrp1(8,imax)
        if (iabs(i2).ge.10) i1=2
        if (iabs(i2).ge.100) i1=1
        match(1,imax)=i1
        do 507 i=1,n2
  507   match(2,i)=11
        i1=9
        i2=igrp2(8,jmin)
        if (iabs(i2).ge.10) then
           i1=8
        endif
        if (iabs(i2).ge.100) then
           i1=7
        endif
        match(2,jmin)=i1
  508   assign 660 to isw1
        assign 670 to ksw1
        goto 642
C
C       GROUP1 IS SOLITARY
C
10508   i1=4
        if (k1.eq.19999) then
           i1=12
        endif
        if (k2.eq.19999) then
           i1=13
        endif
        do 509 i=1,n1
  509   match(1,i)=i1
        assign 690 to isw1
        goto 642
C
C       GROUP2 IS SOLITARY
C
  510   i1=4
        if (m1.eq.19999) then
           i1=12
        endif
        if (m2.eq.19999) then
           i1=13
        endif
        do 511 i=1,n2
  511   match(2,i)=i1
        assign 700 to ksw1
        goto 660
C
C       DETERMINE INTERFACE PREFERENCE
C
C       INTERFACE WEIGHING FACTORS:
C
C       IFCSW - 100
C       OWNER -  10
C       SYSTEM -  1
C
  514   i1=0
        i2=0
        read (fcetxt(ii),516,err=632) lown1,ifcsw1,ktrps1,
     1    kown1,kown2,isyst1
  516   format(bz, t4, a3, t121, 2i1, 2a3, i2)
        read (fcetxt(jj),516,err=632) lown2,ifcsw2,ktrps2,
     1    kown3,kown4,isyst2
      	if (isyst1.eq.isyst2) go to 610
C
C     	TEST INTERFACE SWITCH
C
      	if (ifcsw1-ifcsw2) 530,540,520
  520   i1=100
        i2=-100
        goto 640
  530   i1=-100
        i2=100
        goto 640
  540   if (ifcsw1.gt.0) goto 550
        goto 640
C
C     	TEST OWNERSHIP
C
  550 	j1 = 0
      	j2 = 0
      	if (lown1.eq.'   ') go to 560
      	if (lown1.eq.kown1.or.lown1.eq.kown2) j1 = 1
      	if (lown1.eq.kown1.and.ktrps1.eq.0) j1 = j1 + 1
      	if (lown1.eq.kown2.and.ktrps1.eq.1) j1 = j1 + 1
  560 	if (lown2.eq.'   ') go to 570
      	if (lown2.eq.kown3.or.lown2.eq.kown4) j2 = 1
      	if (lown2.eq.kown3.and.ktrps2.eq.0) j2 = j2 + 1
      	if (lown2.eq.kown4.and.ktrps2.eq.1) j2 = j2 + 1
  570 	if (j1-j2) 590,600,580
  580 	i1 = 10
      	i2 = -10
      	go to 640
  590 	i1 = -10
      	i2 = 10
      	go to 640
C
C     	TEST SYSTEM PREFERENCE
C
  600 	if (isyst1-isyst2) 620,610,630
  610 	call erexit
      	go to 640
  620 	i1 = 1
      	i2 = -1
      	go to 640
  630 	i1= -1
      	i2= 1
      	goto 640
  632   write (errbuf(1),105)
        write (errbuf(2),636) ii,fcetxt(ii)(1:80),fcetxt(ii)(121:130)
  636   format(' TEXT ',i4,' (',a80,a10,')')
        write (errbuf(3),636) jj,fcetxt(jj),fcetxt(jj)(121:130)
        call prterx ('F',3)
  640   goto ksw (430,450,492)
C
C       PROCESS GROUP1
C
  642   outbuf=' '
        call prtout(1)
        do 652 i=1,n1
        i1=match(1,i)
        k=igrp1(6,i)
        read (fcetxt(k),516,err=651) lown1,ifcsw1,ktrps1,kown1,
     1     kown2,isyst1
        write (outbuf,644) isyst1,fcetxt(k)(1:80),ifcsw1,kown1,
     1     kown2,commnt(i1)(1:39)
  644   format(1x,i1,1x,a80,1x,i1,1x,a3,1x,a3,1x,a39)
        call prtout(1)
        if (i1.gt.0.and.i1.le.6) then
           call brntxt (fcetxt(k))
        endif
        goto 652
  651   write (errbuf(1),105)
        write (errbuf(2),636) k,fcetxt(k)(1:80),fcetxt(k)(121:130)
        call prterx ('F',2)
  652   ifsort(1,k)=0
  656   goto isw1 (660,690)
C
C       PROCESS GROUP2
C
  660   if (lsw.eq.1) goto 662
        outbuf=' '
        call prtout(1)
  662   do 664 i=1,n2
        i1=match(2,i)
        k=igrp2(6,i)
        read (fcetxt(k),516,err=663) lown1,ifcsw1,ktrps1,
     1     kown1,kown2,isyst1
        write (outbuf,644) isyst1,fcetxt(k)(1:80),ifcsw1,
     1     kown1,kown2,commnt(i1)(1:39)
        call prtout(1)
        if (i1.gt.0.and.i1.le.6) then
           call brntxt (fcetxt(k))
        endif
        goto 664
  663   write (errbuf(1),105)
        write (errbuf(2),636) k,fcetxt(k)(1:80),fcetxt(k)(121:130)
        call prterx ('F',2)
  664   ifsort(1,k)=0
  666   goto ksw1 (670,700)
C
C       ADVANCE GROUP1 AND GROUP2
C
  670   assign 410 to isw
        assign 430 to ksw
        goto 360
C
C       ADVANCE GROUP1 ONLY
C
  690   assign 430 to isw
        goto 360
C
C       ADVANCE GROUP2 ONLY
C
  700   assign 430 to ksw
        goto 410
  820   continue
C
C     INTERFACE PROCESSING LOOP COMPLETED
C
      write (outbuf,830)
  830 format ('0 NOTES:')
      call prtout(1)
C
      write (outbuf,834)
  834 format ('0    1. COLUMN "S" IDENTIFIES THE BASE SYSTEM FROM ',
     2        ' WHICH THE INTERFACE BRANCH IMAGE WAS OBTAINED.')
      call prtout(1)
C
      write (outbuf,836)
  836 format ('0    2. SYSTEM "2" INTERFACE IMAGES ARE ORIENTED AS ',
     2     'LISTED- SUBSYSTEM "1" IMAGES ARE TRANSPOSED TO ENHANCE ',
     3     'READABILITY.')
      call prtout(1)
C
      write (outbuf,838)
  838 format ('0    3. A BUS-BRANCH OWNERSHIP MATCH OCCURS IF: ')
      call prtout(1)
C
      write (outbuf,840)
  840 format ('0       A. BUS1 OWNER MATCHES LINE OWNER, THEN SYSTEM',
     2 ' "2" IMAGE IS SELECTED, OR ')
      call prtout(1)
C
      write (outbuf,842)
  842 format ('        B. BUS2 OWNER MATCHES LINE OWNER, THEN SYSTEM',
     2 ' "1" IMAGE IS SELECTED. ')
      call prtout(1)
C
  900 return
      end
