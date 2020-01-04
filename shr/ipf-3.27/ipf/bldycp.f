C    @(#)bldycp.f	20.3 2/13/96
      subroutine bldycp (kbr,error)
      integer kbr, error
C
C        This subroutine builds Variable Series Compsensation RZ "YCOMP"
C        array from scratch. Array "BRNCH" must be linked with "BUSDTA"
C        before this subroutine may be called.
C
C        KBR : index of RZ record in KBRNCH array
C        ERROR : 0/1 = No error/error encountered in BLDYCP
C
      include 'ipfinc/parametr.inc'

      include 'ipfinc/blank.inc'
c	Global variables used:
c		bmva, ltot
      include 'ipfinc/branch.inc'
c	Global variables used:
c		kbrnch, brnch
      include 'ipfinc/bus.inc'
c	Global variables used:
c		bus, base
      include 'ipfinc/lfiles.inc'
c	Global variables used:
c		dbg
      include 'ipfinc/prt.inc'
c	Global variables used:
c		errbuf
      include 'ipfinc/ycomp.inc'
c	Global variables used:
c		ycomp, kycomp, nycomp
 
 
C
      complex * 16 y(2,2)
c
      character xbuf * 120, id * 1, nxid * 1
c
c     integer
c
      real imax, ibase
c
      complex yeq(2,2), y1(2,2), y2(2,2), yxy(2,2)
C
C        Initialize counters/switched
C
      error = 0
      kbsdta(16,ntot+1) = ltot + 1
 
C
      k1 = kbrnch(2,kbr)
      k2 = kbrnch(12,kbr)
      call getchr (1,id,kbrnch(13,kbr))
      ksect = kbrnch(14,kbr)
C
C        Search for duplicate entities
C
      do 100 i = 1, nycomp
         if (min0 (k1,k2) .ne.min0 (kycomp(1,i),kycomp(2,i))) go to 100
         if (max0 (k1,k2) .ne.max0 (kycomp(1,i),kycomp(2,i))) go to 100
         if (kycomp(3,i) .ne. ichar (id)) go to 100
         if (kycomp(4,i) .ne. ksect) go to 100
         jycomp = i
         go to 140
C
  100 continue
C
      nycomp = nycomp + 1
      if (nycomp .gt. MXYCMP) then
         write (errbuf(1),110) MXYCMP
  110    format(' MORE THAN ',i4,
     1   ' Series-Compensated RZ Records. Remaining records ignored:')
         errbuf(2) = ' '
         call bcdbrn (kbr,xbuf)
         write (errbuf(3),120) xbuf(1:80)
  120    format(13x,'(',a80,')')
         call prterx ('W',3)
         error = 1
         return
      endif
      jycomp = nycomp
C
      do 130 i = 1, 48
  130 kycomp(i,jycomp) = 0
 
      kycomp(1,jycomp) = k1
      kycomp(2,jycomp) = k2
      kycomp(3,jycomp) = ichar (id)
      kycomp(4,jycomp) = ksect
C
  140 continue
 
      ycomp(5,jycomp) = brnch(6,kbr) / bmva
      ycomp(6,jycomp) = brnch(5,kbr) / bmva
      ycomp(34,jycomp) = brnch(9,kbr)
      ycomp(35,jycomp) = brnch(8,kbr)
      ycomp(38,jycomp) = brnch(18,kbr)
      ycomp(39,jycomp) = brnch(10,kbr)
      ibase = 1000.0 * bmva / (sqrt(3.0) * base(k1))
C
C     Get I rating from RZ record. If zero, obtain I rating from
C     accompanying "E" record.
C
      if (brnch(7,kbr) .gt. 0.0) then
         ycomp(40,jycomp) = brnch(7,kbr) / ibase
      else
         ycomp(40,jycomp) = brnch(4,kbr+1) / ibase
      endif
      kycomp(41,jycomp) = brnch(4,kbr)
C
C        Check for accompanying Line Record
C
      call getchr (1,nxid,kbrnch(13,kbr+1))
      if (kbrnch(2,kbr+1) .ne. k1 .or. kbrnch(12,kbr+1) .ne. k2 .or.
     1   nxid .ne. id .or. kbrnch(14,kbr+1) .ne. ksect) then
         write (errbuf(1),150)
  150    format(' "RZ" record is not matched with associated ',
     1          'line record.')
         errbuf(2) = ' '
         call bcdbrn (kbr,xbuf)
         write (errbuf(3),120) xbuf(1:80)
         call bcdbrn (kbr+1,xbuf)
         write (errbuf(4),120) xbuf(1:80)
         call prterx ('E',4)
         error = 1
         return
      endif
C
C        Check for Series-compensated branch (No Rij or Gis)
C
      call pieqiv (kbr+1,y,ierr)
      gis = sngl( dreal (y(1,1) + y(1,2)) )
      bis = sngl( dimag (y(1,1) + y(1,2)) )
      gjs = sngl( dreal (y(2,2) + y(2,1)) )
      bjs = sngl( dimag (y(2,2) + y(2,1)) )
      rij = sngl( dreal (dcmplx(-1.0d0,0.0d0)/y(1,2)) )
      xij = sngl( dimag (dcmplx(-1.0d0,0.0d0)/y(1,2)) )
      ycomp(32,jycomp) = xij
      ycomp(33,jycomp) = xij
      ycomp(36,jycomp) = bis
      ycomp(37,jycomp) = bis
      if (abs (rij) .gt. 0.001 * abs (xij) .or.
     1    abs (gis) .gt. 0.001 * abs(bis)) then
         write (errbuf(1),170) rij, gis
  170    format(' Series Compensated "RZ" branch must be ',
     &          'pure susceptance (Rij = ',e10.3,' Gis = ',e10.3,'.')
         errbuf(2) = ' '
         call bcdbrn (kbr+1,xbuf)
         write (errbuf(3),120) xbuf(1:80)
         call prterx ('E',3)
         error = 1
      endif
      if (abs (gjs) .gt. 0.001 .or. abs(bjs) .gt. 0.001) then
         write (errbuf(1),180) gjs, bjs
  180    format(' Series Compensated "RZ" branch cannot accept ',
     &          'non-zero G2 + jB2 (',e12.5,',',e12.5,').')
         errbuf(2) = ' '
         call bcdbrn (kbr,xbuf)
         write (errbuf(3),120) xbuf(1:80)
         call bcdbrn (kbr+1,xbuf)
         write (errbuf(4),120) xbuf(1:80)
         call prterx ('E',4)
         error = 1
      endif
C
C        Check Type = 1,2, or 3
C
      if (kycomp(41,jycomp) .lt. 1 .or. kycomp(41,jycomp) .gt. 3) then
         write (errbuf(1),190) kycomp(41,jycomp)
  190    format(' Illegal series compensated "RZ" branch type (',i2,
     1          '). Acceptable types are 1,2, or 3.')
         errbuf(2) = ' '
         call bcdbrn (kbr,xbuf)
         write (errbuf(3),120) xbuf(1:80)
         call bcdbrn (kbr+1,xbuf)
         write (errbuf(4),120) xbuf(1:80)
         call prterx ('E',4)
         error = 1
      endif
      if (ycomp(38,jycomp) .lt. ycomp(39,jycomp)) then
         if (abs (gis) .gt. 0.001 .or. abs(bis) .gt. 0.001) then
            write (errbuf(1),192) kycomp(41,jycomp), gis, bis
  192       format(' Series Compensated "RZ" branch type (',a,
     &             ') cannot accept non-zero G1 + jB1 (',
     &              e12.5,',',e12.5,').')
            errbuf(2) = ' '
            call bcdbrn (kbr,xbuf)
            write (errbuf(3),120) xbuf(1:80)
            call bcdbrn (kbr+1,xbuf)
            write (errbuf(4),120) xbuf(1:80)
            call prterx ('E',4)
            error = 1
         endif
      else
         if (abs (gis) .gt. 0.001) then
            write (errbuf(1),194) kycomp(41,jycomp), gis
  194       format(' Series Compensated "RZ" branch type (',a,
     &             ') cannot accept non-zero G1 (',e12.5,').')
            errbuf(2) = ' '
            call bcdbrn (kbr,xbuf)
            write (errbuf(3),120) xbuf(1:80)
            call bcdbrn (kbr+1,xbuf)
            write (errbuf(4),120) xbuf(1:80)
            call prterx ('E',4)
            error = 1
         endif
      endif
C
C        Check Pc_min < Pc_max
C
      if (brnch(6,kbr) .gt. brnch(5,kbr)) then
         write (errbuf(1),200) brnch(5,kbr), brnch(6,kbr)
  200    format(' Series compensated "RZ" branch must have P_max (',
     1            f6.0,') => P_min (',f6.0,').')
         errbuf(2) = ' '
         call bcdbrn (kbr,xbuf)
         write (errbuf(3),120) xbuf(1:80)
         call bcdbrn (kbr+1,xbuf)
         write (errbuf(4),120) xbuf(1:80)
         call prterx ('E',4)
         error = 1
      endif
C
C        Check Xij(min) < Xij < Xij(max)
C
      if (ycomp(34,jycomp) .gt. xij .or. xij .gt. ycomp(35,jycomp))
     1   then
         write (errbuf(1),210) ycomp(34,jycomp), xij, ycomp(35,jycomp)
  210    format(' Improper Xij limits or initial values: Xij_min (',
     1           f7.5,') < Xij (',f7.5,') < Xij_max (',f7.5,').')
         errbuf(2) = ' '
         call bcdbrn (kbr,xbuf)
         write (errbuf(3),120) xbuf(1:80)
         call bcdbrn (kbr+1,xbuf)
         write (errbuf(4),120) xbuf(1:80)
         call prterx ('E',4)
         error = 1
      endif
C
C        Check Bis_min <= Bis <= Bis_max if Type = 3
C
      if (kycomp(41,jycomp) .eq. 3) then
         if (ycomp(38,jycomp) .gt. bis .or. bis .gt. ycomp(39,jycomp))
     1      then
            write (errbuf(1),220) ycomp(38,jycomp), bis,
     1         ycomp(39,jycomp)
  220       format(' Improper Bis limits or initial values: Bis_min(',
     1         f7.5,') < Bis (',f7.5,') < Bis_max (',f7.5,').')
            errbuf(2) = ' '
            call bcdbrn (kbr,xbuf)
            write (errbuf(3),120) xbuf(1:80)
            call bcdbrn (kbr+1,xbuf)
            write (errbuf(4),120) xbuf(1:80)
            call prterx ('E',4)
            error = 1
         endif
      else
         if (ycomp(38,jycomp) .lt. ycomp(39,jycomp)) then
            write (errbuf(1),230) kycomp(41,jycomp), ycomp(38,jycomp)
     1         ,bis,ycomp(39,jycomp)
  230       format(' Bis limits disabled for type (',i1,'). Bis_min(',
     1               f7.5,') < Bis (',f7.5,') < Bis_max (',f7.5,').')
            ycomp(38,jycomp) = 0.0
            ycomp(39,jycomp) = 0.0
            errbuf(2) = ' '
            call bcdbrn (kbr,xbuf)
            write (errbuf(3),120) xbuf(1:80)
            call bcdbrn (kbr+1,xbuf)
            write (errbuf(4),120) xbuf(1:80)
            call prterx ('E',4)
            error = 1
         endif
      endif
C
C        Check Irate => 0
C
      if (ycomp(40,jycomp) .lt. 0.0) then
         write (errbuf(1),240) brnch(7,kbr)
  240    format('  Rated I (',f6.0,' amps) must be => 0.0.')
         errbuf(2) = ' '
         call bcdbrn (kbr,xbuf)
         write (errbuf(3),120) xbuf(1:80)
         call bcdbrn (kbr+1,xbuf)
         write (errbuf(4),120) xbuf(1:80)
         call prterx ('E',4)
         error = 1
C
C        Check Irate => ABS (P_limit/V_limit). (Note: Per-unit values
C        are used. V_limit is approximated as 1.0)
C
      else if (ycomp(40,jycomp) .gt. 0.0) then
         imax = amax1 (abs(ycomp(5,jycomp)),abs(ycomp(6,jycomp)))
         if (imax .gt. ycomp(40,jycomp)) then
            write (errbuf(1),250) imax * ibase, ycomp(40,jycomp) * ibase
  250       format(' Estimated I (',f7.1,' amps) carrying scheduled ',
     &             'power > rated I (',f6.0,' amps).')
            errbuf(2) = ' '
            call bcdbrn (kbr,xbuf)
            write (errbuf(3),120) xbuf(1:80)
            call bcdbrn (kbr+1,xbuf)
            write (errbuf(4),120) xbuf(1:80)
            call prterx ('E',4)
            error = 1
         endif
      endif
C
C        Compute following 2-port Y-matrices:
C
C        YEQ - Equivalent parallel 2-port
C        Y1  - 2-port left of section KSECT
C        YXY - 2-port for section KSECT
C        Y2  - 2-port right of section KSECT
C
      call getyeq (k1,k2,id,ksect,yeq,y1,yxy,y2)
 
      k = 8
      do 290 i = 1, 2
         do 290 j = 1, 2
            ycomp(k,jycomp) = real (yeq(i,j))
            ycomp(k+1,jycomp) = aimag (yeq(i,j))
  290 k = k + 2
 
      k = 16
      do 320 i = 1, 2
         do 320 j = 1, 2
            ycomp(k,jycomp) = real (y1(i,j))
            ycomp(k+1,jycomp) = aimag (y1(i,j))
  320 k = k + 2
      k = 24
      do 350 i = 1, 2
         do 350 j = 1, 2
            ycomp(k,jycomp) = real (y2(i,j))
            ycomp(k+1,jycomp) = aimag (y2(i,j))
  350 k = k + 2
C
C        Debug dump
C
      write (dbug,360) bus(k1), base(k1), bus(k2), base(k2), id,
     1   ksect,(i,ycomp(i,jycomp),i=5,48)
  360 format ('0 YCOMP ',a8,f6.1,1x,a8,f6.1,1x,a1,1x,i1,/,
     1   8(1x,i2,1x,e12.5))
 
      return
      end
 
