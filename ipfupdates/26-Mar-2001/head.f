C    %W% %G%
      subroutine head

      include 'ipfinc/parametr.inc'

      include 'ipfinc/apcom.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/cgrate.inc'
      include 'ipfinc/cont.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/zbdata.inc'
 
      common /basnam/ basnam
      character basnam * 60
 
      character crat * 3,  tag1 * 1, tag2 * 1, tag3 * 1, ratc * 5,
     1          nretc * 2, tag4 * 1, tag5 * 1
 
      call forbtm
 
      write (outbuf,80) oldcse, dte
   80 format(' Summary of Outage Simulation case ',a,' Dated ',a)
      call shdlod(1)
      outbuf = ' '
      do i = 2,5
        call shdlod(i)
      enddo
      call fortop
 
      call space (2)
 
      write (outbuf,91)
   91 format (1x,132('*'))
      call prtout (1)
      do i = 1, 3
         write(outbuf,92)
   92    format(' *',t133,'*')
         call prtout (1)
      enddo
 
      write (outbuf,94) basnam
   94 format(' *',t18,'OLD BASE FILE :',t50,a,t133,'*')
      call prtout (1)
 
      do i = 1, 2
         write(outbuf,92)
         call prtout (1)
      enddo
 
C     DATA FILES
C     **********
 
      write (outbuf,110)
  110 format(' *',t18,'SELECTION OF BRANCH OUTAGES:',t133,'*')
 
      if (nc .eq. 0) then
         write (outbuf(50:132),120)
  120    format('ZONES = ALL')
         call prtout(1)
      else
         nc1 = min0 (20, nc)
         write (outbuf(50:),130) (znc(i),i=1, nc1)
  130    format('ZONES = ',20(a2,1x))
         call prtout(1)
         do nctot = 21, nc, 20
            nc1 = min0 (nctot+19, nc)
            write (outbuf,132)
  132       format(' *', t133, '*')
            write (outbuf(58:117),134) (znc(i),i=nctot, nc1)
  134       format(20(a2,1x))
            call prtout(1)
         enddo
      endif
 
      write (outbuf,140) vcl, vch
  140 format(' *',t50,'BASE KV RANGE ',f6.1,' TO ',f6.1,t133,'*')
      call prtout(1)
 
      do i=1,2
         write(outbuf,92)
         call prtout (1)
      enddo
 
      write (outbuf,150)
  150 format(' *',t18,'SELECTION OF BRANCH OVERLOADS:',t133,'*')
 
      if (no .eq. 0) then
         write (outbuf(50:132),120)
         call prtout(1)
      else
         no1 = min0 (20, no)
         write (outbuf(50:),130) (zno(i),i=1, no1)
         call prtout(1)
         do notot = 21, no, 20
            no1 = min0 (notot+19, no)
            write (outbuf,132)
            write (outbuf(58:117),134) (zno(i),i=notot, no1)
            call prtout(1)
         enddo
      endif
 
      write (outbuf,140) vol, voh
      call prtout(1)
 
      if (num_network .gt. 0) then
        write (outbuf, 10110)
10110   format(' *',t18,'SELECTION OF NETWORK:',t133,'*')
 
         nc1 = min0 (20, num_network)
         write (outbuf(50:), 130) (zn_network(i),i=1, nc1)
         call prtout(1)
         do nctot = 21, num_network, 20
            nc1 = min0 (nctot+19, num_network)
            write (outbuf,132)
            write (outbuf(58:117), 134) (zn_network(i),i=nctot, nc1)
            call prtout(1)
         enddo
         write (outbuf,140) vl_network, vh_network
         call prtout(1)
 
         do i=1,2
            write(outbuf,92)
            call prtout (1)
         enddo

      endif

      if (nrat .gt. 0) then
 
         do i=1,2
            write(outbuf,92)
            call prtout (1)
         enddo
 
         write (outbuf,160)
  160    format(' *',t18,'DEFAULTED OVERLOAD BRANCH RATINGS ',t133,'*')
         call prtout(1)
 
         write(outbuf,92)
         call prtout (1)
 
         write (outbuf,170)
  170    format(' *',t18,'  BASE KV    BASE KV     RATING ',t133,'*')
         call prtout(1)
 
         write(outbuf,92)
         call prtout(1)
 
         do i = 1, nrat
            crat = 'MVA'
            xrat = rat(3,i)
            if (rat(2,i) .eq. 0) crat = 'AMP'
            write (outbuf,180) (rat(j,i),j=1,3), crat
  180       format(' *',t18,f5.1,f11.1,f11.1,1x,a3,t133,'*')
            call prtout(1)
         enddo
 
      endif
 
      do i = 1,2
         write(outbuf,92)
         call prtout (1)
      enddo
 
      if (kase1(6) .eq. 0) then
         tag1 = 'X'
         tag2 = 'X'
         tag3 = ' '
      else
         tag1 = 'X'
         tag2 = ' '
         tag3 = ' '
      endif
 
      write (outbuf,230) tag1
  230 format(' *',t18,'MINIMUM BRANCH RATINGS:',t46,'(',a1,
     1       ') ACTUAL OR DEFAULT RATINGS ',t133,'*')
      call prtout(1)
 
      write (outbuf,232)
  232 format(' *',t46,'    LEGEND: "N" = NOMINAL, "T" = THERMAL, ',
     1       '"E" = EMERGENCY, "B" = BOTTLENECK',t133,'*')
      call prtout(1)
 
      write (outbuf,240) tag2, ratc
  240 format(' *',t46,'(',a1,
     1   ') EXTEND RATINGS IF OVERLOADED TO BASE LOADING PLUS',a5,
     2   ' PERCENT',t133,'*')
      call prtout(1)
 
      write (outbuf,250) tps
  250 format(' *',t42,'(',f5.3,') PRESCREEN RATING REDUCTION (P.U.) ',
     1   t133,'*')
      call prtout (1)
 
      write (outbuf,260) tpc
  260 format(' *',t42,'(',f5.3,') MIMINUM THRESHOLD RATINGS (P.U.) ',
     1   t133,'*')
      call prtout(1)
 
      do i=1,2
         write(outbuf,92)
         call prtout (1)
      enddo
 
      tag1 = ' '
      tag2 = ' '
      tag3 = ' '
      tag4 = ' '
      tag5 = ' '
 
      if (kase1(21) .eq. 0) then
         if (chase1(21) .eq. ' ') then
            tag3 = 'X'
         else
            tag1 = 'X'
         endif
      else
         tag3 = 'X'
      endif
 
  320 nret = kase1(10)
      if (nret .eq. 0 .and. kase1(22) .eq. 0 .and. irect .ne. 1)
     1   nret = 2
      write (nretc,330) nret
  330 format(i2)
      x = case1(31)
      if (kase1(22) .ne. 0) tag4 = 'X'
      if (kase1(24) .ne. 0) tag5 = 'X'
 
      write (outbuf,340) tag1
  340 format(' *',t18,'REDUCTION SCHEME:',t46,'(',a1,') NO REDUCTION',
     1  t133,'*')
      call prtout(1)
 
      write (outbuf,350) nretc
  350 format(' *',t45,'(',a,') EXPANSION OF BUFFER ZONE ',
     &       'FOR ESSENTIAL NETWORK',t133,'*')
      call prtout(1)
 
      write (outbuf,360) tag3
  360 format(' *',t46,'(',a1,') OPTIMALLY DETERMINED ',
     1       'EQUIVALENT NETWORK',t133,'*')
      call prtout(1)
 
      write (outbuf,370) tag4
  370 format(' *',t46,'(',a1,
     1') retain "rei" equivalent of eliminated generators',t133,'*')
      call prtout(1)
 
      write (outbuf,380) tag5
  380 format(' *',t46,'(',a1,') PRESERVE R/X RATIO OF ',
     1       'ELIMINATED BRANCHES',t133,'*')
      call prtout(1)
 
      write (outbuf,390) x
  390 format(' *',t41,'(',f6.3,') MIMIMUM ADMITTANCE OF ',
     1       'EQUIVALENT BRANCHES (P.U.)',t133,'*')
      call prtout(1)
 
      do i=1,2
         write(outbuf,92)
         call prtout (1)
      enddo
 
      tag1 = 'X'
      tag2 = 'X'
      if (irect .ne. 0) tag2 = ' '
      cx1 = tol
      cx2 = psm
      cx4 = vcrit
 
      write (outbuf,400) tag1
  400 format(' *',t18,'SOLUTION PARAMETERS:',t46,'(',a1,
     1   ') REAL POWER CONSTRAINED',t133,'*')
      call prtout(1)
 
      write (outbuf,410) tag2
  410 format(' *',t46,'(',a1,') REACTIVE POWER CONSTRAINED',t133,'*')
      call prtout(1)
 
      if (kase1(1) .eq. 0) then
         tag1 = 'X'
         tag2 = ' '
      else
         tag1 = ' '
         tag2 = 'X'
      endif
 
      write (outbuf,412) tag1
  412 format(' *',t46,'(',a1,
     1') ltc phase shifters represented as constant terminal injection '
     2,t133,'*')
      call prtout(1)
 
      write (outbuf,414) tag2
  414 format(' *',t46,'(',a1,
     1   ') ltc phase shifters represented as constant phase angle',
     2   t133,'*')
      call prtout(1)
 
      write (outbuf,420) npass
  420 format(' *',t44,'(',i3,') MAXIMUM ITERATIONS ',t133,'*')
      call prtout(1)
 
      write (outbuf,430) cx1
  430 format(' *',t41,'(',f6.4,') CONVERGENCE TOLERANCE (P.U.)',
     1   t133,'*')
      call prtout(1)
 
      write (outbuf,440) cx2
  440 format(' *',t41,'(',f6.2,') REAL POWER DIVERGENCE ',
     1       'TOLERANCE (RADIANS)',t133,'*')
      call prtout(1)
 
      write (outbuf,442) cx4
  442 format(' *',t41,'(',f6.2,') REACTIVE POWER DIVERGENCE ',
     1       'TOLERANCE (P.U. VOLTAGE)',t133,'*')
      call prtout(1)
 
      tag1 = 'X'
      tag2 = 'X'
      if (kase1(3) .eq. 1) tag2 = ' '
      if (kase1(3) .eq. 0) tag1 = ' '
 
      do i=1,2
         write(outbuf,92)
         call prtout (1)
      enddo
 
      write (outbuf,450) tag1
  450 format(' *',t18,'OUTPUT SUMMARY REPORTS:',t46,'(',a1,
     1   ') OUTAGE - OVERLOADS',t133,'*')
      call prtout(1)
 
      write (outbuf,460) tag2
  460 format(' *',t46,'(',a1,') OVERLOAD - OUTAGES ',t133,'*')
      call prtout(1)
 
      if (kase1(5) .eq. 0) then
         tag1 = 'X'
         tag2 = ' '
      else
         tag1 = ' '
         tag2 = 'X'
      endif
 
      write (outbuf,470) tag1
  470 format(' *',t46,'(',a1,') REPORTS SORTED BUS - OWNERSHIP ',
     1   t133,'*')
      call prtout(1)
 
      write (outbuf,480) tag2
  480 format(' *',t46,'(',a1,') REPORTS SORTED OWNERSHIP - BUS ',
     1   t133,'*')
      call prtout(1)
 
      do i=1,2
         write(outbuf,92)
         call prtout (1)
      enddo
 
      write (outbuf,91)
      call prtout (1)
 
      return
      end
