C    %W% %G%
      subroutine cmde2_head()

      include 'ipfinc/parametr.inc'

      include 'ipfinc/cmde_com.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/cgrate.inc'
      include 'ipfinc/cont.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/zbdata.inc'
 
      common /basnam/ basnam
      character basnam*60
 
      character crat*3,  tag1*1, tag2*1, tag3*1, ratc*5,
     1          nretc*2, tag4*1, tag5*1
 
      call forbtm
 
      write (outbuf, 10000) oldcse, dte
10000 format(' Summary of Outage Simulation case ',a,' Dated ',a)
      call shdlod(1)
      outbuf = ' '
      do i = 2,5
        call shdlod(i)
      enddo
      call fortop
 
      call space (2)
 
      write (outbuf, 10010)
10010 format (1x, 132('*'))
      call prtout (1)

      do i = 1,3
        write(outbuf, 10020)
10020   format(' *',t133,'*')
        call prtout (1)
      enddo
 
      write (outbuf, 10030) basnam
10030 format(' *',t18,'OLD BASE FILE :',t50,a,t133,'*')
      call prtout (1)
 
      do i=1,2
        write(outbuf, 10020)
        call prtout (1)
      enddo
 
C     DATA FILES
C     **********
 
      write (outbuf, 10040)
10040 format(' *',t18,'SELECTION OF BRANCH OUTAGES:',t133,'*')
 
      if (nc .eq. 0) then
        write (outbuf(50:132), 10050)
10050   format('ZONES = ALL')
        call prtout(1)
      else
        nc1 = min0 (nc, 20)
        write (outbuf(50:132), 10060) (znc(i),i=1,nc1)
10060   format('ZONES = ',20(a2,1x))
        call prtout(1)
        do nctot = 21, nc, 20
          nc1 = min0 (nctot+19, nc)
          write (outbuf, 10070)
10070     format(' *', t133, '*')
          write (outbuf(58:132), 10080) (znc(i),i=nctot, nc1)
10080     format(20(a2,1x))
          call prtout(1)
        enddo
      endif
 
      write (outbuf, 10090) vcl, vch
10090 format(' *',t50,'BASE KV RANGE ',f6.1,' TO ',f6.1,t133,'*')
      call prtout(1)
 
      do i=1,2
        write(outbuf, 10020)
        call prtout (1)
      enddo
 
      write (outbuf, 10100)
10100 format(' *',t18,'SELECTION OF BRANCH OVERLOADS:',t133,'*')
 
      if (no .eq. 0) then
        write (outbuf(50:132), 10050)
        call prtout(1)
      else
        no1 = min0 (no, 20)
        write (outbuf(50:132), 10060) (zno(i),i=1,no1)
        call prtout(1)
        do notot = 21, no, 20
          no1 = min0 (notot+19, no)
          write (outbuf, 10070)
          write (outbuf(58:132), 10080) (zno(i),i=notot,no1)
          call prtout(1)
        enddo
      endif
 
      write (outbuf, 10090) vol, voh
      call prtout(1)
 
      if (nrat .gt. 0) then
 
        do i=1,2
          write(outbuf, 10020)
          call prtout (1)
        enddo
 
        write (outbuf, 10110)
10110   format(' *',t18,'DEFAULTED OVERLOAD BRANCH RATINGS ',t133,'*')
        call prtout(1)
 
        write(outbuf, 10020)
        call prtout (1)
 
        write (outbuf, 10120)
10120   format(' *',t18,'  BASE KV    BASE KV     RATING ',t133,'*')
        call prtout(1)
 
        write(outbuf, 10020)
        call prtout(1)
 
        do i = 1, nrat
          crat = 'MVA'
          xrat = rat(3,i)
          if (rat(2,i) .eq. 0) crat = 'AMP'
          write (outbuf, 10130) (rat(j,i),j=1,3), crat
10130     format(' *',t18,f5.1,f11.1,f11.1,1x,a3,t133,'*')
          call prtout(1)
        enddo
 
      endif
 
      do i = 1,2
        write(outbuf, 10020)
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
 
      write (outbuf, 10140) tag1
10140 format(' *',t18,'MINIMUM BRANCH RATINGS:',t46,'(',a1,
     1       ') ACTUAL OR DEFAULT RATINGS ',t133,'*')
      call prtout(1)
 
      write (outbuf, 10150)
10150 format(' *',t46,'    LEGEND: "N" = NOMINAL, "T" = THERMAL, ',
     1       '"E" = EMERGENCY, "B" = BOTTLENECK',t133,'*')
      call prtout(1)
 
      write (outbuf, 10160) tag2, case1(17)
10160 format(' *',t46,'(',a1,
     1   ') EXTEND RATINGS IF OVERLOADED TO BASE LOADING PLUS',f5.0,
     2   ' PERCENT',t133,'*')
      call prtout(1)
 
      write (outbuf, 10170) tps
10170 format(' *',t42,'(',f5.3,') PRESCREEN RATING REDUCTION (P.U.) ',
     1   t133,'*')
      call prtout (1)
 
      write (outbuf, 10180) tpc
10180 format(' *',t42,'(',f5.3,') MIMINUM THRESHOLD RATINGS (P.U.) ',
     1   t133,'*')
      call prtout(1)
 
      do i = 1,2
        write(outbuf, 10020)
        call prtout (1)
      enddo
 
      tag1 = 'X'
      tag2 = 'X'
      if (kase1(3) .eq. 1) tag2 = ' '
      if (kase1(3) .eq. 0) tag1 = ' '
 
      do i = 1,2
        write(outbuf, 10020)
        call prtout (1)
      enddo
 
      write (outbuf, 10190) tag1
10190 format(' *',t18,'OUTPUT SUMMARY REPORTS:',t46,'(',a1,
     1   ') OUTAGE - OVERLOADS',t133,'*')
      call prtout(1)
 
      write (outbuf, 10200) tag2
10200 format(' *',t46,'(',a1,') OVERLOAD - OUTAGES ',t133,'*')
      call prtout(1)
 
      if (kase1(5) .eq. 0) then
         tag1 = 'X'
         tag2 = ' '
      else
         tag1 = ' '
         tag2 = 'X'
      endif
 
      write (outbuf, 10210) tag1
10210 format(' *',t46,'(',a1,') REPORTS SORTED BUS - OWNERSHIP ',
     1   t133,'*')
      call prtout(1)
 
      write (outbuf, 10220) tag2
10220 format(' *',t46,'(',a1,') REPORTS SORTED OWNERSHIP - BUS ',
     1   t133,'*')
      call prtout(1)
 
      do i = 1,2
        write(outbuf, 10020)
        call prtout (1)
      enddo
 
      write (outbuf, 10010)
      call prtout (1)
 
      return
      end
