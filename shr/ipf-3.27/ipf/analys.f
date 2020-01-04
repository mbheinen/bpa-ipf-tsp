C    @(#)analys.f	20.6 11/12/98
      subroutine analys
 
C     POWERFLOW OUTPUT ANALYSIS
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/agc.inc'
      include 'ipfinc/alpha2.inc'
      include 'ipfinc/anlys.inc'
      include 'ipfinc/arcntl.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/branch.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/busanl.inc'
      include 'ipfinc/com010.inc'
      include 'ipfinc/ecvar.inc'
      include 'ipfinc/header.inc'
      include 'ipfinc/lndpcp.inc'
      include 'ipfinc/losanl.inc'
      include 'ipfinc/oldbus.inc'
      include 'ipfinc/optim.inc'
      include 'ipfinc/owncom.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/qksrt.inc'
      include 'ipfinc/slnopt.inc'
      include 'ipfinc/svc.inc'
      include 'ipfinc/usranl.inc'
      include 'ipfinc/zonlst.inc'
 
      common /neggen/ neggen
      real neggen
 
      common /sortsw/ sortsw, vltsrt(MAXBUS)
      integer sortsw, vltsrt, first
 
      common /ownflg/ ownflg
      logical ownflg
 
      dimension systot(10), subtot(16), subttl(14), arate(4),
     &          varate(4), apctc(4), vapctc(4), subto2(10)
      integer fichsx, fchsx1, fldsiz, oldbsh
      character xx1*1, xx2*1, xx3*1, xx4*1, totalc*8, core*8, cu*8, 
     &          tot*8, corels*8, culoss*8,  kowner*3, zn*2, zn1*2, 
     &          zn2*2, j1*1, j2*1, j3*1, own*3, kown1*3, ljstfy*3, 
     &          id*1, type*1, apctc*7, vapctc*7, ownbf1*40,
     &          ownbf2*40, oldown*3, relvlt*5
 
      external kompan, swapan, kmpan2, swpan2, kompef, swapef, kotxef,
     &         swtxef, kpovll, spovll, kpovlt, spovlt, kpovuv, spovuv  

      integer ptr, qptr 
 
      call forbtm
 
C               SAVE LINE PRINT AND FICHE SWITCHES
 
        lprtsv = lprtsw
        fichsx = fichsw
 
C               SET PRINT AND FICHE TO REQUESTED LEVELS
 
        lprtsw = kspare(8)
        fichsw = kspare(9)
 
C       CHECK FOR FAILED SOLUTION
C               LSKP = 0 :  Initial value before any solution
C                      1 :  Solved OK
C                      2 :  un-assigned setting
C                      3 :  Diverged  -  NO SOLUTION
 
        if ( lskp .eq. 1 .or. lskp .eq. 2 ) then
           outbuf = ' * * * * OUTPUT ANALYSIS LISTING * * * *'
        else
           outbuf = ' >>* FAILED SOLUTION ANALYSIS LISTING <<'
        endif
 
        call rpnlod
        call fortop
 
      jowner = min0(jowner,MAXOWN)
      ncon = min0(ncon,MAXLOD)
      jvolt = min0(jvolt,MAXVLT)
      novls = min0(novls,MAXOVL)
      noltr = min0(noltr,MAXOLT)
      nolbr = min0(nolbr,MAXOLB)
      ncomp = min0(ncomp,MAXCMP)
 
      do namtot = 2,MAX_OWNERS
        if (owners(namtot)(1:3).eq.'   ') go to 120
        if (kompr(owners(namtot-1)(1:3),owners(namtot)(1:3),junk)
     &      .ge. 0) then
          write(errbuf(1)(1:120),100) owners(namtot)(1:3),
     1                                owners(namtot)(8:60)
100       format('0 OWNERSHIP NOT IN SORT ORDER (',a,')-',a)
          call prterx ('W',1)
        endif
      enddo
      namtot = MAX_OWNERS + 1
 
120   namtot = namtot - 1
 
      do i = 1,jowner
        lstown(i) = i
      enddo
 
      if (jowner.ne.1) then
        call qiksrt(1,jowner,kompan,swapan)
      endif
 
      do i = 1,jvolt
         lstvlt(i) = i
      enddo
 
      if (jvolt.ne.1) then
        call qiksrt(1,jvolt,kmpan2,swpan2 )
      endif
 
C      SET UP THE OPTIONS FOR PRINT/FICHE
 
        if(fichsw.gt.0) then
           write (outbuf,140)
140        format('$MFCB     ANALYS' )
           call pfomf(1)
        endif
C
C       ANALYSIS OPTION SUMMARY
C
        write (outbuf,15)
   15   format (1x,132('*'))
        call prtout (1)
        do i = 1, 2
           write(outbuf,52)
   52      format(' *',t133,'*')
           call prtout (1)
        enddo
 
        write (outbuf,144)
  144   format (' *',t10,'LEGEND:    F = FULL SUMMARY ',t133,'*')
        call prtout (1)
        write (outbuf,145)
  145   format (' *',t10,'           Z = RESTRICTED BY ZONES ',
     1               t133,'*')
        call prtout (1)
        write (outbuf,146)
  146   format (' *',t10,'           O = RESTRICTED BY OWNERS ',
     1               t133,'*')
        call prtout (1)
        write (outbuf,147)
  147   format (' *',t10,'           B = RESTRICTED BY BOTH ',
     1               t133,'*')
        call prtout (1)
        write (outbuf,148)
  148   format (' *',t10,'       BLANK = NO SUMMARY REQUESTED',
     1               t133,'*')
        call prtout (1)
 
        do i = 1,2
            write(outbuf,52)
            call prtout (1)
        enddo
 
        write (outbuf,150)
  150   format (' *',t27,'PAPER',t36,'FICHE',t50,'SUMMARY NAME',
     1               t133,'*')
        call prtout (1)
 
        write (outbuf,151)
  151   format (' *',t27,'-----',t36,'-----',t50,'-------  ----',
     1               t133,'*')
        call prtout (1)
 
C       LEVEL   INDEX  DESCRIPTION
C
C         1      21    User-defined analysis listing
C         1       1    Buses With Unscheduled Reactive
C         2       2    Total System Generations and Loads by
C                         Owner
C         2       3    System Generations, Loads, Losses and
C                         Shunts by Zones
C         2       4    Undervoltage-Overvoltage Buses
C         2       5    Transmission Lines Loaded Above xxx.x%
C                         of Ratings
C         2       6    Transformers Loaded Above xxx.x% of
C                         Ratings
C         2       7    Transformers Excited Above xxx.x% over Tap
C         2       8    Transmission System Losses
C         2       9    BPA Industrial Loads
C         2      10    DC System
C         2      11    Shunt Reactive Summary
C         2      12    Summary of LTC Transformers
C         2      12    Summary of LTC Reactive Utilization
C         2      13    Summary of Phase-shifters
C         2      14    Summary of %Var-controlled buses
C         2      14    Summary of AGC Control
C         2      14    Summary of Line Drop Compensation
C         2      14    Summary of SVC buses
C         2      15    Summary of Type BX buses
C         2      16    Summary of Adjustable Var compensation
C         2      17    Transmission Lines Containing Series Compensation
C         3      18    Bus Quantities
C         4      19    Spinning Reserves
C         4      20    Transmission Line Efficiency Analysis
C                         (Lines Loaded Above xxx.x % of Nominal Ratings
C         4      22    Transformer Total Losses Above xx.xx %
C                         of Nominal Rating.)
C         4      23    Transformer Core Losses Above xx.xx % of
C                         Nominal Ratings.
C
C ***   LEVEL 1 ANALYSIS SUMMARIES
 
        if (numusr .gt. 0) then
 
           call ifanal ('P', 21, 0, kspare(8), xx1, lprsx1)
           call ifanal ('F', 21, 0, kspare(9), xx2, fchsx1)
 
           write (outbuf,10151) xx1,xx2
10151      format (' *',t29,'(',a,')',t38,'(',a,')',
     1                  t50,'User-defined analysis', t133,'*')
C ***                                                                  *
C ***      Use UA sequential access input file already opened.         *
C ***                                                                  *
           rewind lunusr
           do 20190 iuser = 1, numusr
 
              max1 = numdef(iuser)
              do 20172 first  = 1, max1, 200
                 last = min0 (first+199,max1)
                 read (lunusr) (usrdef(i),i=first,last)
20172         continue
              max2 = numtxt(iuser)
              do 20174 first = 1, max2, 200
                 last = min0 (first+199,max2)
                 read (lunusr) (usrtxt(i),i=first,last)
20174         continue
 
              do 20180 i = 1, numtxt(iuser)
              if (usrtxt(i)(1:1) .eq. 'H') then
C
C                Append user-assigned name to report.
C
                 l = lastch (outbuf(1:132))
                 outbuf(l+1:132) = ': ' // usrtxt(i)(2:)
                 call prtout (1)
                 go to 20200
              endif
20180         continue
20190      continue
20200      continue
        endif
 
        call ifanal ('P', 1, 0, kspare(8), xx1, lprsx1)
        call ifanal ('F', 1, 0, kspare(9), xx2, fchsx1)
 
        write (outbuf,152) xx1,xx2
  152   format (' *',t29,'(',a,')',t38,'(',a,')',
     1               t50,'Buses With Unscheduled Reactive',t133,'*')
        call prtout (1)
 
C ***   LEVEL 2 ANALYSIS SUMMARIES
 
        write(outbuf,52)
        call prtout (1)
 
        call ifanal ('P', 2, 0, kspare(8), xx1, lprsx1)
        call ifanal ('F', 2, 0, kspare(9), xx2, fchsx1)
 
        write (outbuf,160) xx1,xx2
  160   format (' *',t29,'(',a,')',t38,'(',a,')',
     1               t50,'Total System Generations and Loads by Owner',
     2               t133,'*')
        call prtout (1)
 
        call ifanal ('P', 3, 0, kspare(8), xx1, lprsx1)
        call ifanal ('F', 3, 0, kspare(9), xx2, fchsx1)
 
        write (outbuf,162) xx1,xx2
  162   format (' *',t29,'(',a,')',t38,'(',a,')',
     1               t50,'System Generations, Loads, Losses and ',
     2          'Shunts by Zones',t133,'*')
        call prtout (1)
 
        call ifanal ('P', 4, 1, kspare(8), xx3, lprsx1)
        call ifanal ('F', 4, 1, kspare(9), xx4, fchsx1)
 
        write (outbuf,166) xx3,xx4
  166   format (' *',t29,'(',a,')',t38,'(',a,')',
     1               t50,'Undervoltage-Overvoltage Buses',
     2               t133,'*')
        call prtout (1)
 
        call ifanal ('P', 5, 1, kspare(8), xx3, lprsx1)
        call ifanal ('F', 5, 1, kspare(9), xx4, fchsx1)
 
        write (outbuf,167) xx3,xx4,ratln
  167   format (' *',t29,'(',a,')',t38,'(',a,')',
     1               t50,'Transmission Lines Loaded Above '
     2          ,f5.1,'% of Ratings',t133,'*')
        call prtout (1)
 
        call ifanal ('P', 6, 1, kspare(8), xx3, lprsx1)
        call ifanal ('F', 6, 1, kspare(9), xx4, fchsx1)
 
        write (outbuf,168) xx3,xx4,rattx
  168   format (' *',t29,'(',a,')',t38,'(',a,')',
     1               t50,'Transformers Loaded Above ',f5.1,
     2          '% of Ratings',t133,'*')
        call prtout (1)
 
        call ifanal ('P', 7, 1, kspare(8), xx3, lprsx1)
        call ifanal ('F', 7, 1, kspare(9), xx4, fchsx1)
 
        write (outbuf,169) xx3,xx4
  169   format (' *',t29,'(',a,')',t38,'(',a,')',
     1               t50,'Transformers Excited Above 5.0% over Tap')
        call prtout (1)
 
        call ifanal ('P', 8, 0, kspare(8), xx1, lprsx1)
        call ifanal ('F', 8, 0, kspare(9), xx2, fchsx1)
 
        write (outbuf,170) xx1,xx2
  170   format (' *',t29,'(',a,')',t38,'(',a,')',
     1               t50,'Transmission System Losses ',
     2               t133,'*')
        call prtout (1)
 
        call ifanal ('P', 9, 0, kspare(8), xx1, lprsx1)
        call ifanal ('F', 9, 0, kspare(9), xx2, fchsx1)
 
        write (outbuf,172) xx1,xx2
  172   format (' *',t29,'(',a,')',t38,'(',a,')',
     1               t50,'BPA Industrial Loads',
     2               t133,'*')
        call prtout (1)
 
        call ifanal ('P', 10, 0, kspare(8), xx1, lprsx1)
        call ifanal ('F', 10, 0, kspare(9), xx2, fchsx1)
 
        write (outbuf,174) xx1,xx2
  174   format (' *',t29,'(',a,')',t38,'(',a,')',
     1               t50,'DC System ',
     2               t133,'*')
        call prtout (1)
 
        call ifanal ('P', 11, 0, kspare(8), xx1, lprsx1)
        call ifanal ('F', 11, 0, kspare(9), xx2, fchsx1)
 
        write (outbuf,175) xx1,xx2
  175   format (' *',t29,'(',a,')',t38,'(',a,')',
     1               t50,'Shunt Reactive Summary ',
     2               t133,'*')
        call prtout (1)
 
        call ifanal ('P', 12, 0, kspare(8), xx1, lprsx1)
        call ifanal ('F', 12, 0, kspare(9), xx2, fchsx1)
 
        write (outbuf,10175) xx1,xx2
10175   format (' *',t29,'(',a,')',t38,'(',a,')',
     1               t50,'Summary of LTC Transformers',
     2               t133,'*')
        call prtout (1)
 
        write (outbuf,10173) xx1,xx2
10173   format (' *',t29,'(',a,')',t38,'(',a,')',
     1               t50,'Summary of LTC Reactive Utilization',
     2               t133, '*')
        call prtout (1)
 
        call ifanal ('P', 13, 0, kspare(8), xx1, lprsx1)
        call ifanal ('F', 13, 0, kspare(9), xx2, fchsx1)
 
        write (outbuf,10174) xx1,xx2
10174   format (' *',t29,'(',a,')',t38,'(',a,')',
     1               t50,'Summary of Phase-shifters',
     2               t133,'*')
        call prtout (1)
 
        call ifanal ('P', 14, 0, kspare(8), xx1, lprsx1)
        call ifanal ('F', 14, 0, kspare(9), xx2, fchsx1)
 
        write (outbuf,10176) xx1,xx2
10176   format (' *',t29,'(',a,')',t38,'(',a,')',
     1               t50,'Summary of %Var-controlled buses',
     2               t133,'*')
        call prtout (1)
 
        write (outbuf,20176) xx1,xx2
20176   format (' *',t29,'(',a,')',t38,'(',a,')',
     1               t50,'Summary of AGC Control',
     2               t133,'*')
        call prtout (1)
 
        write (outbuf,30176) xx1,xx2
30176   format (' *',t29,'(',a,')',t38,'(',a,')',
     1               t50,'Summary of Line Drop Compensation',
     2               t133,'*')
        call prtout (1)
 
        write (outbuf,30177) xx1,xx2
30177   format (' *',t29,'(',a,')',t38,'(',a,')',
     1               t50,'Summary of SVC buses',
     2               t133,'*')
        call prtout (1)
 
        call ifanal ('P', 15, 0, kspare(8), xx1, lprsx1)
        call ifanal ('F', 15, 0, kspare(9), xx2, fchsx1)
 
        write (outbuf,10177) xx1,xx2
10177   format (' *',t29,'(',a,')',t38,'(',a,')',
     1               t50,'Summary of Type BX buses',
     2               t133,'*')
        call prtout (1)
 
        call ifanal ('P', 16, 0, kspare(8), xx1, lprsx1)
        call ifanal ('F', 16, 0, kspare(9), xx2, fchsx1)
 
        write (outbuf,10178) xx1,xx2
10178   format (' *',t29,'(',a,')',t38,'(',a,')',
     1               t50,'Summary of Adjustable Var compensation',
     2               t133,'*')
        call prtout (1)
 
        call ifanal ('P', 17, 1, kspare(8), xx3, lprsx1)
        call ifanal ('F', 17, 1, kspare(9), xx4, fchsx1)
 
        write (outbuf,180) xx3,xx4
  180   format (' *',t29,'(',a,')',t38,'(',a,')',
     1               t50,'Transmission Lines Containing Series ',
     2          'Compensation',t133,'*')
        call prtout (1)
 
        write(outbuf,52)
        call prtout (1)
C
C ***   LEVEL 3 ANALYSIS SUMMARIES
C
        call ifanal ('P', 18, 1, kspare(8), xx3, lprsx1)
        call ifanal ('F', 18, 1, kspare(9), xx4, fchsx1)
 
        write (outbuf,182) xx3,xx4
  182   format (' *',t29,'(',a,')',t38,'(',a,')',
     1               t50,'Bus Quantities',
     2               t133,'*')
        call prtout (1)
 
        write(outbuf,52)
        call prtout (1)
C
C ***   LEVEL 4 ANALYSIS SUMMARIES
C
        call ifanal ('P', 19, 0, kspare(8), xx1, lprsx1)
        call ifanal ('F', 19, 0, kspare(9), xx2, fchsx1)
 
        write (outbuf,184) xx1,xx2
  184   format (' *',t29,'(',a,')',t38,'(',a,')',
     1               t50,'Spinning Reserves',
     2               t133,'*')
        call prtout (1)
 
        call ifanal ('P', 20, 1, kspare(8), xx3, lprsx1)
        call ifanal ('F', 20, 1, kspare(9), xx4, fchsx1)
 
        write (outbuf,186) xx3,xx4
  186   format (' *',t29,'(',a,')',t38,'(',a,')',
     1               t50,'Transmission Line Efficiency Analysis'
     2              ,t133,'*')
        call prtout (1)
 
        write (outbuf,188) rateff
  188   format (' *',t50,'Lines Loaded Above ',f5.1,
     1                   ' % of Nominal Ratings', t133,'*')
        call prtout (1)
 
        call ifanal ('P', 22, 1, kspare(8), xx3, lprsx1)
        call ifanal ('F', 22, 1, kspare(9), xx4, fchsx1)
 
        write (outbuf,190) xx3,xx4
  190   format (' *',t29,'(',a,')',t38,'(',a,')',
     1               t50,'Transformer Efficiency Analysis',
     2               t133,'*')
        call prtout (1)
 
        write (outbuf,192) ratxef
  192   format (' *',t50,'Total Losses Above ',f5.2,
     1                   ' % of Nominal Ratings', t133,'*')
        call prtout (1)
 
        call ifanal ('P', 23, 1, kspare(8), xx3, lprsx1)
        call ifanal ('F', 23, 1, kspare(9), xx4, fchsx1)
 
        write (outbuf,194) xx3,xx4
  194   format (' *',t29,'(',a,')',t38,'(',a,')',
     1               t50,'Transformer Efficiency Analysis',
     2               t133,'*')
        call prtout (1)
 
        write (outbuf,196) ratcef
  196   format (' *',t50,'Core Losses Above ',f5.2,
     1                   ' % of Nominal Ratings', t133,'*')
        call prtout (1)
 
        write(outbuf,52)
        call prtout (1)
 
        if (npzanl .gt. 0) then
 
           write (outbuf,230)
  230      format (' *',t5,'Summaries on paper coded "Z" are ',
     2               t133,'*')
           call prtout (1)
           write (outbuf,240)
  240      format (' *',t5,'Restricted to the following zones: ',
     2               t133,'*')
 
           do 260 kst = 1,npzanl,20
              kend = min0(kst+19,npzanl)
              write (outbuf(50:132),250) (pzalst (m),m=kst,kend)
  250         format(20(a2,1x))
              outbuf(133:133) = '*'
              call prtout (1)
              write (outbuf,52)
  260      continue
 
           write(outbuf,52)
           call prtout (1)
 
        else if (npoanl .gt. 0) then
 
           write (outbuf,270)
  270      format (' *',t5,'Summaries on paper coded "O" are ',
     2               t133,'*')
           call prtout (1)
           write (outbuf,280)
  280      format (' *',t5,'Restricted to the following ownerships:',
     2               t133,'*')
 
           j = min0(npoanl,15)
           write (outbuf(50:132),290) (poalst(m),m=1,j)
  290      format(20(a3,1x))
           outbuf(133:133) = '*'
           call prtout (1)
 
           write(outbuf,52)
           call prtout (1)
 
        endif
C
        if (nfzanl .gt. 0) then
 
           write (outbuf,300)
  300      format (' *',t5,'Summaries on fiche coded "Z" are ',
     2                  t133,'*')
           call prtout (1)
           write (outbuf,310)
  310      format (' *',t5,'Restricted to the following zones: ',
     2                  t133,'*')
 
           do 330 kst = 1,nfzanl,20
              kend = min0(kst+19,nfzanl)
              write (outbuf(50:132),320) (fzalst (m),m=kst,kend)
  320         format(20(a2,1x))
              outbuf(133:133) = '*'
              call prtout (1)
              write(outbuf,52)
  330      continue
 
           write(outbuf,52)
           call prtout (1)
 
        else if (nfoanl .gt. 0) then
 
           write (outbuf,340)
  340      format (' *',t5,'Summaries on fiche coded "O" are ',
     1                  t133,'*')
           call prtout (1)
           write (outbuf,350)
  350      format (' *',t5,'Restricted to the following ownerships:',
     1                  t133,'*')
 
           j = min0(nfoanl,15)
           write (outbuf(50:132),360) (foalst(m),m=1,j)
  360      format(20(a3,1x))
           outbuf(133:133) = '*'
           call prtout (1)
 
           write(outbuf,52)
           call prtout (1)
 
        endif
 
        do 362 i = 1,2
        write(outbuf,52)
        call prtout (1)
  362   continue
 
        write (outbuf,15)
        call prtout (1)
 
 
C ***   LEVEL  1  REPORTS  *******
 
C ***   User defined analysis * * * * *
 
        if (numusr .gt. 0) then
 
           call ifanal ('P', 21, 0, kspare(8), xx1, lprsx1)
           call ifanal ('F', 21, 0, kspare(9), xx2, fchsx1)
           call usrrpt
 
        endif
 
C ***   BUSES WITH UNSCHEDULED REACTIVE SUMMARY * * * * *
 
        call ifanal ('P', 1, 0, kspare(8), xx1, lprsx1)
        call ifanal ('F', 1, 0, kspare(9), xx2, fchsx1)
 
        if (lprsx1 .ne. 0 .or. fchsx1 .ne. 0) then
 
           lprtsw = lprsx1
           fichsw = fchsx1
 
           call title(13)
 
           ksw = 0
           call bsanrp  (ksw,subttl,lprsx1,fchsx1)
           qtot = subttl(7) + subttl(10)
           lprtsw = lprsx1
           fichsw = fchsx1
           write (outbuf,370) subttl(7)
  370      format(1h0,19x,'UNSCHEDULED CAPACITORS', f10.1)
           call prtout (1)
 
           write (outbuf,380) subttl(10)
  380      format(    20x,'UNSCHEDULED REACTORS  ', f10.1)
           call prtout (1)
 
           write (outbuf,390) qtot
  390      format(    20x,'UNSCHEDULED TOTAL     ', f10.1)
           call prtout (1)
 
        endif
 
C ****  LEVEL 2  REPORTS  *******
 
C ****  Total system generations, loads, and losses by owner * *
 
        call ifanal ('P', 2, 0, kspare(8), xx1, lprsx1)
        call ifanal ('F', 2, 0, kspare(9), xx2, fchsx1)
 
        if (lprsx1 .ne. 0 .or. fchsx1 .ne. 0) then
 
           lprtsw = lprsx1
           fichsw = fchsx1
 
           do 445 i = 1,10
              systot(i) = 0.0
  445      continue
 
           do 447 i = 1,10
              subtot(i) = 0.0
              subto2(i) = 0.0
  447      continue
 
           call title(1)
           io1 = 1
 
           do 555 io = 1,jowner
              jo = lstown(io)
              kowner = lowner(jo)
              kown1 = ljstfy  (kowner)
 
  446         if (io1 .le. namtot) then
                 if (kompr(kown1,owners(io1)(1:3),junk) .gt. 0) then
                    io1 = io1 + 1
                    goto 446
                 else if(kompr(kown1,owners(io1)(1:3),junk).eq.0) then
                    io2 = io1
                 else
                    io2 = namtot + 1
                 endif
              else
                 io2 = namtot + 1
              endif
 
C
C             Split OWNERS(IO2) into a 34-character string an a
C             16-character string.
C
              if (owners(io2)(42:) .eq. ' ') then
                 ownbf1 = owners(io2)(8:)
                 ownbf2 = ' '
              else
                 do 452 i = 41, 9, -1
                    if (owners(io2)(i:i) .eq. ' ') then
                       ownbf1 = owners(io2)(8:i)
                       ownbf2 = owners(io2)(i+1:)
                       go to 454
                    endif
  452            continue
                 ownbf1 = owners(io2)(8:41)
                 ownbf2 = owners(io2)(42:)
  454            continue
              endif
 
              if (kowner .eq. 'BPA') then
                 do 451 i = 1,10
                    subtot(i) = firm(i) + secn(i)
                    subto2(i) = firmi(i) + rupti(i) + poti(i)
  451            continue
                 write (outbuf,460) kowner, ownbf1
  460            format(1x,a3,1x,a34)
                 call prtout (1)
                 if (ownbf2 .ne. ' ') then
                    write (outbuf,460) ownbf2
  462               format(t6, a)
                    call prtout (1)
                 endif
                 write (outbuf,470)
  470            format('0',t6,'NON-INDUSTRIAL  ')
                 call prtout (1)
                 write (outbuf,500) firm
                 call prtout (1)
                 write (outbuf,480) secn
  480            format(t6, '  SECONDARY     ', t40, 10f9.1)
                 call prtout (1)
                 write(outbuf,521)
  521            format(t40,10('    _____'))
                 call prtout (1)
                 write (outbuf,530) (subtot(i),i=1,10)
                 call prtout (1)
                 write (outbuf,490)
  490            format('0', t6, 'INDUSTRIAL      ')
                 call prtout (1)
                 write (outbuf,500) firmi
  500            format(t6, '  FIRM          ', t40, 10f9.1)
                 call prtout (1)
                 write (outbuf,510) rupti
  510            format(t6, '  INTERRUPTIBLE ', t40, 10f9.1)
                 call prtout (1)
                 write (outbuf,520) poti
  520            format(t6, '  POTENTIAL     ', t40, 10f9.1)
                 call prtout (1)
                 write(outbuf,522)
  522            format(t40, 10('    _____'))
                 call prtout(1)
                 write (outbuf,530) (subto2(i),i=1,10)
  530            format(t6, '  SUB-TOTAL     ', t40, 10f9.1)
                 call prtout (1)
                 write(outbuf,532)
  532            format(t40,10('    _____'))
                 call prtout(1)
                 write (outbuf,540) (syst(i,jo),i=1,10)
  540            format(t6,'TOTAL           ', t40, 10f9.1)
                 call prtout (1)
                 outbuf = '0'
                 call prtout (1)
 
              else if (kowner.ne.'   ') then
                 if (ownbf2 .eq. ' ') then
                    write (outbuf, 542) kowner, ownbf1,
     1                (syst(i,jo),i=1,10)
                    call prtout (1)
  542               format(1x, a3, 1x, a, t40, 10f9.1)
                 else
                    write (outbuf,544) kowner, ownbf1
  544               format(1x, a3, 1x, a)
                    call prtout (1)
                    write (outbuf,546) ownbf2, (syst(i,jo),i=1,10)
  546               format(t6, a, t40, 10f9.1)
                    call prtout (1)
                 endif
              else
                 write (outbuf,550) (syst(i,jo),i=1,10)
                 call prtout (1)
  550            format(t6,'NON-CLASSIFIED     ',t40, 10f9.1)
 
              endif
 
              do 551 i = 1,10
                 systot(i) = systot(i) + syst(i,jo)
  551         continue
 
  555      continue
           write(outbuf,556)
  556      format(t40, 10('    _____'))
           call prtout(1)
           write (outbuf,560) systot
           call prtout (1)
  560      format(t6, 'SYSTEM TOTAL       ', t40, 10f9.1)
 
        endif
 
C ****  SYSTEM GENERATIONS, LOADS, LOSSES AND SHUNTS BY ZONES
 
        call ifanal ('P', 3, 0, kspare(8), xx1, lprsx1)
        call ifanal ('F', 3, 0, kspare(9), xx2, fchsx1)
 
        if (lprsx1 .ne. 0 .or. fchsx1 .ne. 0) then
 
           lprtsw = lprsx1
           fichsw = fchsx1
 
           if (nztot.ne.0) then
 
              call title(12)
              do 675 i = 1,14
                subtot(i) = 0.0
675           continue
 
              do 688 i = 1,nztot
                write (outbuf,680) acznam(i),(zsum(j,i),j=1,4),
     1             (zsum(j,i),j=21,26), (zsum(j,i),j=5,8)
680             format(t2, a2, t8, 14f9.1)
                call prtout (1)
 
                do 683 j = 1,8
                  subtot(j) = subtot(j) + zsum(j,i)
683             continue
                do 684 j = 9, 14
                  subtot(j) = subtot(j) + zsum(j+12,i)
684             continue
 
688           continue
 
              write(outbuf,689)
689           format('0', t8, 14('    -----'))
              call prtout(1)
              write (outbuf,690) (subtot(j),j=1,4),
     1           (subtot(j),j=9,14), (subtot(j),j=5,8)
690           format(' Totals', t8, 14f9.1)
              call prtout (1)
 
              if (neggen .ne. 0.0) then
                 write(outbuf,20691) neggen
20691            format('0-Gen', t8, f9.1)
                 call prtout(1)
                 write(outbuf,20692)
20692            format('0', t8, 2(' --------', 9x))
                 call prtout(1)
                 write (outbuf,20693) subtot(1)+neggen, subtot(3)+neggen
20693            format(' Net', t8, f9.1, 9x, f9.1)
                 call prtout(1)
              endif
 
           endif
 
        endif
 
C ***   SUMMARY OF UNDERVOLTAGE - OVERVOLTAGE BUSES * * * * *
 
        call ifanal ('P', 4, 1, kspare(8), xx1, lprsx1)
        call ifanal ('F', 4, 1, kspare(9), xx2, fchsx1)
 
        if (lprsx1 .ne. 0 .or. fchsx1 .ne. 0) then
 
           lprtsw = lprsx1
           fichsw = fchsx1
 
           sortsw = mod (kspare(21),10)
           if (sortsw .eq. 0) then
              if (kspare(11) .eq. 0 .or. kspare(11) .eq. 3) then
C ***
C ***            Default OVERVOLTAGE = <OWNER> if RPT_SORT is defaulted.
C ***
                 sortsw = 1
              else if (kspare(11) .eq. 1) then
C ***
C ***            Default OVERVOLTAGE = <ZONE> if RPT_SORT = <ZONE>.
C ***
                 sortsw = 3
              else if (kspare(11) .eq. 2 .and. ntotc .eq. 0) then
C ***
C ***            Default OVERVVOLTAGE = <AREA> if RPT_SORT = <AREA>
C ***            and NTOTC = 0
C ***
                 sortsw = 3
              else if (kspare(11) .eq. 2 .and. ntotc .gt. 0) then
C ***
C ***            Default OVERVVOLTAGE = <AREA> if RPT_SORT = <AREA>.
C ***
                    sortsw = 4
              endif
           endif
           call title(4)
 
           do 572 i = 1, nba
  572      vltsrt(i) = i
 
           if (sortsw .gt. 0) call qiksrt (1, nba, kpovuv, spovuv )
 
           isw = 1
           do 575 jt = 1,nba
              nb = vltsrt(jt)
              if (inp2alf(nb) .eq. 0 .or. inp2alf(nb) .gt. ntot_alf)
     &           go to 575
              lprtsw = 0
              fichsw = 0
              voltpu = abus(4,nb)
              dvkv = dim(voltpu,abus(15,nb)) - dim(abus(12,nb),voltpu)
              if( abs(dvkv) .lt. 0.0005 ) dvkv = 0.0
 
              if (dvkv.ne.0) then
                 write ( type, 580) abus(2,nb)
 580             format (a1)
                 degree = abus(14,nb)
                 vmin = abus(12,nb)
                 vmax = abus(15,nb)
                 k1 = kabus(1,nb)
                 zn = zone(k1)
                 own = owner(k1)
 
                 lprtsw = ipzo(zn,own,lprsx1)
                 fichsw = ifzo(zn,own,fchsx1)
 
                 if (lprtsw.ne.0.or.fichsw.ne.0) then
                     if (isw.ne.2) then
                        isw = 2
                     endif
                     voltkv = base(k1)*voltpu
                     if (numobs .eq. 0) then
                        relvlt = ' '
                     else
                        i = oldbsh (bus(k1), base(k1))
                        if (i .gt. 0) then
                           v = voltpu / sqrt(olde(i)**2 + oldf(i)**2)
                           ivrel = 100.0 * v + 0.5
                           write (relvlt, 562) ivrel
  562                      format ('(', i3, ')')
                        else
                           relvlt = ' '
                        endif
                     endif
                     write (outbuf,570) owner(k1),zone(k1),bus(k1),
     1               base(k1),type,voltkv,voltpu,relvlt,vmin,vmax,dvkv
                     call prtout (1)
570                  format(16x,a3,2x,a2,4x,a8,f7.1,5x,a1,f10.2,f9.4,
     1                  6x, a5, f12.4,f10.4,f14.4)
                 endif
              endif
575        continue
 
           lprtsw = lprsx1
           fichsw = fchsx1
           if (isw .eq. 1) then
              outbuf='0     NO UNDERVOLTAGE/OVERVOLTAGE BUSES DETECTED'
              call prtout(1)
           endif
        endif
 
C ****  TRANSMISSION LINES LOADED ABOVE RATING * * * * *
 
        call ifanal ('P', 5, 1, kspare(8), xx1, lprsx1)
        call ifanal ('F', 5, 1, kspare(9), xx2, fchsx1)
 
        if (lprsx1 .ne. 0 .or. fchsx1 .ne. 0) then
 
           lprtsw = lprsx1
           fichsw = fchsx1
 
            sortsw = kspare(21) / 10
            if (sortsw .eq. 0) then
               if (kspare(11) .eq. 0 .or. kspare(11) .eq. 3) then
C ***
C ***          Default OVERLOAD = <OWNER> if RPT_SORT is defaulted.
C ***
                  sortsw = 1
               else if (kspare(11) .eq. 1) then
C ***
C ***          Default OVERLOAD = <ZONE> if RPT_SORT = <ZONE>.
C ***
                  sortsw = 3
               else if (kspare(11) .eq. 2) then
C ***
C ***          Default OVERLOAD = <AREA> if RPT_SORT = <AREA>.
C ***
                  sortsw = 4
               endif
            endif
            call title(5)
 
            if (nolbr.ne.0) then
 
              nolbr = min0(nolbr,400)
              if (sortsw .gt. 0) call qiksrt (1, nolbr, kpovll, spovll)

 
              do 605 kt = 1,nolbr
                 ptr = kolbr(1,kt)  
                 k1 = kx(ptr)
                 k2 = ky(ptr)
                 id = brid(ptr)
                 isect = brsect(ptr) 
                 qptr= brnch_ptr(ptr)
                 jbr = iabs(qptr)
                 write (own,585) kbrnch(3,jbr)
 585             format(a3)
                 zn1 = zone(k1)
                 zn2 = zone(k2)
                 lprtsw = ipzo(zn1,own,lprsx1)
                 if (lprtsw.eq.0) then
                    lprtsw = ipzo(zn2,own,lprsx1)
                 endif
                 fichsw = ifzo(zn1,own,fchsx1)
                 if (fichsw.eq.0) then
                    fichsw = ifzo(zn2,own,fchsx1)
                 endif
 
                 if (lprtsw.ne.0.or.fichsw.ne.0) then
 
                    arate(1) = brnch(4,jbr)
                    amag = olbr(2,kt)
                    vamag = olbr(3,kt)
                    tang = olbr(5,kt)
                    pft = olbr(6,kt)
 
                    arate(2) = rateln(1,jbr)
                    arate(3) = rateln(2,jbr)
 
                    do 604 i = 1,3
                    varate(i) = 0.00173205 * arate(i) * base(k1)
                    if (arate(i) .gt. 0.0) then
                       apct = 100.0 * amag / arate(i)
                       write (apctc(i),603) apct
  603                  format (f7.1)
                       vapct = 100.0 * vamag / varate(i)
                       write (vapctc(i),603) vapct
                    else
                       apctc(i) = ' '
                       vapctc(i) = ' '
                    endif
  604               continue
C ***
C ***              Force a new page if less than 8 lines remain
C ***
                   call chkbtm(8)
                   write (outbuf,590) own,zone(k1),bus(k1),base(k1),
     1                zone(k2),bus(k2),base(k2),id,isect,amag,
     2                arate(2),arate(3),arate(1),vamag,varate(2),
     3                varate(3),varate(1),pft,tang
590                format(1x,a3,1x,a2,1x,a8,f7.1,1x,a2,1x,a8,f7.1,1x,
     1                a1,i2,t48,4f8.1,t83,4f8.1,t117,f7.3,f9.1)
 
                   if (varate(1) * arate(1) .eq. 0.0) then
                       write (outbuf(127:),600)
600                    format('NO RAT')
                   endif
                   call prtout (1)
 
                   write (outbuf,602) apctc(2),apctc(3),apctc(1),
     1                vapctc(2),vapctc(3),vapctc(1)
602                format(t57,3(a7,'%'),t92,3(a7,'%'))
                   call prtout (1)
                   call space (1)
 
                 endif
605              continue
 
            else
               outbuf = '0           NO OVERLOADED LINES DETECTED'
               call prtout(1)
            endif
 
        endif
 
C ****  TRANSFORMERS LOADED ABOVE RATING * * * * * * * *
 
        call ifanal ('P', 6, 1, kspare(8), xx1, lprsx1)
        call ifanal ('F', 6, 1, kspare(9), xx2, fchsx1)
 
        if (lprsx1 .ne. 0 .or. fchsx1 .ne. 0) then
 
           lprtsw = lprsx1
           fichsw = fchsx1
 
            sortsw = kspare(21) / 10
            if (sortsw .eq. 0) then
               if (kspare(11) .eq. 0 .or. kspare(11) .eq. 3) then
C ***
C ***          Default OVERLOAD = <OWNER> if RPT_SORT is defaulted.
C ***
                  sortsw = 1
               else if (kspare(11) .eq. 1) then
C ***
C ***          Default OVERLOAD = <ZONE> if RPT_SORT = <ZONE>.
C ***
                  sortsw = 3
               else if (kspare(11) .eq. 2) then
C ***
C ***          Default OVERLOAD = <AREA> if RPT_SORT = <AREA>.
C ***
                  sortsw = 4
               endif
            endif
            call title(6)
 
            if (noltr.ne.0) then
 
              if (sortsw .gt. 0) call qiksrt (1, noltr, kpovlt, spovlt)

 
              do 625 kt = 1,noltr
                 ptr = koltr(1,kt)  
                 k1 = kx(ptr)
                 k2 = ky(ptr)
                 id = brid(ptr)
                 isect = brsect(ptr) 
                 qptr= brnch_ptr(ptr)
                 jbr = iabs(qptr)
                 write (own,585) kbrnch(3,jbr)
                 zn1 = zone(k1)
                 zn2 = zone(k2)
                 lprtsw = ipzo(zn1,own,lprsx1)
                 if (lprtsw.eq.0) then
                    lprtsw = ipzo(zn2,own,lprsx1)
                 endif
                 fichsw = ifzo(zn1,own,fchsx1)
                 if (fichsw.eq.0) then
                    fichsw = ifzo(zn2,own,fchsx1)
                 endif
 
                 if (lprtsw.ne.0.or.fichsw.ne.0) then
 
                    varate(1) = brnch(4,jbr)
                    vamag = oltr(2,kt)
                    amag = oltr(3,kt)
                    varate(2) = rateln(1,jbr)
                    varate(3) = rateln(2,jbr)
                    varate(4) = rateln(3,jbr)
 
                    do 606 i = 1,4
                    if (varate(i) .gt. 0.0) then
                       if (i .lt. 4) then
                          vapct = 100.0 * vamag / varate(i)
                       else
                          vapct = 100.0 * amag / varate(i)
                       endif
                       write (vapctc(i),10603) vapct
10603                  format (f6.1)
                    else
                       vapctc(i) = ' '
                    endif
  606               continue
 
                    write (outbuf,610) own,zone(k1),bus(k1),base(k1),
     1                 zone(k2),bus(k2),base(k2),id,isect,
     2                 vamag, (varate(i),vapctc(i),i=2,3), amag,
     3                 varate(4),vapctc(4),varate(1),vapctc(1),
     4                 oltr(4,kt)
 
610                 format (t2,a3,1x,a2,1x,a8,f7.1,1x,a2,1x,a8,f7.1,
     1                 1x,a1,i2,f9.1,1x,2(f8.1,a6),f9.1,1x,2(f8.1,a6),
     2                 f8.3)
 
                    if (varate(1) .eq. 0.0) then
                        write (outbuf(117:),620)
620                     format('NO RATING')
                    endif
                    call prtout (1)
                 endif
625           continue
 
              call space(4)
              write (outbuf, 10626)
10626         format (' Note: "Adj MVA Loading" is larger of Primary MVA
     &/primary v(p.u) or secondary mva/secondary v(p.u.)')
              call prtout (1)
 
              lprtsw = lprsx1
              fichsw = fchsx1
 
            else
              outbuf = '0          NO OVERLOADED TRANSFORMERS DETECTED'

              call prtout(1)
            endif
 
        endif
 
C ****  Overexcited transformers * * * * * * * * * *
C
 
        call ifanal ('P', 7, 1, kspare(8), xx1, lprsx1)
        call ifanal ('F', 7, 1, kspare(9), xx2, fchsx1)
 
        if (lprsx1 .ne. 0 .or. fchsx1 .ne. 0) then
 
           lprtsw = lprsx1
           fichsw = fchsx1
 
           call ovextx
 
        endif
C
C ****  TRANSMISSION SYSTEM LOSSES * * * * * * * * * *
 
        call ifanal ('P', 8, 0, kspare(8), xx1, lprsx1)
        call ifanal ('F', 8, 0, kspare(9), xx2, fchsx1)
 
        if (lprsx1 .ne. 0 .or. fchsx1 .ne. 0) then
 
           lprtsw = lprsx1
           fichsw = fchsx1
 
            call title(3)
 
            do 626 i = 1,10
              subttl(i) = 0.0
626         continue
 
            io1 = 1
 
            do 665 ij = 1,jowner
              ijs = lstown(ij)
              kowner = lowner(ijs)
              kown1 = ljstfy  (kowner)
 
627           if (io1 .le. namtot) then
 
                if (kompr (kown1,owners(io1)(1:3),junk) .gt. 0) then
                   io1 = io1 + 1
                   goto 627
                else if (kompr(kown1,owners(io1)(1:3),junk).eq.0) then
                   io2 = io1
                else
                   io2 = namtot + 1
                endif
              else
                io2 = namtot + 1
              endif
 
              isw = 0
 
              do 655 iv = 1,jvolt
                ivs = lstvlt(iv)
                xvolt = avolt(ivs)
                lov = kov(ivs,ijs)
 
                if (lov.ne.0) then
                  isw = isw + 1
 
                  if (isw.le.1) then
                     do 628 i = 1,10
                        subtot(i) = 0.0
628                  continue
                     if (kowner.eq.'   ') then
                        write (outbuf,630)
                        call prtout (1)
630                     format('0   NON-CLASSIFIED')
                     else
                        write (outbuf,640) kowner,owners(io2)(8:60)
640                     format('0   ',a3,2x,a53)
                        call prtout (1)
                     endif
 
                  endif
 
                  x1 = 0
                  x2 = 0
 
                  do 645 i = 1,5,2
                     x1 = x1 + ovlos(i,lov)
                     x2 = x2 + ovlos(i+1,lov)
645               continue
 
                  write (outbuf,650) xvolt, (ovlos(i,lov),i=1,6),x1,x2,

     1               ovlos(7,lov),ovlos(8,lov)
 
                  call prtout (1)
650               format (7x,f6.1,' KV',5x,4(1x,2f9.1),f16.1,f16.1)
 
                  do 653 i = 1,8
                    subtot(i) = subtot(i) + ovlos(i,lov)
653               continue
 
                  subtot(9) = subtot(9) + x1
                  subtot(10) = subtot(10) + x2
                endif
 
655           continue
 
              if (isw-1 .gt. 0) then
                 write(outbuf,656)
656          format(t27,'_____',t36,'_____',t46,'_____',t55,'_____',t65,
     1       '_____',t74,'_____',t84,'_____',t93,'_____',t109,'_____',
     2           t125,'_____')
                 call prtout(1)
                write (outbuf,660) (subtot(i),i=1,6),subtot(9),
     1           subtot(10),subtot(7),subtot(8)
                call prtout (1)
660             format (7x,'SUB-TOTAL',5x,4(1x,2f9.1),f16.1,f16.1)
              endif
 
              if (isw-1 .ge. 0) then
                do 663 i = 1,10
                    subttl(i) = subttl(i) + subtot(i)
663             continue
              endif
 
665         continue
                 write(outbuf,666)
666         format(t27,'_____',t36,'_____',t46,'_____',t55,'_____',t65,

     1      '_____',t74,'_____',t84,'_____',t93,'_____',t109,'_____',
     2           t125,'_____')
                 call prtout(1)
 
            write (outbuf,670) (subttl(i),i=1,6),subttl(9),subttl(10)
     1      ,subttl(7),subttl(8)
 
            call prtout (1)
670         format ('0   SYSTEM TOTAL',5x,4(1x,2f9.1),f16.1,f16.1)
 
 
        endif
 
C ***   BPA INDUSTRIAL LOAD AND GENERATION BY BUS * * *
C
 
        call ifanal ('P', 9, 0, kspare(8), xx1, lprsx1)
        call ifanal ('F', 9, 0, kspare(9), xx2, fchsx1)
 
        if (lprsx1 .ne. 0 .or. fchsx1 .ne. 0) then
 
           lprtsw = lprsx1
           fichsw = fchsx1
 
            call title(8)
            if (ncon .ne. 0) then
 
              do 692 i = 1,8
                 subtot(i) = 0.0
  692         continue
 
              do 707 i1 = 1, ncon
                i2 = load(1,i1)
 
                do 10690 i = 1, 8
10690           subto2(i) = 0.0
 
                do 10692 i = 1, 3
                   do 10692 j = 1, 3
                      k = 6*i + 2*j - 6
                      subto2(2*i-1) = subto2(2*i-1) + aload(k,i1)
                      subto2(2*i)   = subto2(2*i)   + aload(k+1,i1)
                      subtot(2*i-1) = subtot(2*i-1) + aload(k,i1)
                      subtot(2*i)   = subtot(2*i)   + aload(k+1,i1)
10692           continue
 
                ptot = 0.0
                qtot = 0.0
                do 693 i = 1,3
                  ptot = ptot + aload(6*i-4,i1)
                  qtot = qtot + aload(6*i-3,i1)
  693           continue
 
                write (outbuf,694) bus(i2),base(i2),zone(i2),
     1             'Constant P', (aload(6*i-4,i1), aload(6*i-3,i1),
     2              i=1,3),ptot,qtot
  694           format('0', a8, f6.1, t18, a2, t24, a, t40, 8f9.1)
                call prtout (1)
 
                ptot = 0.0
                qtot = 0.0
                do 695 i = 1,3
                  ptot = ptot + aload(6*i-2,i1)
                  qtot = qtot + aload(6*i-1,i1)
  695           continue
 
                write (outbuf,696) 'Constant I', (aload(6*i-2,i1),
     1             aload(6*i-1,i1),i=1,3),ptot,qtot
  696           format(t24, a, t40, 8f9.1)
                call prtout (1)
 
                ptot = 0.0
                qtot = 0.0
                do 697 i = 1,3
                  ptot = ptot + aload(6*i,i1)
                  qtot = qtot + aload(6*i+1,i1)
  697           continue
 
                write (outbuf,696) 'Constant Z', (aload(6*i,i1),
     1             aload(6*i+1,i1),i=1,3),ptot,qtot
                call prtout (1)
 
                ptot = 0.0
                qtot = 0.0
                do 698 i = 1,3
                  ptot = ptot + subto2(2*i-1)
                  qtot = qtot + subto2(2*i)
  698           continue
 
                write (outbuf,699)
  699           format(t40,8('     ----'))
                call prtout (1)
 
                write (outbuf,696) 'Sub-total',(subto2(i),i=1,6),
     1             ptot,qtot
                call prtout (1)
 
707           continue
 
              subtot(7) = subtot(1) + subtot(3) + subtot(5)
              subtot(8) = subtot(2) + subtot(4) + subtot(6)
 
              write (outbuf,708)
  708         format('0', t40, 8('     ----'))
              call prtout (1)
 
              write (outbuf,710) (subtot(i),i=1,8)
              call prtout (1)
710           format('0Total Industrial Loads', t40, 8f9.1)
            else
              outbuf='0     Case contains no Industrial-type loads'
              call prtout(1)
            endif
 
        endif
 
C ***   DC SYSTEM SUMMARY * * * * * * * * * * * *
 
        call ifanal ('P', 10, 0, kspare(8), xx1, lprsx1)
        call ifanal ('F', 10, 0, kspare(9), xx2, fchsx1)
 
        if (lprsx1 .ne. 0 .or. fchsx1 .ne. 0) then
 
           lprtsw = lprsx1
           fichsw = fchsx1
 
            if (ndcln.ne.0) then
 
              call forbtm
              write (outbuf,720)
720           format('0',60x,'DC LINE ANALYSIS ' )
              call shdlod (1)
              write (outbuf,730)
730           format('0TYPE CONVERTER BUS   ZONE  FUNCTION  ----------',
     1               '-------------- CONVERTER OUTPUT -------   ',
     2               'DC BRANCHES       ZONE   EXPORT  LOSSES')
              call shdlod (2)
              write (outbuf,740)
740           format (43x,' LOSSES       ANGLE   POWER  VOLTAGE  ',
     1                'CURRENT  ',26x,'POWER     (MW) ')
              call shdlod (3)
              write (outbuf,750)
750           format (40x,'(MW)   (MVAR)    (DEG)    (MW)    (KV)   ',
     1                '(AMPS) ', 29x,'(MW)')
              call shdlod (4)
              outbuf = '0'
              call shdlod (5)
 
              call fortop
              k1old = 0
              do 795 k = 1,ndcln
                 k1 = ladc(1,k)
                 if (k1.ne.k1old) then
                   ksw = 0
                   k1old = k1
                   ktyp=ladc(2,k)
                   kode=ladc(11,k)
 
                   if (kode.ne.0) then
                      j1 = ' '
                      j2 = ' '
                      j3 = ' '
                      i1 = ladc(7,k)
                      i1 = max0 (i1,ladc(9,k),ladc(14,k))
                      if (ladc(12,k).ne.0) j1 = '*'
                      if (i1.ne.0) j2 = '*'
                      if (ladc(13,k).ne.0) j3 = '*'
                      adc(3,k) = abs(adc(3,k) - adc(6,k))
 
                      write (outbuf,760) ktyp,bus(k1),base(k1),
     1                   zone(k1),fnct(kode),(adc(i,k),i=3,6),j1,
     2                   adc(8,k),j2,adc(10,k),j3
 
760                   format('0 ',a3,2x,a8,f7.1,2x,a2,2x,a9,f7.1,f8.1,
     1                  f8.1,f9.1,a1,f8.1,a1,f8.1,a1)
 
                   else
                      write (outbuf,770) ktyp,bus(k1),base(k1)
     1                 ,zone(k1),adc(8,k)
 
770                   format('0 ',a3,2x,a8,f7.1,2x,a2,'   PASSIVE NODE '
     1               ,29x,f7.1)
 
                   endif
 
                 else
                   k2 = ladc (2, k)
                   if (ksw.eq.0) then
                     write (outbuf(91:),780) bus(k2),base(k2)
     1                ,zone(k2),(adc(i,k),i=3,4),ladc(5,k)
780                  format(1x,a8,f7.1,2x,a2,f12.1,f6.1,i5)
                     ksw = 1
                   else
                     write (outbuf,790) bus(k2),base(k2)
     1                ,zone(k2),(adc(i,k),i=3,4),ladc(5,k)
790                  format(91x,a8,f7.1,2x,a2,f12.1,f6.1,i5)
                   endif
                   call prtout (1)
                 endif
 
795           continue
 
              write (outbuf,800)
800           format('0',45x,'("*" INDICATES CONVERTER-CONTROLLED ',
     1               'QUANTITY)')
 
              call prtout (1)
 
            endif
        endif
 
C ***   Shunt Reactive Summary * * * * * * * * * * *
 
        call ifanal ('P', 11, 0, kspare(8), xx1, lprsx1)
        call ifanal ('F', 11, 0, kspare(9), xx2, fchsx1)
 
        if (lprsx1 .ne. 0 .or. fchsx1 .ne. 0) then
 
           lprtsw = lprsx1
           fichsw = fchsx1
 
           call title(16)
           do i = 1,8
             subttl(i) = 0.0
           enddo
 
           if (ntotc.gt.0) then
             do i = 1,ntotc
               do j = 1,8
                 subtot(j) = 0.0
               enddo
               write(outbuf,1020)arcnam(i)
 1020          format('0 ',a10)
               call prtout(1)
               izone = 0
               do j = 1,nztot
                 if (acznum(j) .eq. i) then
                   izone = izone + 1
                   do k = 1,6
                     subttl(k) = subttl(k) + zsum(14+k,j)
                   enddo
                   x1 = zsum(15,j) - zsum(16,j)
                   x2 = zsum(18,j) - zsum(19,j)
                   write (outbuf,1040) acznam(j), zsum(15,j), 
     &               zsum(16,j), x1, zsum(17,j), zsum(18,j),
     &               zsum(19,j), x2, zsum(20,j)
 1040              format(4x,a2,6x,4f12.1,7x,4f13.1)
                   call prtout(1)
                   do k = 1,6
                     subtot(k) = subtot(k) + zsum(14+k,j)
                   enddo
                   x1 = subtot(1)-subtot(2)
                   x2 = subtot(4)-subtot(5)
                 endif
               enddo
               if (izone.gt.1) then
                 write(outbuf,1065)
 1065            format(t20, '_____', t32, '_____', t44, '_____',
     &             t56, '_____', t76, '_____', t89, '_____', t102,
     &             '_____', t115, '_____')
                 call prtout(1)
                 write(outbuf,1070) subtot(1), subtot(2), x1,
     &             subtot(3), subtot(4), subtot(5), x2, subtot(6)
 1070            format ('  SUBTOTAL',2x,4f12.1,7x,4f13.1)
                 call prtout(1)
               endif
             enddo
           else
             do j = 1,nztot
               do k = 1,6
                 subttl(k) = subttl(k) + zsum(14+k,j)
               enddo
               x1 = zsum(15,j)-zsum(16,j)
               x2 = zsum(18,j)-zsum(19,j)
               write (outbuf,1040) acznam(j), zsum(15,j), zsum(16,j),
     &           x1, zsum(17,j), zsum(18,j), zsum(19,j), x2,
     &           zsum(20,j)
               call prtout(1)
             enddo
           endif
           x1 = subttl(1)-subttl(2)
           x2 = subttl(4)-subttl(5)
           write(outbuf,1105)
 1105      format(t20, '_____', t32, '_____', t44, '_____', t56,
     &       '_____', t76, '_____', t89, '_____', t102, '_____', 
     &       t115, '_____')
           call prtout(1)
           write (outbuf,1110) subttl(1), subttl(2), x1, subttl(3),
     &       subttl(4), subttl(5), x2, subttl(6)
 1110      format('  TOTAL ',4x,4f12.1,7x,4f13.1)
           call prtout(1)
        endif   
 
C ***   Summary of LTC Transformers
C ***   Summary of LTC Reactive Utilization
 
        call ifanal ('P', 12, 0, kspare(8), xx1, lprsx1)
        call ifanal ('F', 12, 0, kspare(9), xx2, fchsx1)
 
        if (lprsx1 .ne. 0 .or. fchsx1 .ne. 0) then
 
           lprtsw = lprsx1
           fichsw = fchsx1
 
           call ltcsum
           call txbsum
 
        endif
 
C ***   Summary of Phase-shifters  * * * * * * * * * * *
 
        call ifanal ('P', 13, 0, kspare(8), xx1, lprsx1)
        call ifanal ('F', 13, 0, kspare(9), xx2, fchsx1)
 
        if (lprsx1 .ne. 0 .or. fchsx1 .ne. 0) then
 
           lprtsw = lprsx1
           fichsw = fchsx1
 
           call phasum
 
        endif
 
C ***   Summary of %Var-controlled buses
C ***   Summary of AGC Control
C ***   Summary of Line Drop Compensation
C ***   Summary of SVC buses
 
        call ifanal ('P', 14, 0, kspare(8), xx1, lprsx1)
        call ifanal ('F', 14, 0, kspare(9), xx2, fchsx1)
 
        if (lprsx1 .ne. 0 .or. fchsx1 .ne. 0) then
 
           lprtsw = lprsx1
           fichsw = fchsx1
 
           call varsum
           if (numagc .gt. 0) call agcsum
           if (numldc .gt. 0) call ldcsum
           if (numsvc .gt. 0) call svcsum
        endif
 
C ***   Summary of Type BX buses  * * * * * * * * * * * * * *
 
        call ifanal ('P', 15, 0, kspare(8), xx1, lprsx1)
        call ifanal ('F', 15, 0, kspare(9), xx2, fchsx1)
 
        if (lprsx1 .ne. 0 .or. fchsx1 .ne. 0) then
 
           lprtsw = lprsx1
           fichsw = fchsx1
 
           call xbusum
 
        endif
 
C ***   Summary of Adjustable Var Compsensation * * * * * * *
 
        call ifanal ('P', 16, 0, kspare(8), xx1, lprsx1)
        call ifanal ('F', 16, 0, kspare(9), xx2, fchsx1)
 
        if (lprsx1 .ne. 0 .or. fchsx1 .ne. 0) then
 
           lprtsw = lprsx1
           fichsw = fchsx1
 
           call ycpsum
 
        endif
 
C ***   TRANSMISSION LINES CONTAINING SERIES COMPENSATION  *
 
        call ifanal ('P', 17, 1, kspare(8), xx1, lprsx1)
        call ifanal ('F', 17, 1, kspare(9), xx2, fchsx1)
 
        if (lprsx1 .ne. 0 .or. fchsx1 .ne. 0) then
 
           lprtsw = lprsx1
           fichsw = fchsx1
 
              call forbtm
              write (outbuf,810)
810           format('0',44x,'TRANSMISSION LINES CONTAINING ',
     1           'COMPENSATION ')
              call shdlod (1)
              write (outbuf,820)
820           format( 1h0, 33x,
     1           'OWN  ZONE  BUS1      BASE1  ZONE  BUS2',
     2           '      BASE2   ID  PERCENT '  )
              call shdlod (2)
              outbuf = '0'
              call shdlod (3)
              outbuf = ' '
              call shdlod(4)
              call shdlod(5)
              call fortop
 
           if (ncomp.ne.0) then
              do 835 k = 1,ncomp
                 k1 = komp(1,k)
                 k2 = komp(3,k)
                 zn1 = zone(k1)
                 zn2 = zone(k2)
                 write(own,585)comp(5,k)
                 lprtsw = ipzo(zn1,own,lprsx1)
                 if (lprtsw.eq.0) then
                   lprtsw = ipzo(zn2,own,lprsx1)
                 endif
                 fichsw = ifzo(zn1,own,fchsx1)
                 if (fichsw.eq.0) then
                   fichsw = ifzo(zn2,own,fchsx1)
                 endif
                 if(lprtsw.ne.0.or.fichsw.ne.0) then
 
                    write (id,580) comp(4,k)
                    write (outbuf,830)own, zone(k1),bus(k1),base(k1)
     1                ,zone(k2),bus(k2),base(k2),id,comp(2,k)
                    call prtout (1)
  830               format(34x,a3,2x,a2,3x,a8,f7.1,3x,a2,3x,a8,f7.1,
     1                 4x,a1,f10.1)
                 endif
835           continue
           else
              call space(3)
              outbuf = '0        CASE CONTAINS NO TRANSMISSION LINES '
     1          //  'WITH SERIES COMPENSATION'
              call prtout(1)
           endif
 
        endif
 
C ***   LEVEL 3   * * * * * * * *
 
C ***   BUS QUANTITIES * * * * * * * * *
 
        call ifanal ('P', 18, 1, kspare(8), xx1, lprsx1)
        call ifanal ('F', 18, 1, kspare(9), xx2, fchsx1)
 
        if (lprsx1 .ne. 0 .or. fchsx1 .ne. 0) then
 
          lprtsw = lprsx1
          fichsw = fchsx1
 
          call title(2)
          ksw = 1
 
          call bsanrp  (ksw,subttl,lprsx1,fchsx1)
          lprtsw = lprsx1
          fichsw = fchsx1
 
          toto1 = total(5) + total(8)
          toto2 = total(6) + total(9)
          toto3 = total(7) + total(10)
                 write(outbuf,399)
  399     format(t37,'______',t46,'_____',t54,'_____',t62,'_____',t71,
     1    '_____',t80,'_____',t89,'_____')
                 call prtout(1)
 
          write (outbuf,400) (total(i),i=1,4),toto2,toto1,toto3
  400     format(11x,'TOTAL SYSTEM',10x,2f8.1,1x,2f8.1,3f9.1)
          call prtout (1)
 
          write (outbuf,410) total(6),total(5),total(7)
  410     format( 11x, 'TOTAL STATICS ', 41x, 3f9.1 )
          call prtout (1)
 
          write (outbuf,420) total(9),total(8),total(10)
  420     format(11x,'TOTAL REACTORS',41x,3f9.1)
          call prtout (1)
 
          write (outbuf,430) total(11)
  430     format(11x,'TOTAL GENERATORS',14x,f8.1)
          call prtout (1)
 
          write (outbuf,440) total(12)
  440     format(11x,'TOTAL CONDENSERS',14x,f8.1)
          call prtout (1)
 
        endif
C
C ***   LEVEL 4 ***********
C
C ***   SPINNING RESERVE
C
 
        call ifanal ('P', 19, 0, kspare(8), xx1, lprsx1)
        call ifanal ('F', 19, 0, kspare(9), xx2, fchsx1)
 
        if (lprsx1 .ne. 0 .or. fchsx1 .ne. 0) then
 
           lprtsw = lprsx1
           fichsw = fchsx1
 
           call title(15)
           do 832 i = 1,8
  832      subttl(i) = 0.0
 
           if (ntotc.gt.0) then
              do 900 i = 1,ntotc
              do 840 j = 1,8
  840         subtot(j) = 0.0
              write(outbuf,870)arcnam(i)
  870         format('0 ',a10)
              call prtout(1)
              izone = 0
 
              do 890 j = 1,nztot
              if (acznum(j).ne.i) go to 890
              izone = izone + 1
 
              do 872 k = 1,6
  872         subttl(k) = subttl(k) + zsum(8+k,j)
 
              x1 = zsum(9,j)-zsum(10,j)
              x2 = zsum(11,j)-zsum(12,j)-zsum(14,j)
              x3 = zsum(13,j)-zsum(12,j)-zsum(14,j)
              write (outbuf,880) acznam(j),zsum(9,j),zsum(10,j),x1,
     1        zsum(11,j),zsum(13,j),zsum(12,j),zsum(14,j),x2,x3
  880         format(4x,a2,5x,f14.1,2f12.1,2x,4f12.1,2f14.1)
              call prtout(1)
 
              do 850 k = 1,6
  850         subtot(k) = subtot(k) + zsum(8+k,j)
 
              x1 = subtot(1)-subtot(2)
              x2 = subtot(3)-subtot(4)-subtot(6)
              x3 = subtot(5)-subtot(4)-subtot(6)
  890         continue
 
              if (izone.gt.1) then
                 write(outbuf,891)
  891            format( t20,'______', t32,'______',t45,'_____',
     1                  t58,'______',t69,'_______',t82,'______',
     2                  t94,'______',t108,'______',t121,'_______' )
                 call prtout(1)
                 write(outbuf,892)subtot(1),subtot(2),x1,
     1           subtot(3),subtot(5),subtot(4),subtot(6),x2,x3
 892             format ('  SUBTOTAL  ',f13.1,2f12.1,2x,4f12.1,
     1                   2f14.1)
                 call prtout(1)
                 call space(1)
              endif
 
  900         continue
 
           else
              do 910 j = 1,nztot
              do 902 k = 1,6
  902         subttl(k) = subttl(k) + zsum(8+k,j)
              x1 = zsum(9,j)-zsum(10,j)
              x2 = zsum(11,j)-zsum(12,j)-zsum(14,j)
              x3 = zsum(13,j)-zsum(12,j)-zsum(14,j)
              write (outbuf,880)acznam(j),zsum(9,j),zsum(10,j),x1,
     1           zsum(11,j),zsum(13,j),zsum(12,j),zsum(14,j),x2,x3
              call prtout(1)
  910         continue
           endif
 
           x1 = subttl(1)-subttl(2)
           x2 = subttl(3)-subttl(4)-subttl(6)
           x3 = subttl(5)-subttl(4)-subttl(6)
           write(outbuf,911)
  911      format(t20,'______',t32,'______',t45,'_____',t58,
     1            '______',t69,'_______',t82,'______',t94,
     2            '______',t108,'______',t121,'_______')
           call prtout(1)
 
           write (outbuf,912) subttl(1),subttl(2),x1,subttl(3),
     1            subttl(5), subttl(4),subttl(6),x2,x3
  912      format('  TOTAL',4x,f14.1,2f12.1,2x,4f12.1,2f14.1)
           call prtout(1)
 
           write (outbuf,920)
  920      format('0 Notes:')
           call prtout(1)
 
           write (outbuf,922)
  922      format('0       1. Active Spinning Reserve excludes all ',
     1            'quantities pertaining to synchronous motors ',
     2            '(i.e., pumped storage plants).')
           call prtout(1)
 
           write (outbuf,925)
  925      format('0          All negative generation (synchronous ',
     1            'motor load) is interpreted as load.')
           call prtout(1)
 
           write (outbuf,924)
  924      format('0          This exclusion is invoked by requiring ',
     1            'PGEN and PMAX both be greater than zero.')
           call prtout(1)
 
           write (outbuf,926)
  926      format('0       2. Reactive Spinning Reserve excludes all ',
     1            'quantities pertaining to synchronous condensers.')
           call prtout(1)
 
           write (outbuf,928)
  928      format('0          This exclusion is invoked by requiring ',
     1            'both PGEN be greater than zero and the base be ',
     2            'greater than 30 KV.')

           call prtout(1)
 
        endif
 
C ***   TRANSMISSION  LINE EFFICIENCY  ANALYSIS * * * * * * * *
 
        call ifanal ('P', 20, 1, kspare(8), xx1, lprsx1)
        call ifanal ('F', 20, 1, kspare(9), xx2, fchsx1)
 
        if (lprsx1 .ne. 0 .or. fchsx1 .ne. 0) then
 
           lprtsw = lprsx1
           fichsw = fchsx1
C ***                                                                  *
C ***      Open analysis output file if option enabled.                *
C ***                                                                  *
           if (apfile(20) .ne. ' ' .and. apfile(20)(1:1) .ne. char(0))
     1        then
C ***                                                                  *
C ***         NUMREC keeps track of the total number of records in the
C ***         analysis output file.
C ***                                                                  *
              numrec = 0
              lun = 23
              close (unit=lun, err = 930)
              last = lastch (apfile(20))
  930         open (unit=lun, file = apfile(20)(1:last), status = 'NEW',
     1              err = 932)
              go to 936
  932         write (errbuf(1), 934) apfile(20)(1:last)
  934         format (' Analysis report file cannot be opened (',
     1                a, ')')
              call prterx ('W', 1)
              apfile(20) = ' '
              lun = 0
  936         continue
           else
              lun = 0
           endif
           fldsiz = apsize(20)
           call title(17)
           if (lun .ne. 0) then
               do 938 j = 1, 5
                  write (lun, 964) subhed(j)(1:fldsiz)
                  numrec = numrec + 1
  938          continue
           endif
           if (noleff.ne.0) then
               if(leffan.eq.0) then
                  leffan = 1
                  effan(1) = 'BPA'
               endif
               noleff = min0(noleff,MAXBRN/2)
               key = apsort(20)
C
C              Sort by KEY: 0 - Sort by bus names.
C                           1 - sort by voltage-bus names.
C                           2 - Sort by owner-bus names.
C                           3 - Sort by zone-bus names.
C
               oldvlt = 0.0
               oldown = ' '
               call qiksrt(1,noleff,kompef,swapef)
               do 940 kt = 1,noleff
               k1 = leff(2,kt)
               k2 = leff(3,kt)
               write (id,950) leff(4,kt)
 950           format (a1)
               isect = leff(5,kt)
               zn1 = zone(k1)
               zn2 = zone(k2)
               write (own,585) leff(1,kt)
               do 955 i = 1,leffan
               if (own.eq.effan(i)) go to 960
  955          continue
               go to 940
  960          if (lprtsw.ne.0.or.fichsw.ne.0) then
 
                   if (key .eq. 1) then
                      if (base(k1) .ne. oldvlt) then
                         write (outbuf, 962) base(k1)
  962                    format ('0 *** ', f6.1, ' BASE KV ***')
                         call prtout(1)
                         if (lun .ne. 0) then
                            write (lun, 964) outbuf(1:fldsiz)
  964                       format(a)
                            numrec = numrec + 1
                         endif
                         outbuf = ' '
                         call prtout(1)
                         if (lun .ne. 0) then
                            write (lun, 964) outbuf(1:fldsiz)
                         endif
                         oldvlt = base(k1)
                      endif
                   else if (key .eq. 2) then
                   endif
                   write (outbuf,980) leff(1,kt),zone(k1),bus(k1),
     1                base(k1),zone(k2),bus(k2),base(k2),id,isect,
     2                eff(6,kt),eff(12,kt),(eff(i,kt),i=7,11),
     3                eff(13,kt)
980                format(1x,a3,2x,a2,2x,a8,f7.1,2x,a2,2x,a8,f7.1,a4,i3,
     1                    f7.1,f8.0,f9.1,f8.1,f11.3,f12.3,f10.2,f10.1)
 
                   call prtout (1)
                   if (lun .ne. 0) then
                      write (lun, 964) outbuf(1:fldsiz)
                      numrec = numrec + 1
                   endif
                endif
 
940           continue
C ***                     
C ***         Close Analysis output file if enabled.
C ***                     
              if (lun .ne. 0) then
                 close (unit=lun, err = 942 )
  942            continue
                 last = lastch (apfile(20))
                 write (outbuf, 944) numrec, apfile(20)(1:last)
  944            format (1x, i4,
     1      ' records written to > Line Efficency Analysis file ', a)
                 call prtout (1)
                 apfile(20) = ' '
              endif
C
            else
              outbuf = '0       NO LINE EFFICIENCY CANDIDATES DETECTED'

              call prtout(1)
            endif
 
        endif
 
C ***   TRANSFORMER EFFICIENCY ANALYSIS - TOTAL LOSSES  *
 
        call ifanal ('P', 22, 1, kspare(8), xx1, lprsx1)
        call ifanal ('F', 22, 1, kspare(9), xx2, fchsx1)
 
        if (lprsx1 .ne. 0 .or. fchsx1 .ne. 0) then
 
           lprtsw = lprsx1
           fichsw = fchsx1
 
           if (ntxeff.ne.0) then
              if (txefan.eq.0) then
                 txefan = 1
                 xefan(1) = 'BPA'
              endif
              ntxeff = min0(ntxeff, 2*MAXOLT)
              call qiksrt(1,ntxeff,kotxef,swtxef)
              call title(18)
              do 1011 kt = 1,ntxeff
              if (txeff(8,kt).lt.ratxef) go to 1011
              k1 = ltxeff(2,kt)
              k2 = ltxeff(3,kt)
              write(id,950)ltxeff(4,kt)
              isect = ltxeff(5,kt)
              zn1 = zone(k1)
              zn2 = zone(k2)
              write(own,585)ltxeff(1,kt)
 
              do 990 j = 1,txefan
                 if (own.eq.xefan(j)) go to 992
  990         continue
              go to 1011
 
  992         if (lprtsw.ne.0.or.fichsw.ne.0) then
                 x = txeff(8,kt)
                 if(x.eq.0.0) then
                    corels = ' '
                 else
                    write (corels,993) x
  993               format(f8.2)
                 endif
                 x = txeff(9,kt)
                 if(x.eq.0.0) then
                    culoss = ' '
                 else
                    write (culoss,993) x
                 endif
                 x = (txeff(8,kt) + txeff(9,kt))
                 if (x.eq.0.0) then
                     totalc = ' '
                 else
                    write (totalc,993) x
                 endif
                 x = (txeff(8,kt)/txeff(7,kt))*100.0
                 if (x.eq.0.0) then
                    core = ' '
                 else
                    write(core,993) x
                 endif
                 x = txeff(9,kt)/txeff(7,kt)*100.0
                 if (x.eq.0.0) then
                    cu = ' '
                 else
                    write (cu,993) x
                 endif
                 x = (txeff(8,kt) + txeff(9,kt))/txeff(7,kt)*100.0
                 if (x.eq.0.0) then
                    tot = ' '
                 else
                    write (tot,993) x
                 endif
                 write(outbuf,1005) ltxeff(1,kt),zone(k1),bus(k1),
     1              base(k1),zone(k2),bus(k2),base(k2),id,isect,
     2              (txeff(i,kt),i=6,7),corels,culoss,totalc,core,cu,
     3              tot
 1005            format(1x,a3,2x,a2,2x,a8,f7.1,2x,a2,2x,a8,f7.1,a4,i3,
     1                  2f8.1,6a8)
                 call prtout(1)
              endif
 
 1011         continue
 1021         continue
           endif
 
        endif
 
C ***   TRANSFORMER EFFICIENCY ANALYSIS - CORE LOSSES  *
 
        call ifanal ('P', 23, 1, kspare(8), xx1, lprsx1)
        call ifanal ('F', 23, 1, kspare(9), xx2, fchsx1)
 
        if (lprsx1 .ne. 0 .or. fchsx1 .ne. 0) then
 
           lprtsw = lprsx1
           fichsw = fchsx1
 
           if (ntxeff.ne.0) then
              if (txefan.eq.0) then
                 txefan = 1
                 xefan(1) = 'BPA'
              endif
              ntxeff = min0(ntxeff,MAXOLT+MAXOLT/2)
              call qiksrt(1,ntxeff,kotxef,swtxef)
              call title(19)
              do 11011 kt=1,ntxeff
              if (txeff(9,kt).lt.ratcef) go to 11011
              k1 = ltxeff(2,kt)
              k2 = ltxeff(3,kt)
              write(id,950)ltxeff(4,kt)
              isect = ltxeff(5,kt)
              zn1 = zone(k1)
              zn2 = zone(k2)
              write(own,585)ltxeff(1,kt)
 
              do 10990 j = 1,txefan
                 if (own.eq.xefan(j)) go to 10992
10990         continue
              go to 11011
 
10992         if (lprtsw.ne.0.or.fichsw.ne.0) then
                 x = txeff(8,kt)
                 if(x.eq.0.0) then
                    corels = ' '
                 else
                    write (corels,10993) x
10993               format(f8.2)
                 endif
                 x = txeff(9,kt)
                 if(x.eq.0.0) then
                    culoss = ' '
                 else
                    write (culoss,10993) x
                 endif
                 x = (txeff(8,kt) + txeff(9,kt))
                 if (x.eq.0.0) then
                     totalc = ' '
                 else
                    write (totalc,10993) x
                 endif
                 x = (txeff(8,kt)/txeff(7,kt))*100.0
                 if (x.eq.0.0) then
                    core = ' '
                 else
                    write(core,10993) x
                 endif
                 x = txeff(9,kt)/txeff(7,kt)*100.0
                 if (x.eq.0.0) then
                    cu = ' '
                 else
                    write (cu,10993) x
                 endif
                 x = (txeff(8,kt) + txeff(9,kt))/txeff(7,kt)*100.0
                 if (x.eq.0.0) then
                    tot = ' '
                 else
                    write (tot,10993) x
                 endif
                 write(outbuf,11005) ltxeff(1,kt),zone(k1),bus(k1),
     1              base(k1),zone(k2),bus(k2),base(k2),id,isect,
     2              (txeff(i,kt),i=6,7),corels,culoss,totalc,core,cu,
     3              tot
11005            format(1x,a3,2x,a2,2x,a8,f7.1,2x,a2,2x,a8,f7.1,a4,i3,
     1                  2f8.1,6a8)
                 call prtout(1)
              endif
 
11011         continue
11021         continue
           endif
 
        endif
C
C       BUILD OWNERSHIP-LOSS TABLES.
C
        do 1140 i = 1,jowner
        iown = lstown(i)
        ownnam(i) = lowner(iown)
        ownlos(1,i) = 0.0
        ownlos(2,i) = 0.0
        do 1130 j = 1,jvolt
        jvlt = lstvlt(j)
        lov = kov(jvlt,iown)
        if (lov.gt.0) then
           do 1120 k = 1,5,2
           ownlos(1,i) = ownlos(1,i) + ovlos(k,lov)
           ownlos(2,i) = ownlos(2,i) + ovlos(k+1,lov)
 1120      continue
        endif
 1130   continue
 1140   continue
C
C       Set Build ownership loss flag
C
        ownflg = .true.
 
        lprtsw = 1
        if (kspare(16) .ge. 0) fichsw = 1
        outbuf = ' '
        call rpnlod
        call shdlod (1)
        call shdlod (2)
        call shdlod (3)
        call shdlod (4)
        call shdlod (5)
 
        outbuf = '0 END OF ANALYSIS LISTINGS '
        call prtout(1)
        call forbtm
 
C       RESTORE ORIGINAL PRINT/FICHE SETTINGS
 
        lprtsw=lprtsv
        fichsw=fichsx
 
        return
 
        end
