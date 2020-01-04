C    @(#)opsln1.f	20.19 10/20/99
      subroutine opsln1
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/agc.inc'
      include 'ipfinc/alpha.inc'
      include 'ipfinc/amtrx.inc'
      include 'ipfinc/arcntl.inc'
      include 'ipfinc/area.inc'
      include 'ipfinc/beta.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/bxlock.inc'
      include 'ipfinc/dc.inc'
      include 'ipfinc/dc2t.inc'
      include 'ipfinc/dcmt.inc'
      include 'ipfinc/ecvar.inc'
      include 'ipfinc/ikk.inc'
      include 'ipfinc/intbus.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/lndpcp.inc'
      include 'ipfinc/ltcsln.inc'
      include 'ipfinc/optim1.inc'
      include 'ipfinc/optim2.inc'
      include 'ipfinc/pctvr2.inc'
      include 'ipfinc/phase.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/slnopt.inc'
      include 'ipfinc/sort.inc'
      include 'ipfinc/svc.inc'
      include 'ipfinc/tbx.inc'
      include 'ipfinc/tbxsrt.inc'
      include 'ipfinc/tran.inc'
      include 'ipfinc/xdata.inc'
 
      common /auxslk/ nxslck,xslck(2,MAXREI+MAXTIE),
     &                kxslck(MAXREI+MAXTIE) 
      common /xinit/ xinit(MAXXDT)
      double precision xinit
c  
      common /itota/ itota  
c
      common /tran_phid/ tran_phid(MAXLTC)
      integer tran_phid
c
      common /is_batch / is_batch
c
      integer islack(2,10), itemp(MAXBUS), ptr
      real basenu(5), temp(MAXBUS)
      double precision tapnew(MAXLTC), dx
      character type*1, type2*1, busnu(5)*8
      logical found, reset_tap

      equivalence (itemp, amtrx), (temp, amtrx(MAXBUS+1))
      external kmpidx, swpidx, kmpjdx, swpjdx  
 
      call forbtm
      outbuf = ' * * * DIAGNOSTICS * * * '
      call rpnlod
      outbuf = ' '
      do i = 1, 5
         call shdlod(i)
      enddo
      call fortop
      call space(2)
      koptsw=1
C
C     ITSW assignments: 0 - off
C                       1 - Type R on (d-c commutating LTC's only),
C                           Types RM, RN, RP, RQ on
C                       2 - on
C                       3 - Type R on, Types RM, RN, RP, RQ off
C                       4 - Type R on (d-c commutating LTC's only).
C
      itsw=iopton(16)
      if (ntota.eq.0) itsw=0
      iasw=0
      if (iopton(17).eq.1.and.jtie.gt.0.and.ntotc.gt.0) iasw=1  
      idcsw=0   
      if (idckt.gt.0) idcsw = 1 
      idswa=iopton(12)  
      idswb=iopton(13)  
      idswc=iopton(14)  
      ntotx=ntot+ntota+1
C       
C     Determine slack bus indices for each node 
C       
      do kt = 1,ntot
         nsysno(kt) = -1
      enddo

      nindex = 0
      isystm = 0
      do while (nindex .lt. ntot)
        do kt = 1,ntot
          if (nsysno(kt) .lt. 0) go to 150
        enddo
        call erexit()

  150   isystm = isystm + 1   
        nsysno(kt) = 0
        istart = nindex + 1   
        nindex = istart   
        ikkind(1,nindex) = kt 
        do while (nindex .lt. ntot .and. nindex .le. istart)
          kt = ikkind(1,nindex) 
          if (ntypu(kt) .eq. 3) isystm = kt             
          do l = km(kt), km(kt)-1+kmlen(kt)   
            mt = ikmu(l)                                 
            if (nsysno(mt) .lt. 0) then
              nsysno(mt) = 0
              istart = istart + 1   
              ikkind(1,istart) = mt 
            endif
          enddo
          nindex = nindex + 1   
        enddo

        if (isystm .eq. 0) then
          write (errbuf(1),200) intbus(kt),intbas(kt)   
  200     format (' Bus ',a8,f6.1,' has no subsystem slack bus ')   
          call prterx ('W',1)   
        endif
       
        do kt = 1,ntot
          if (nsysno(kt) .eq. 0) nsysno(kt) = isystm
        enddo

      enddo

      p1sked=0.0
      ntot11=0  
      itota=0   
      itotb=0   
      nxslck = 0

C     initialize arrays   

      do i=1,ntotx  
        do j=1,5
           ikk(j,i)=0  
        enddo
        ikkind(1,i)=1   
        ikkind(2,i)=1 
      enddo
      nindxx=0  
      njndxx=0  

C     Initialize ikk array 

      ntopt = 0 
      do 350 kt=1,ntot  
      if (ntypu(kt).ne.3) p1sked = p1sked + pnetu(kt) 
      isystm = nsysno(kt)   
      if (kt.le.nbslck) isystm = kt 
c
c     Catch deleted buses and passive d-c buses here.
c
      if (kmlen(kt) .eq. 0) then
         isystm = 1
         go to 340 
      endif
      go to (260,330,254,260,260,260,330,330,280,260,330,260,330,   
     1       252,252,252) ntypu(kt)                

  252 nb = opt2inp(kt)
      nxslck = nxslck + 1   
      kxslck(nxslck) = kt   
      xslck(1,nxslck) = busdta(11,nb)   
      xslck(2,nxslck) = busdta(12,nb)   
      njndxx=njndxx+1   
      jndx(1,njndxx)=kt+ntota   
      jndx(2,njndxx)=9  
      jndx(3,njndxx)=nxslck 
      if (ntypu(kt) .eq. 14) then
         ikk(1,kt+ntota)=2  
      else  
         ikk(1,kt+ntota)=1  
      endif 
      go to 340 
C       
  254 nb = opt2inp(kt)
      nxslck = nxslck + 1   
      kxslck(nxslck) = kt   
      xslck(1,nxslck) = busdta(11,nb)   
      xslck(2,nxslck) = busdta(12,nb)   
      njndxx=njndxx+1   
      jndx(1,njndxx)=kt+ntota   
      jndx(2,njndxx)=9  
      jndx(3,njndxx)=nxslck 
      go to 280 
        
  260 ikk(1,kt+ntota)=2 
      go to 340 
C       
  280 ntopt=ntopt+1 
      if (ntopt.le.600) go to 300
      write (errbuf(1),290) 
  290 format(' More than 600 optimal busses in system.')
      call prterx ('W',1)   
      ntopt=600 
C       
  300 kvgrad(1,ntopt)=kt
      do j=2,5  
         vgrad(j,ntopt)=0.0
      enddo
      if (ntypu(kt) .eq. 3) then
        ikk(1,kt+ntota)=1 
        njndxx=njndxx+1   
        jndx(1,njndxx)=kt+ntota   
        jndx(2,njndxx)=6  
        jndx(3,njndxx)=ntopt  
      else
        ikk(1,kt+ntota)=1 
      endif
      go to 340 
       
  330 ikk(1,kt+ntota)=1 
       
  340 continue  

      if (iopton(18) .gt. 0) then
         angle = slkxx(4,isystm)   
         nb=opt2inp(kt)  
         v=vstart(nb)  
         e(kt) = v*cos(angle)  
         f(kt) = v*sin(angle)  
      endif
  350 continue  
C       
      if (ntota .gt. 0 .and. koptsw .ge. 3) then
         do j=1,ntota
            do k=1,3
               tgrad(k,j)=0.0
            enddo
         enddo
      endif
C
C     Check correctness of multiple slack busses
C       
      do i=1,nbslck 
         k=nslkxx(1,i) 
         j=inp2opt(k)
         if (j .gt. nbslck) then
            write (errbuf(1),380) intbus(j),intbas(j) 
  380       format(' Slack bus ',a8,f7.1,
     &          ' is not in array NSLCKK (OPSLN1)')  
            if (is_batch .eq. 0) then
               call prterx ('E',1)
            else
               call prterx ('F',1)
            endif
         endif
         islack(1,i)=j 
         islack(2,i)=0 
      enddo
c       
c     Process special bus arrays:   
c       
c       1. Optionally restore to initial state   
c       2. Check data
c       3. Establish remote generator controls
c
      do i=1,ntot
         nsysno(i)=0
      enddo
 
      if (ntotb.eq.0) go to 940
      do 930 jt = 1,ntotb
      ltyp=tbx(1,jt)
      ityp=tbx(7,jt)
      kxdt=tbx(5,jt)   
      if (ltyp.ge.10) go to 930
      itotb=itotb+1
      kt=tbx(2,jt)
      njndxx=njndxx+1
      jndx(1,njndxx)=kt+ntota
      jndx(2,njndxx)=8
      jndx(3,njndxx)=jt
      nsysno(kt)=jt
      mt=tbx(8,jt)
      if (mt.ge.0) go to 420
      mt = -mt  
      tbx(8,jt) = mt   
C       
  420 continue  
      kta = kt + ntota  
      mta=mt+ntota  
      if (iopton(18).eq.0) go to 530
C       
C     Re-initialize sensitivities dV/dQ.
C       
      ltbxsl(1,jt) = 0  
      tbxsrt(jt) = 0
C       
C     Restore busses to normal state
C       
      go to (430,440,510,440,450,432) ltyp  
C       
  430 go to (530,530,460,460) ityp  
        
  432 go to (530,460) ityp  
C       
  440 go to (520,520,470,480) ityp  
C       
C     Restore BX bus only if XBUS = WSCC option is not enabled. 
C       
C     KSPARE(24) = 0 - Normal (BPA) option. Bias PV state to V_max, 
C                      then freeze at nearest discrete step.
C                  1 - WSCC option. Bias discrete step to original  
C                      shunt while observing V_limits.  
C                  2 - VMAX option.  Same as BPA, except continue   
C                      adjusting discrete step to bias voltage to   
C                      v_max.   
C       
  450 xinit(kxdt) = xdata(5,kxdt) + xdata(6,kxdt)   
C       
C     If locked BX bus, do not reinitialize XDATA.  
C       
      do 452 i = 1, numlck  
        if (inp2opt(nxlock(i)) .eq. kt) then  
           go to 530
        endif   
  452 continue  
C       
C     If X_BUS = WSCC, do not reinitialize XDATA.   
C       
      if (kspare(24) .eq. 1) go to 530  
      go to (520,500,500,500,500) ityp  
C       
C     RESTORE BV AND BF BUS 
C       
  460 qnetu(kt) = tbx(5,jt)
      go to 510 

C     restore BQ and BO bus

  470 continue  
      bkku(kt) = bkku(kt) + dmax1(0.0d0,tbx(6,jt))  
      go to 510 
C       
  480 continue  
      bkku(kt) =  bkku(kt) + dmin1(0.0d0,tbx(6,jt)) 
      go to 510 
C       
C     restore  BX bus   
C       
  500 totrek=xdata(3,kxdt)  
      totcap=xdata(4,kxdt)  
      userek=xdata(5,kxdt)  
      usecap=xdata(6,kxdt)  
      bkku(kt) = bkku(kt) +(totrek + totcap - userek - usecap)/bmva   
      xdata(5,kxdt)=totrek  
      xdata(6,kxdt)=totcap  
        
  510 ityp = 1  
      tbx(7,jt) = ityp 
        
  520 if (ltyp.ne.4) go to 530  
      vsq = (e(kt)**2 + f(kt)**2)-tbx(5,jt)**2  
      tbx(5,jt) = dsqrt (e(kt)**2+f(kt)**2)  
      tbx(3,jt) = tbx(3,jt) - vsq * dmin1(0.0d0,tbx(6,jt))  
      tbx(4,jt) = tbx(4,jt) - vsq * dmax1(0.0d0,tbx(6,jt))  
  530 continue  
C       
C     Determine state of special buses  
C       
      go to (540,600,630,560,620,550), ltyp 
C       
  540 if (ityp.le.2) go to 660  
      ikk(1,kt+ntota)=1 
      go to 660 
C       
  550 if (ityp.eq.1) go to 660   
      ikk(1,kt+ntota)=2 
      go to 660 
C       
  560 if (koptsw.eq.1) go to 590
      do 570 i=1,ntopt  
      if (kvgrad(1,i).eq.kt) go to 580  
  570 continue  
      call erexit   
  580 kvgrad(2,i)=jt
  590 if (ityp.gt.2) go to 610  
      ityp=1
      tbx(7,jt) = ityp 
      go to 660 
C       
  600 if (ityp.le.2) go to 660  
  610 ikk(1,kt+ntota)=2 
      go to 660 
C       
  620 ntot11=ntot11+1   
  630 if (mt.eq.0) mt=kt 
      mta=mt+ntota  
      if (ltyp .eq. 3) then 
         go to (660, 610, 610, 640, 640) ityp   
      else if (ltyp .eq. 5) then
C       
C        If locked BX bus, do not reinitialize XDATA.   
C       
         do 632 i = 1, numlck   
            if (inp2opt(nxlock(i)) .eq. kt) then  
               go to (660, 610, 610, 610) ityp  
            endif   
  632    continue   
C       
C        If X_BUS = WSCC, do not reinitialize XDATA.
C       
         if (kspare(24) .ne. 1) go to 650   
         go to (660, 610, 610, 610) ityp
      else  
         call erexit
      endif 
C       
  640 ityp = 1  
      tbx(7,jt) = ityp 
      go to 660 
        
  650 ityp = 1  
      tbx(7,jt) = ityp 
      kxdt = tbx(5,jt)  
      totrek=xdata(3,kxdt)  
      totcap=xdata(4,kxdt)  
      userek=xdata(5,kxdt)  
      usecap=xdata(6,kxdt)  
      bkku(kt) = bkku(kt) + (totrek + totcap - userek - usecap)/bmva  
      xdata(5,kxdt)=totrek  
      xdata(6,kxdt)=totcap  
      ikk(1,kt+ntota)=1 
        
  660 if (tbx(4,jt).le.tbx(3,jt)) go to 680 
      if (ltyp .eq. 1 .or. ltyp .eq. 6) then
         write (errbuf(1),662) intbus(kt), intbas(kt), tbx(3,jt),
     1                         tbx(4,jt)   
  662    format(' Bus ',a8,f7.1,' has VMAX (',f6.3,') < VMIN (',   
     1         f6.3,'). These quantities have been exchanged.') 
         call prterx ('W',1)   
      else
         write (errbuf(1),670) intbus(kt), intbas(kt), tbx(3,jt),
     1                         tbx(4,jt)   
  670    format(' Bus ',a8,f7.1,' has QMAX (',f6.3,') < QMIN (',   
     1         f6.3,'). These quantities have been exchanged.') 
         call prterx ('W',1)   
      endif
      qmax=tbx(3,jt)
      tbx(3,jt)=tbx(4,jt)   
      tbx(4,jt)=qmax
  680 if (ltyp.ne.4) go to 720  
      vmin=vlimn(kt)                              
      vmax=vlimx(kt)                              
      if (vmax.gt.vmin) go to 720   
      if (vmin .ne. vmax) go to 700 
      write (errbuf(1),690)intbus(kt),intbas(kt)
      call prterx ('W',1)   
  690 format(' Optimal bus ',a8,f7.1,' has VMIN = VMAX (',f6.3,')') 
  700 if (vmin.le.vmax) go to 720   
      vlimn(kt) = vmax
      vlimx(kt) = vmin
      write (errbuf(1), 710) intbus(kt), intbas(kt), vlimx(kt), 
     1                      vlimn(kt)  
      call prterx ('W', 1)   
  710 format(' Optimal bus ', a8,f7.1,'has VMAX (',f6.3,') < VMIN (',
     1 f6.3,').')   
  720 if (ltyp.eq.3) go to 730  
      if (ltyp.ne.5) go to 930  
      if (kt.eq.mt) go to 930   
C       
  730 continue  
      emin = vlimn(mt)                            
      emax = vlimx(mt)                            
      if (emin.gt.emax) then
         write (errbuf(1),740) intbus(mt),intbas(mt), intbus(kt),   
     &     intbas(kt),emin,emax 
  740    format(' Bus ',a8,f7.1,' controlled by generator ',a8,f7.1,
     &    ' has exchanged improper limits: VMIN (',f6.3,') > VMAX (',
     &      f6.3,').') 
         call prterx ('W',1)
         vlimx(mt) = emin 
         vlimn(mt) = emax 
      else if (emin .lt. emax .and. ntypu(mt) .ne. 5 .and. !uua
     &    ntypu(mt) .ne. 12)   then                !uua
         write (errbuf(1),742) intbus(mt),intbas(mt), intbus(kt),   
     &     intbas(kt),emin,emax 
  742    format(' Bus ',a8,f7.1,' controlled by ',a8,f7.1,
     &    ' within v-limits: VMIN (',f6.3,' < VMAX (',
     &    f6.3,'). ')  
         call prterx ('I',1)
      endif 
C       
  750 continue  
  760 if (mt.le.nbslck.or.mt.eq.kt) go to 770
      go to 790 
C       
  770 write (errbuf(1),780) intbus(kt),intbas(kt),intbus(mt),intbas(mt)

  780 format(' Generator bus ',a8,f7.1,' controlling ', 
     &  ' bus ', a8, f7.1,' is self-regulating.') 
      call prterx ('I',1)   
      go to 920 
C       
  790 if (abs(bkku(kt)) .le. 500.0) go to 810  
      write (errbuf(1),800) intbus(kt),intbas(kt),intbus(mt),intbas(mt)

  800 format(' Generator bus ',a8,f7.1,' controlling bus ', a8, f7.1,
     &  ' will use indirect control because of bus-tie connection') 
      call prterx ('W',1)   
      tbx(8,jt)=-mt
  810 continue  
      if (tbx(3,jt).gt.tbx(4,jt)) go to 830 
      if ((ltyp.eq.5) .and.
     &  (xdata(3,kxdt) .ne. 0.0 .or. xdata(4,kxdt) .ne. 0.0)) go to 830


      write (errbuf(1),820) intbus(kt),intbas(kt),intbus(mt),intbas(mt)

  820 format(' Generator bus ',a8,f7.1,' controlling  ',a8,f7.1,' has fi
     &xed reactive limits. ')
      write (errbuf(2),822) 
  822 format(' Remote voltage control aborted.')
      call prterx ('W',2)   
      go to 920 
C       
  830 if (tbx(8,jt) .lt. 0d0) go to 918 
C       
C     Check proximity of mt to kt to determine direct or indirect   
c     control   
C       
      l1 =  0   
      level = 0 
      l2 = 1
      l3 = 1
      itemp(1) = kt  
  840 l1 = l1 + 1   
      if (l1.gt.l2) go to 870   
      kx = itemp(l1) 
      do 860 l = km(kx), km(kx)-1+kmlen(kx)   
         mx = ikmu(l)                                
         do 850 i = 1,l3   
            if (itemp(i).eq.mx) go to 860  
  850    continue  
         if (mx.eq.mt) go to 880   
         l3 = l3 + 1   
         itemp(l3) = mx 
  860 continue  
      go to 840 
C       
  870 level = level + 1 
      if (level.gt.2) go to 880 
      l1 = l2   
      l2 = l3   
      if (l1.le.ntot) go to 840 
  880 if (level.le.1) go to 910 
      write (errbuf(1),890) intbus(kt),intbas(kt),level,intbus(mt), 
     &       intbas(mt)
  890 format(' "BG" bus ',a8,f7.1,' is more than ',i2,' buses away',
     &       ' from controlled bus ',a8,f7.1,'.')  
      if (level.gt.1) write (outbuf(61:),900)   
  900 format('- Voltage indirectly controlled.')
      call prterx ('W',1)   
      if (level.le.1) go to 910 
      tbx(8,jt)  = -mt 
      go to 918 
C       
  910 continue  
      nindxx=nindxx + 1 
      indx(1,nindxx)= kt + ntota
      indx(3,nindxx)= mt + ntota
      indx(2,nindxx)= 1 
      nindxx=nindxx + 1 
      indx(1,nindxx)= mt + ntota
      indx(3,nindxx)= kt + ntota
      indx(2,nindxx)= 2 
      ikk(4,mta)=4  
      go to 920
c
c     Dummy entry to flag manual BG -> BC controls
c
C     Note:  INDX(1,*)  INDX(2,*)  INDX(3,*)  Meaning   
C               KT       101         MT       BG -> BC (manually)
C               KT       102         MY       BC <- BG (manually)
C       
  918 continue  
      nindxx=nindxx + 1 
      indx(1,nindxx)= kt + ntota
      indx(3,nindxx)= mt + ntota
      indx(2,nindxx)= 101 
      nindxx=nindxx + 1 
      indx(1,nindxx)= mt + ntota
      indx(3,nindxx)= kt + ntota
      indx(2,nindxx)= 102 

  920 continue  
  930 continue  
  940 continue  
c       
c     Process ltc arrays:   
c       
c       1. Reset taps to mid range   
c       2. Check data
c       3. Establish remote generator control
C       
C     Check LTC sensitivities (Note: E,F converted to polar coordinates)
C       
      if (itsw .eq. 2 .or. itsw .eq. 3 .or. itsw .eq. 4) call ltcint

      if (idswa .gt. 0) then
        write (dbug, 10010)
10010   format ('0 LTC starting taps ', /, '  LTC Bus1            Bus2
     &             Tap1    Tap2    Tmin    Tmax', /)
      endif        
      do 1340 jt=1,ntota
      ksw=0 
      kt=ltran(1,jt)
      mt=ltran(9,jt)
      tapnew(jt) = tap(jt)  
      nt=ltran(2,jt)
      ityp = mod(ltran(10,jt),100)  
      lt = ltran(10,jt)/100 
      itota=itota+1 
      ltcsln(jt) = 0
      if (itsw .eq. 0) go to 1320   
C       
C     Restore previously disabled LTC's.
C       
      if (ityp .gt. 20) then
         ityp=mod(ityp,10)  
         ltran(10,jt) = ityp + 100 * lt
         go to 1320 
      else if (ityp .gt. 10) then
         ityp=mod(ityp,10)  
         ltran(10,jt) = ityp + 100 * lt
      endif 
        
      if (ityp .eq. 1 .or. ityp .eq. 4 .or. ityp .eq. 5) then   
         if (itsw .eq. 2 .or. itsw .eq. 3) then 
         else if (itsw .eq. 4) then 
C       
C           Examine if controlled bus is a d-c commutating bus. 
C       
            if (ntypu(kt) .ne. 5 .and. ntypu(kt) .ne. 12) then
               go to 1320   
            endif   
         endif  
      endif 
      go to (1030,1170,1170,1030,1030) ityp 
 1030 if (nt .gt. 0) then   
         write (errbuf(1), 1050) intbus(kt),intbas(kt),intbus(mt),  
     1    intbas(mt),intbus(nt),intbas(nt)  
 1050    format(' LTC transformer ',a8,f6.1,2x,a8,f6.1,
     &          ' is indirectly controlling remote bus ',a8,f6.1) 
         call prterx ('W',1)
         ltcsln(jt) = 1 
         ksw = 1
      endif 
C       
      if (tran(7,jt) .le. tran(8,jt)) then  
         write (errbuf(1),960) intbus(kt),intbas(kt), intbus(mt),   
     1    intbas(mt),tran(8,jt),tran(7,jt)  
  960    format(' LTC ',a8,f7.1,1x,a8,f7.1,' HAS TMIN (',f6.3,
     &          ') > TMAX (',f6.3,')')
         call prterx ('I',1)
      endif 
C       
      dtap = ddim(tap(jt),dble(tran(7,jt))) 
     &     - ddim(dble(tran(8,jt)),tap(jt))  
      if (abs(dtap) .ge. 1.0e-3) then   
         write (errbuf(1),980) intbus(kt),intbas(kt),intbus(mt),
     1    intbas(mt),tap(jt),tran(7,jt),tran(8,jt)  
  980    format (' LTC ',a8,f6.1,1x,a8,f6.1,' STARTING TAP ',   
     1 f6.3,' VIOLATES LIMITS (',f6.3,',',f6.3,'). LTC DISABLED.')  
         call prterx ('W',1)
         go to 1320 
      endif 
C       
      if (nt.eq.-1) nt = kt 
      if (nt.eq.-2) nt = mt 
      go to (1100,1100,1090,1100,1100,1100,1100,1070,1100,1100,1070,
     1       1100,1100,1100,1100,1100) ntypu(nt)   
 1070 i = nsysno(nt)
      if (tbx(8,i) .ne. nt .and. tbx(8,i) .ne. 0d0) then
         call typno (type,ntypu(nt))               
         write (errbuf(1),1080) intbus(kt),intbas(kt),intbus(mt),   
     1     intbas(mt),intbus(nt),intbas(nt),type
 1080    format(' LTC  ',a8,f7.1,1x,a8,f7.1,' cannot control ',
     1     a8,f7.1,' ( is sub-type ',a1,'). LTC control disabled.')
         call prterx ('W',1)
         ksw = 1
      endif 
      go to 1100

 1090 call typno (type,ntypu(nt))                  
      write (errbuf(1),1080) intbus(kt),intbas(kt),intbus(mt),  
     1   intbas(mt),intbus(nt),intbas(nt),type  
      call prterx ('W',1)   
      ksw = 1   
 1100 if (vlimn(nt) .gt. vlimx(nt)) then        
         write (errbuf(1),1110)intbus(kt),intbas(kt),intbus(mt),
     1      intbas(mt),intbus(nt),intbas(nt),vlimn(nt),vlimx(nt)
 1110    format(' LTC ',a8,f7.1,1x,a8,f7.1,' controls ',a8,f7.1,
     1' with vmin (',f6.3,') > vmax (',f6.3,').')   
         call prterx ('I',1)
         vlimn(nt) = vmax 
         vlimx(nt) = vmin 
      else if (vlimn(nt) .lt. vlimx(nt) .and. ntypu(nt)!uua
     1    .ne. 5 .and. ntypu(nt) .ne. 12) then     !uua
         write (errbuf(1),1112)intbus(kt),intbas(kt),intbus(mt),
     1      intbas(mt),intbus(nt),intbas(nt),vlimn(nt),vlimx(nt)
 1112    format(' LTC ',a8,f7.1,1x,a8,f7.1,' controls ',a8,f7.1,
     1      ' with vmin (',f6.3,') < vmax (',f6.3,').')   
         call prterx ('I',1)
      endif 
C       
C     Test sensitivity  
C       
      if (ksw .eq. 0 .and.  
     1   (itsw .eq. 2 .or. itsw .eq. 3 .or. itsw .eq. 4)) then  
         call ltcsen (jt,txsen) 
         if (txsen .eq. 0.0) then   
C       
C           Set LTC to permanent manual control because of an   
C           open reactive circuit.  
C       
            if (ntypu(kt) .ne. 5 .and. ntypu(kt) .ne. 12 .and.
     &          ntypu(mt) .ne. 5 .and. ntypu(mt) .ne. 12) 
     &         ltcsln(jt) = 2  
         else   
C       
C           Exempt d-c commutation LTC's from manual control.   
C       
            if (ntypu(kt) .ne. 5 .and. ntypu(kt) .ne. 12 .and.
     1          ntypu(mt) .ne. 5 .and. ntypu(mt) .ne. 12) then
               if (kmlen(kt) .eq. 1 .or. kmlen(mt) .eq. 1) then     
C       
C                 LTC is set to manual control because of a radial  
C                 circuit.  This diagnostic is replicated in LTCSUM.
C       
                  ltcsln(jt) = 4
               else 
                  la1=ltran(3,jt)   
                  if (abs (bkmu(la1)) .gt. 500.0 .or. 
     &                abs (bkku(kt)) .gt. 1000.0 .or.
     &                abs (bkku(mt)) .gt. 1000.0) then
C       
C                    LTC is set to manual control because of low TX 
C                    impedance. This diagnostic is replicated in
C                    LTCSUM.
C       
                     ltcsln(jt) = 3 
                  endif 
               endif
            endif   
         endif  
      endif 
C       
C     Set flags for T(JT) --> V(NT) 
C       
      if (ksw .eq. 0) then  
         nindxx=nindxx+1
         indx(1,nindxx)=jt  
         indx(2,nindxx)=3   
         indx(3,nindxx)=nt+ntota
         nindxx=nindxx+1
         indx(1,nindxx)=nt+ntota
         indx(2,nindxx)=4   
         indx(3,nindxx)=jt  
      endif 
C       
C     Set optimization flags
C       
      if (ityp .eq. 4 .and. koptsw .ge. 3 .and. ksw .eq. 0) then
         ikk(1,jt)=1
         njndxx=njndxx+1
         jndx(1,njndxx)=jt  
         jndx(2,njndxx)=6   
         jndx(3,njndxx)=jt  
      else if (ksw .eq. 0) then 
         ikk(1,jt) = 1  
      else  
         ikk(1,jt) = 0  
      endif 
      go to 1190
C       
C     LTC phase shifter or var control  
C       
C     Check Pmax, Pmin limits   
C       
 1170 if (tran(4,jt) .lt. tran(5,jt)) then  
         write (errbuf(1),1172) intbus(kt),intbas(kt), intbus(mt),  
     1      intbas(mt),tran(4,jt),tran(5,jt)
 1172    format(' LTC ',a8,f7.1,1x,a8,f7.1,' has PMAX (',f6.3,  
     1      ') < pmin (',f6.3,'). limits have been interchanged.')  
         call prterx ('W',1)
         x = tran(4,jt) 
         tran(4,jt) = tran(5,jt)
         tran(5,jt) = x 
      endif 
C       
C     Check for radial system   
C       
      kix = min0(kmlen(kt), kmlen(mt))              
      if (kix .le. 1) then  
         write (errbuf(1),1180) intbus(kt),intbas(kt),intbus(mt),   
     1      intbas(mt)  
 1180    format(' LTC phase shifter ',a8,f7.1,2x,a8,f7.1,' is in a ',
     1   'radial circuit and is set to manual control.')
         call prterx ('W',1)
         ltcsln(jt) = 4 
      endif 
      ikk(1,jt)=1   
      if (itsw .eq. 0 .or. itsw .eq. 3 .or. itsw .eq. 4) then   
         ksw = 1
         go to 1320 
      endif 
C       
C     Flag dependency T(JT) <--> P(KT) and T(JT) <--> P(MT) 
C       
 1190 njndxx=njndxx+1   
      jndx(1,njndxx)=kt+ntota   
      jndx(2,njndxx)=2  
      jndx(3,njndxx)=jt 
      njndxx=njndxx+1   
      jndx(1,njndxx)=mt+ntota   
      jndx(2,njndxx)=2  
      jndx(3,njndxx)=jt 
      if (itsw .eq. 1 .and. ityp .eq. 1) go to 1340 
C       
C     Set LTC taps if Flat Start
C       
      if ( ityp .eq. 1) then
         reset_tap = (iopton(18) .eq. 1 .or. 
     &               (ntypu(nt) .eq. 5 .or. ntypu(nt) .eq. 12)) 
      else
         reset_tap = (iopton(18) .eq. 1) 
      endif
      if (reset_tap) then
         if ( ityp .ne. 3 ) then
            tmax = tran(6,jt) / tran(8,jt)   
            tmin = tran(6,jt) / tran(7,jt)   
            if (intbas(kt) .eq. 500.0 .or. intbas(mt) .eq. 500.0) then
               if (option(35) .le. 1.0) then   
                  if (intbas(kt) .lt. intbas(mt)) then 
                     tapnew(jt) = option(35) * tmax
     1                          + (1.0 - option(35)) * tmin
                  else 
                     tapnew(jt) = (1.0 - option(35)) * tmax
     1                          + option(35) * tmin
                  endif
                  tapnew(jt) = tran(6,jt) / tapnew(jt) 
               else
                  tapnew(jt) = vstart(opt2inp(kt)) / vstart(opt2inp(mt))
                  tapnew(jt) = dmin1( dble(tran(7,jt)), tapnew(jt) ) 
                  tapnew(jt) = dmax1( tapnew(jt), dble(tran(8,jt)) ) 
               endif   
            else 
               if (option(40) .le. 1.0) then   
                  if (intbas(kt) .lt. intbas(mt)) then 
                     tapnew(jt) = option(40) * tmax
     1                          + (1.0 - option(40)) * tmin
                  else 
                     tapnew(jt) = (1.0 - option(40)) * tmax
     1                          + option(40) * tmin
                  endif
                  tapnew(jt) = tran(6,jt) / tapnew(jt) 
               else
                  tapnew(jt) = vstart(opt2inp(kt)) / vstart(opt2inp(mt))
                  tapnew(jt) = dmin1( dble(tran(7,jt)), tapnew(jt) ) 
                  tapnew(jt) = dmax1( tapnew(jt), dble(tran(8,jt)) ) 
               endif   
            endif
            if (idswa .ne. 0) then
               tap1 = tran(6,jt) * intbas(kt)
               tap2 = tran(6,jt) / tapnew(jt) * intbas(mt)
               tmax = tran(6,jt) / tran(8,jt) * intbas(mt)
               tmin = tran(6,jt) / tran(7,jt) * intbas(mt)
               write (dbug, 10020) jt, intbus(kt), intbas(kt), 
     &           intbus(mt), intbas(mt), tap1, tap2, tmax, tmin
10020          format (1x, i4, 1x, a8, f7.1, 1x, a8, f7.1, 4f8.2)
            endif
         else   
C       
C           Phase Shifter - observe PHASESHIFTER_BIAS = BPA or WSCC:
C           
C           BPA: Set to tap closest to 0.0  
C           WSCC: Set to tap closest to original tap
C       
            if (iopton(21) .eq. 0) then
               tapnew(jt) = 0
            else
               i = 1
               found = .false.
               do while (i .le. jphno .and. .not. found)
                  if (inp2opt(jphid(1,i)) .eq. kt .and.
     &                inp2opt(jphid(2,i)) .eq. mt) then
                     tran_phid(jt) = i
                     found = .true.
                  else if (inp2opt(jphid(1,i)) .eq. mt .and.
     &                inp2opt(jphid(2,i)) .eq. kt) then
                     tran_phid(jt) = -i
                     found = .true.
                  else
                     i = i + 1
                  endif
               enddo
               i = tran_phid(jt)
               if (i .gt. 0) then
                  tapnew(jt) = phid(5,i)
               else if (i .lt. 0) then
                  tapnew(jt) = -phid(5,i)
               else
                  tapnew(jt) = 0.0
               endif                  
            endif
            tapnew(jt) = dmin1( dble(tran(7,jt)), tapnew(jt) )
            tapnew(jt) = dmax1( tapnew(jt), dble(tran(8,jt)) )
            if (tapnew(jt) .ne. 0.0 .and. iopton(21) .eq. 0) then   
               write (errbuf(1),1200) intbus(kt), intbas(kt),   
     1            intbus(mt), intbas(mt), tapnew(jt) * 57.295   
 1200          format(' LTC phase shifter ',a8,f7.1,2x,a8,f7.1, 
     1            ' starting tap ',f8.1,' is non-zero by bounding') 
               call prterx ('W',1)  
            endif   
         endif  
      endif 
C       
C     Defer updating taps until TIE and TXTIE arrays have been linked
C     with LTC's.   
C       
      go to 1340
C       
 1320 continue  
      itota=itota-1 
      ltran(10,jt) = ityp+20 + 100*lt
      ikk(1,jt) = 0 
      minerr=1  
 1330 continue  
 1340 continue  
C       
C     Restore E,F to rectangular coordinates
C       
      if (itsw .eq. 2 .or. itsw .eq. 3 .or. itsw .eq. 4) call ltcfin
        
 1350 continue  
C       
C     Build TXTIE array of phase shifters using updated status of   
C     LTCs. 
C       
      z_threshold = 0.0005
      call bldxti(z_threshold)
C       
C     Build "TBX-TRAN" array of integrated LTCs and TBX buses.  
C       
      call bldxtb   
C       
C     Link TXTIE with LTC, busses   
C       
      do 1352 i = 1,ntxtie  
C       
C     Flag dependency TXTIE <--> busses 
C       
      k1 = txtie(1,i)  
      k2 = txtie(2,i)  
      ltc = txtie(7,i) 
      njndxx=njndxx+1   
      jndx(1,njndxx)=k1+ntota   
      jndx(2,njndxx)=10 
      jndx(3,njndxx)=i  
C       
C     Flag dependency LTC's <--> TXTIE  
C       
      if (ltc .gt. 0) then  
        
         njndxx=njndxx+1
         jndx(1,njndxx)=ltc 
         jndx(2,njndxx)=11  
         jndx(3,njndxx)=i   
        
      else if (ltc .lt. 0) then 
        
         njndxx=njndxx+1
         jndx(1,njndxx)=-ltc
         jndx(2,njndxx)=12  
         jndx(3,njndxx)=i   
        
      endif 
        
 1352 continue  
C       
C     Link up control schemes   
C       
      if (nindxx.eq.0) go to 1450   
      call qiksrt (1,nindxx,kmpidx,swpidx)  
      k1old=0   
C       
      do i=1,nindxx
        k1=indx(1,i)  
        indx(1,i)= -k1
        if (k1 .ne. k1old) then
          ikk(4,k1)=i   
          k1old=k1  
        endif
      enddo
C       
      if (idswa .ne. 0) then
        write (dbug,1370) 
 1370   format('0 Remote control summary ', /
     1         '0 Controlling device',44x, 'Controlled bus' /)  
      endif
C       
      do 1440 i=1,nindxx
      if (indx(2,i) .eq. 1 .or. indx(2,i) .eq. 3) go to 1440
      k1 = iabs(indx(1,i))  
      m1 = k1 - ntota   
      k2 = indx(3,i)
      m2 = k2 - ntota   

C     Check if controlled bus has fixed or variable voltage limit

      if (ntypu(m1) .ne. 5 .and. ntypu(m1) .ne. 12) then  
         if (vlimn(m1) .lt. vlimx(m1)) then     
            nb = opt2inp(m1)  
            if (ntypu(m1) .eq. 14) then            
               vmin = 0.0   
               vmax = 0.0   
            else if (ntypu(m1) .eq. 15 .or. ntypu(m1) .eq. 16) then   
               vmin = busdta(11,nb) 
               vmax = busdta(11,nb) 
            else
               vmin = busdta(12,nb) 
               vmax = busdta(11,nb) 
            endif   
            call glbvlt(nb, vmingb, vmaxgb)   
            if (ntypu(m1) .eq. 8 .or. ntypu(m1) .eq. 11) then
               if (m2 .gt. 0 .and. m1 .ne. m2) then
                 if (vmin .eq. 0) vmin = vmingb
                 if (vmax .eq. 0) vmax = vmaxgb
                 write (errbuf(2),1402) vmin, vmax
 1402            format(' The preceding BG/BX/LTC is controlling a remot
     &e bus with V limits (', f6.3, ',', f6.3, ')')
                 nummsg = 2
               else 
                 go to 1440
               endif
        
            else if (vmingb .eq. vlimn(m1) .and. 
     &               vmaxgb .eq. vlimx(m1)) then
               write (errbuf(2),1406) vmin, vmax, vlimn(m1), vlimx(m1) 

 1406          format(' Original limits (', f6.3, ',', f6.3, 
     &           ') are changed to global limits (', f6.3, ',', f6.3, 
     &           ').')   
               nummsg = 2   
        
            else

               go to 1440
        
            endif   
            if (k2 .le. ntota) then 
               kt = ltran(1,k2) 
               mt = ltran(9,k2) 
               call typno (type,ntypu(m1))               
               write (errbuf(1), 1412) intbus(m1), intbas(m1), type,   

     1            intbus(kt), intbas(kt), intbus(mt), intbas(mt)
 1412          format(' Remote bus ', a8, f7.1,' Type ', a1,
     &             ' is V-controlled by LTC ', a8, f7.1, 1x, a8, f7.1)
            else
               call typno (type,ntypu(m1))            
               call typno (type2,ntypu(m2))            
               write (errbuf(1), 1414) intbus(m2), intbas(m2), type2,
     1            intbus(m1), intbas(m1), type
 1414          format(' Bus ', a8, f7.1, ' type ', a1,
     &            ' is V-controlling remote bus ', a8, f7.1, ' type ', 
     &            a)
            endif   
            call prterx ('W',nummsg)
         endif  
      endif 
      if (k2 .gt. ntota) then
        if (idswa .ne. 0) then
          call typno (type,ntypu(m2))                  
          write (dbug,1420) type, intbus(m2), intbas(m2), m2, 
     &      intbus(m1), intbas(m1), m1   
 1420     format(2x, 'Type "', a1, '" bus  ', a8, f6.1, ' (', i5, ')',
     &      27x, a8, f6.1, ' (', i5, ')')  
        endif
      else
        if (idswa .ne. 0) then
          kt = ltran(1,k2)  
          mt = ltran(9,k2)  
          write (dbug,1430) k2, intbus(kt), intbas(kt), kt, intbus(mt),
     &      intbas(mt), mt, intbus(m1), intbas(m1), m1   
 1430     format(2x, 'LTC xfmr (', i4, ')  ', a8, f6.1, ' (', i5, ')',
     &      2x, a8, f6.1, ' (', i5, ')', 2x, a8, f6.1, ' (', i5, ')')
        endif
      endif
 1440 continue
 1450 continue  
c
c     Test for compact control schemes interconnected with bus ties
c
      do kt = 1, ntot
        ifirst = 1
        ilast = 0
        jt = ikk(4,kt+ntota)
        found = .false.
        do while ((jt .gt. 0 .and. jt .le. nindxx) .and. 
     &            (iabs(indx(1,jt)) .eq. kt+ntota) .and. (.not. found))

c
c         Search all voltage controlled buses
c
          if (indx(2,jt) .eq. 2 .or. indx(2,jt) .eq. 4 .or.
     &        indx(2,jt) .eq. 102) then
            ilast = ifirst
            itemp(ifirst) = kt
            found = .true.
          else
            jt = jt + 1
          endif
        enddo
c
c       Define a subnetwork of all buses emanating from kt 
c       which are interconnnected with bus ties
c
        do while (ifirst .le. ilast)
          k = itemp(ifirst)
          if (abs(bkku(k)) .gt. 1000.0) then
            do l = km(k), km(k)-1+kmlen(k)   
              if (abs(bkmu(l)) .gt. 1000.0) then
                m = ikmu(l)                                 
                found = .false.
                i = 1
                do while (i .le. ilast .and. .not. found)
                  if (itemp(i) .eq. m) then
                    found = .true.
                  else
                    i = i + 1
                  endif
                enddo
                if (.not. found) then
                  ilast = ilast + 1
                  itemp(ilast) = m
                endif
              endif
            enddo
          endif
          ifirst = ifirst + 1
        enddo
c
c       Skip if a candidate node has been processed (kt > mt), or
c       if there are no voltage limit inconsistencies. 

C       icount flags meaningful voltage discrepencies
c
        if ( ntypu(kt) .ne. 5 .and. ntypu(kt) .ne. 12) then
          icount = 1
        else
          itemp(1) = -kt
          icount = 0
        endif
        i = 2
        do while (i .le. ilast)
          mt = itemp(i)
          found = .false.
          jt = ikk(4,mt+ntota)
          do while ((jt .gt. 0 .and. jt .le. nindxx) .and. 
     &              (iabs(indx(1,jt)) .eq. mt+ntota) .and.
     &              ( .not. found))
c
c           Search all voltage controlled buses
c
            if (indx(2,jt) .eq. 2 .or. indx(2,jt) .eq. 4 .or.
     &          indx(2,jt) .eq. 102) then
               found = .true.
            else
              jt = jt + 1
            endif
          enddo
          if (found ) then
            if ((vlimn(kt) .ne. vlimn(mt) .or.
     &           vlimx(kt) .ne. vlimx(mt)) .and.
     &          (ntypu(mt) .ne. 5 .and. ntypu(mt) .ne. 12)) then
              icount = icount + 1
            endif
          else
            itemp(i) = -mt
          endif             
          i = i + 1
        enddo

        if (icount .gt. 1) then
c
c         Now compute true icount and average voltage
c
          icount = 0
          vavemin = 0.0
          vavemax = 0.0
          do i = 1, ilast
            mt = itemp(i)
            if (mt .gt. 0) then
              icount = icount + 1
              vavemin = vavemin + vlimn(mt)
              vavemax = vavemax + vlimx(mt)
            endif
          enddo
          ivavemin = 1000.0 * vavemin / float(icount)
          ivavemax = 1000.0 * vavemax / float(icount)
          vavemin = float(ivavemin) / 1000.0
          vavemax = float(ivavemax) / 1000.0
          ierr = 1
          write (errbuf(ierr), 11730)
11730     format (' Inconsistent controlled-bus voltage limits in a bus-
     &tie sybsystem are changed')
          do i = 1, ilast
            mt = itemp(i)
            if (mt .gt. 0) then
              ierr = ierr + 1
              call typno (type,ntypu(mt))               
              write (errbuf(ierr), 11732) type, intbus(mt), intbas(mt),

     &          vlimn(mt), vlimx(mt), vavemin, vavemax
11732         format (' Bus B', a1, 1x, a8, f7.1, 1x, 'old V-limits (',

     &          f5.3, ',', f5.3, ') new V-limits (', f5.3, ',', f5.3, 
     &          ')')
              vlimx(mt) = vavemax
              vlimn(mt) = vavemin
            endif
          enddo
          if (ierr .gt. 1) call prterx ('W', ierr)
        endif
      enddo
C       
      if (iasw .eq. 0) go to 1680  
C       
C     Set of system slack buses must be subset of area slack buses  
C       
      do i=1,nbslck
         kt=nslkxx(1,i)
         islack(2,i)=jarzn(kt) 
      enddo
C       
      ksw=1 
      do 1550 jt=1,ntotc
        nt=0  
        kt=karea(1,jt)
        nb=opt2inp(kt)  
        jb=jarzn(nb)  
        if (jb.eq.jt) go to 1480  
        if (kt.le.nbslck) go to 1480  
 1472   ksw = 2   
        write (errbuf(1),1470)intbus(kt),intbas(kt)   
 1470   format(' Area interchange slack bus resides in wrong area '   
     1       ,a8,f7.1,'. Interchange control aborted.')
        if (is_batch .eq. 0) then
           call prterx ('E',1)
        else
           call prterx ('F',1)
        endif
        iasw = 0  
        ksw = 2   
C       
 1480   continue  
        if (kt.le.nbslck) nt=nt+1 
 1490   continue  
        go to (1500,1520,1520,1500,1500,1500,1520,1520,1520,1500,1520,
     1         1500,1500,1500,1520,1520) ntypu(kt)   
C       
 1500   call typno (type,ntypu(kt))                  
        write (errbuf(1),1510) intbus(kt),intbas(kt),type 
 1510   format('  AREA SLACK BUS ',a8,f7.1,'  SUB-TYPE ', a1, 
     1       ' HAS FIXED REACTIVE.')   
        call prterx ('W',1)   
c       
c       LT: Count of system slack buses in area JT 
c       NT: Count of area slack buses in area JT which are system  
c         slack buses
C       
 1520   continue  
        lt=0  
        do i=1,nbslck
          if (islack(2,i).eq.jt) lt=lt+1  
        enddo
C       
        if ((nt .eq. 0 .and. lt .gt. 0) .or.
     &      (nt .gt. 0 .and. lt .eq. 0)) then
           write (errbuf(1),1540) arcnam(jt),intbus(kt),intbas(kt)   
 1540     format(' Area ',a10,' with interchange slack bus ',a8,f6.1,
     1       ' has an independent and illegal system slack bus.') 
          call prterx ('W',1)   
        endif
C       
 1550 continue  

 1560 if (ksw .eq. 1) go to 1562
      iasw = 0  
      kabort=1  
      kase1(6)=2
      go to 1680
C       
 1562 do jt=1,ntotc
        kt=karea(1,jt)
        njndxx=njndxx+1   
        jndx(1,njndxx)=kt+ntota   
        jndx(2,njndxx)=3  
        jndx(3,njndxx)=jt 
      enddo
C       
C     Flag LTC tie lines
C       
 1600 do 1630 i=1,jtie  
      k1 = tie(1,i)
      k2 = tie(7,i)
      if (itsw.eq.0) go to 1620  
C       
      do 1610 jt=1,ntota
        ityp = mod(ltran(10,jt),100)  
        lt = ltran(10,jt)/100 
        if (ityp .gt. 10) go to 1610  
        k10=min0(k1,k2)   
        k20=min0(ltran(1,jt),ltran(9,jt)) 
        if (k10.ne.k20) go to 1610 
        k10=max0(k1,k2)   
        k20=max0(ltran(1,jt),ltran(9,jt)) 
        if (k10.ne.k20) go to 1610 
        
        tie(10,i) = ifix(sngl(tie(10,i))) + 100000 * jt 
        
        njndxx=njndxx+1   
        jndx(1,njndxx)=jt 
        jndx(2,njndxx)=7  
        jndx(3,njndxx)=i  
        
 1610 continue  
C       
C     Flag tie lines for dg/dv effects    
C       
 1620 njndxx=njndxx+1   
      jndx(1,njndxx)=k1+ntota   
      jndx(2,njndxx)=5  
      jndx(3,njndxx)=i  
        
      njndxx=njndxx+1   
      jndx(1,njndxx)=k2+ntota   
      jndx(2,njndxx)=5  
      jndx(3,njndxx)=i  
C       
C     Link up tie array with d-c arrays  
C       
 1622 kdc_flag = tie(9,i)   
      if (kdc_flag .ne. 0) then
C       
        kdc = 1
        do while (kdc .le. mtdcln)
           k1x = dcmtln(1,kdc) 
           k2x = dcmtln(2,kdc) 
           k1x = inp2opt(k1x)
           k2x = inp2opt(k2x)
           if (min0(k1,k2) .eq. min0(k1x,k2x) .and.
     &         max0(k1,k2) .eq. max0(k1x,k2x)) then
              tie(9,i) = kdc 
              go to 1630  
           else
              kdc = kdc + 1
           endif
        enddo
        kdc = 1
        do while (kdc .le. kdtot)
           k1x = dc2t(1,kdc)  
           k2x = dc2t(3,kdc)  
           k1x = inp2opt(k1x)
           k2x = inp2opt(k2x)
           if (min0(k1,k2) .eq. min0(k1x,k2x) .and.
     &         max0(k1,k2) .eq. max0(k1x,k2x)) then
              tie(9,i) = kdc 
              go to 1630  
           else
              kdc = kdc + 1
           endif
        enddo
        call erexit 
      endif
 1630 continue  

 1640 do i=1,ntotc 
         dkdp(i)=0.0   
         kt=karea(1,i) 
         if (kt.gt.nbslck) p1sked = p1sked - pnetu(kt)
      enddo
C       
 1680 if (iasw.gt.0) koptsw=koptsw+1
C       
C     Link percentage-var PCTVR with bus
C       
      do jt=1,npctvr   
         ptr = kpctvr(2,jt)
         do while (ptr .gt. 0)
            mt=pctvr(1,ptr)   
            if (mt .gt. 0) then
              njndxx=njndxx+1   
              jndx(1,njndxx)=mt+ntota   
              jndx(2,njndxx)=4  
              jndx(3,njndxx)=jt
              ptr = pctvr(8,ptr)
            endif
         enddo
      enddo
C       
C     Link AGC generators.  
C       
      do jt = 1, numagc
         nb = kagc(1,jt)
         kt = inp2opt(nb) 
         njndxx = njndxx + 1
         jndx(1,njndxx) = kt+ntota  
         jndx(2,njndxx) = 13
         jndx(3,njndxx) = jt
      enddo
C       
C     Link Line Drop Compensators.  
C       
      do jt = 1, numldc
         nb = lndpcp(1,jt)  
         kt = inp2opt(nb) 
         njndxx = njndxx + 1
         jndx(1,njndxx) = kt+ntota  
         jndx(2,njndxx) = 14
         jndx(3,njndxx) = jt
      enddo
C       
C     Link SVC Compensators.
C       
      do jt = 1, numsvc
         nb = svc(1,jt)
         kt = inp2opt(nb) 
         njndxx = njndxx + 1
         jndx(1,njndxx) = kt+ntota  
         jndx(2,njndxx) = 15
         jndx(3,njndxx) = jt
      enddo
        
      if (njndxx .gt. 0) then
C       
C        Sort and link up JNDX 
C       
         call qiksrt(1,njndxx,kmpjdx,swpjdx)   
         k1old=0   
         do i=1,njndxx
            k1=jndx(1,i)  
            if (k1 .ne. k1old) then
               ikk(5,k1)=i   
               k1old=k1  
            endif
         enddo
      endif

C     Update Taps using completed JNDX array.  The following data   
C     structures are affected: Y-matrix, TIE array, TXTIE array.
C       
      if (itsw .ne. 0) then 
         do jt = 1, ntota  
            ityp = mod(ltran(10,jt),100)  
            lt = ltran(10,jt)/100 
            if (ityp .lt. 10) then 
                dx = tapnew(jt) - tap(jt)  
                if (abs (dx) .gt. 1.0e-6) then 
                   call ltcadj(jt,dx,'OPSLN1') 
                endif  
            endif  
         enddo
      endif 
C       
C     Check LINE_DROP_COMPENSATORS for interference with %Var   
C     controlled schemes.   
C       
C     Hint:  JNDX(1,*)  JNDX(2,*)  JNDX(3,*)  Meaning   
C               KT         4          J       PCTVR(1,J) = KT  
C               KT         8          J       TBX(2,J) = KT
C               KT        14          J       LNDPCP(1,J) = KT  
C               KT        15          J       LNDPCP(2,J) = KT  
C       
      do 1728 i = 1, numldc 
         nb = lndpcp(1,i)   
         kt = inp2opt(nb) 
         do 1726 j = ikk(5,kt+ntota), njndxx
            if (jndx(1,j) .eq. kt + ntota) then 
               if (jndx(2,j) .eq. 4) then   
                  mt = kpctvr(1,jndx(3,j))  
                   call typno (type, ntypu(mt))    
                  write (errbuf(1), 1724) intbus(kt),intbas(kt), type,
     1               intbus(mt), intbas(mt) 
 1724             format(' BG bus ', a8, f6.1, ' controlling B', a1,
     1               ' bus is both %VARs and LINE_DROP_COMPENSATION')
                  call prterx ('W',1)   
                  go to 1728
               endif
            else
               go to 1728   
            endif   
 1726    continue   
 1728 continue  
C       
C     Check "ECONOMIC DISPATCH" 
C       
      if (ngen.eq.0) go to 1890 
      lsw=1 
      if (iasw.eq.0) go to 1750 
      ina=0 
      go to 1740
C       
 1730 kecon(2,i)=ina
 1740 ina=ina+1 
      if (ina.gt.ntotc) go to 1800  
      kt=karea(1,ina)   
      go to 1760
C       
 1750 kt=1  
 1760 do 1770 i=1,ngen  
      if (kecon(1,i).eq.kt) go to 1790  
 1770 continue  
      write (errbuf(1),1780) intbus(kt),intbas(kt)  
 1780 format(' Slack bus ',a8,f7.1,' has no assigned cost coefficients.

     & optimal dispatch aborted.')  
      call prterx ('W',1)   
      lsw=2 
 1790 if (iasw.ne.0) go to 1730 
      kecon(2,i)=1  
      kt = kt + 1   
      if (kt.le.nbslck) go to 1760  
 1800 if (lsw.eq.2) go to 1880  
      koptsw=koptsw+2   
        
      call forbtm   
      outbuf = ' * * * ECONOMIC DISPATCH * * * '
      call rpnlod   
      outbuf = ' '  
      call shdlod(1)
        
      call fortop   
      write (outbuf,1810)   
 1810 format (t41,'ECONOMIC DISPATCHABLE GENERATION DATA ' )
      call prtout(1)
      write (outbuf,1820)   
 1820 format(1h0,41x,33hcost($) = a + b'pgen + c'pgen''2  ) 
      call prtout(1)
      write (outbuf,1830)   
 1830 format('0',                       11x,'BUS       BASE         PMIN
     1           PMAX             A             B             C ' ) 
      call prtout(1)
      write (errbuf(1),1840)
 1840 format(35x,',(MW)           (MW) ',11x,'($)',9x,'($/MW)',7x,  
     1    ' ($/MW**2)') 
      call prtout(1)
      write (outbuf,1850)   
 1850 format(133x)  
      call prtout(1)
C       
      do 1870 jt=1,ngen 
      kt=kecon(1,jt)
      write (errbuf(1),1860)intbus(kt),intbas(kt),(econ(i,jt),i=3,4),
     1   (econ(i,jt),i=8,10)
      call prtout(1)
 1860 format (12x,a8,f7.1,f12.1,f15.1,3x,3e14.4)
      econ(3,jt)=econ(3,jt)/bmva
      econ(4,jt)=econ(4,jt)/bmva
      econ(9,jt)=econ(9,jt)*bmva
 1870 econ(10,jt)=econ(10,jt)*bmva*bmva 
 1880 continue  
      if (lsw.eq.1) go to 1890  
      ngen=0
 1890 if (nqpen.eq.0) go to 1950
C       
C     List all reactive penalty busses  
c       
      call forbtm   
      outbuf = ' * * * REACTIVE PENALTIES * * * '   
      call rpnlod   
      outbuf = ' '  
      call shdlod(1)
        
      call fortop   
      write (outbuf,1900) qpen, 0.0
 1900 format ('  Reactive penalties will be accessed on the following ',
     1'buses with factors ',2f8.3  )
      call prtout(1)
      write (outbuf,1910)   
 1910 format(1h0,    ' BUS        BASE     NET REACTIVE INJECTION ' )
      call prtout(1)
      write (outbuf,1920)   
 1920 format(   '                          QMIN        QMAX     '  )
      call prtout(1)
      write (outbuf,1850)   
      call prtout(1)
C       
      do 1930 ig=1,nqpen
      qmin=qpent(3,ig)*bmva 
      qmax=qpent(4,ig)*bmva 
      ntyp=kqpent(2,ig) 
      kt=kqpent(1,ig)   
      call typno(type,ntyp) 
      write (outbuf,1940) intbus(kt),intbas(kt),type,qmin,qmax  
 1930 call prtout(5)
 1940 format (2x,a8,f7.1,2x,a1,f11.1,f12.1) 
C       
 1950 continue  
C       
C     Check d-c data 
C       
      kdcnt=0   
      if (idcsw.eq.0) go to 2160
      do 2150 jckt = 1,idckt
      ks=nckt(jckt) 
      ke=nckt(jckt+1)-1 
      do 2140 k = ks,ke 
      kt = dcbus(1,k)  
      mt = dcbus(3,k)  
      kdcnt=kdcnt+1 
 1970 ksw = 0   
      i = 0 
      if (kmlen(kt).gt.0) go to 1990              
      if (mt.eq.0) go to 2140   
      write (errbuf(1),1980)intbus(kt),intbas(kt),intbus(mt),intbas(mt)

 1980 format(' D-C bus ',a8,f7.1,' has no ac branches but is',   
     1' assigned a commutator bus ',a8,f7.1)
      call prterx ('W',1)   
      minerr = 1
      dcbus(3,k) = 0   
      go to 2140
C       
 1990 continue  
      if (kmlen(kt) .le. 1) go to 2010
      write (errbuf(1),2000)intbus(kt),intbas(kt)   
 2000 format(' D-C converter ',a8,f7.1,' has more than one ac branch.')

      if (is_batch .eq. 0) then
         call prterx ('E',1)
      else
         call prterx ('F',1)
      endif
      idcsw = 2 
 2010 do 2012 l=km(kt),km(kt)-1+kmlen(kt)     
      if (ikmu(l).eq.mt) go to 2030                
 2012 continue  
      write (errbuf(1),2020)  intbus(kt),intbas(kt),intbus(mt), 
     1 intbas(mt)   
 2020 format(' D-C converter ',a8,f7.1,' is not connected directly to ',
     1'  commutating bus. ',a8,f6.1)
      if (is_batch .eq. 0) then
         call prterx ('E',1)
      else
         call prterx ('F',1)
      endif
      idcsw = 2 
 2030 tmax = dcbus(16,k)
      tmin = dcbus(16,k)
      if (itsw .ne. 2 .and. itsw .ne. 4) go to 2060 
 2040 i=ikk(4,kt+ntota) 
      if (i.le.0) go to 2060 
 2050 if (i.gt.nindxx) go to 2060
      if (iabs(indx(1,i)).ne.kt+ntota) go to 2052
      if (indx(2,i).eq.4) go to 2080 
 2052 i=i+1 
      go to 2050
C       
 2060 continue  
      write (errbuf(1),2070)intbus(kt),intbas(kt),intbus(mt),intbas(mt)

 2070 format(' Converter bus ',a8,f7.1,' to d-c terminal ',  
     1 a8,f7.1,' is not controlled by ltc transformer.')
      call prterx ('W',1)   
      go to 2100
C       
 2080 i = indx(3,i) 
      k1=ltran(1,i) 
      tmax = tran(7,i)  
      tmin = tran(8,i)  
      if (kt .ne. k1) then
        write (errbuf(1),2090) intbus(mt), intbas(mt), intbus(kt),
     &    intbas(kt)
 2090   format(' LTC bridging commutator bus ',a8,f7.1,' and R/I bus ',

     &   a8, f7.1, ' has variable taps at wrong terminal.')
        call prterx ('W',1)   
        idcsw = 2 
      endif
C       
C     Check current limit against 99% of rating 
C       
 2100 a = 990.0*abs(dcbus(19,k)/dcbus(20,k))
      da = ddim (dble(a),dcbus(9,k))   
c
c     iopton(23) = 0:  BRIDGE_CURRENT_RATING = ON
c                  1:                          OFF
c
      if ( da .ne. 0.0 .and. dcbus(9,k) .eq. 0.0) then
        write (errbuf(1),2110) intbus(kt), intbas(kt), dcbus(19,k),
     &    dcbus(20,k),a
 2110   format(' D-C converter ', a8, f7.1, ' loading ', 2f8.1, 
     &    ' has zero ratings changed to 105% of loading ', f8.1)
        call prterx ('W',1)   
        dcbus(9,k) = 1050.0*abs(dcbus(19,k)/dcbus(20,k))
        nb = opt2inp(kt)
        busdta(8,nb) = dcbus(9,k)
      else if ( da .ne. 0.0 .and. iopton(23) .eq. 0) then
        write (errbuf(1),2112) intbus(kt),intbas(kt),dcbus(19,k),
     &    dcbus(20,k),a
 2112   format(' D-C converter ',a8,f7.1,' initial P', f8.1,
     &    ' MW and V ', f8.1, ' kv yields ', f8.1,
     &    ' amps which exceed rating.')  
        call prterx ('W',1)   
      endif
      emax = 1.35047447*dcbus(7,k)*intbas(kt)*tmax*cos (dcbus(10,k))
     1     * vlimx(kt)                                   
      emin = 1.35047447*dcbus(7,k)*intbas(kt)*tmin*cos (dcbus(12,k))
     1     * vlimn(kt)                                  
      dv = ddim (dcbus(20,k),dble(emax)) - ddim (dble(emin),dcbus(20,k))
      if (dv .ne. 0.0) then
        write (errbuf(1),2130) intbus(kt),intbas(kt),dcbus(20,k),dv   
 2130   format (' Converter ',a8,f7.1,' d-c terminal KV ',
     &     f6.1,' exceeds permissible limits by ',f6.1,' kv.')   
        call prterx ('W',1)   
      endif
 2140 continue
 2150 continue  
      if (kdcnt.eq.0) idcsw=0   
 2160 continue  
c
c     Adjust bus-tie voltages if flat start
c
      if (iopton(18) .eq. 1) then
        do kt = 1, ntot
          ifirst = 1
          ilast = 0
          if (abs(bkku(kt)) .gt. 1000.0) then
            found = .false.
            do l = km(kt), km(kt)-1+kmlen(kt)   
              if (abs(bkmu(l)) .gt. 1000.0) then
                found = .true.
              endif
            enddo
            if (found) then
              ilast = ilast + 1
              itemp(ilast) = kt
            endif
c
c           Define a subnetwork of all buses emanating from kt 
c           which are interconnnected with bus ties
c
            do while (ifirst .le. ilast)
              k = itemp(ifirst)
              if (abs(bkku(k)) .gt. 1000.0) then
                do l = km(k), km(k)-1+kmlen(k)   
                  if (abs(bkmu(l)) .gt. 1000.0) then
                    m = ikmu(l)                                 
                    found = .false.
                    i = 1
                    do while (i .le. ilast .and. .not. found)
                      if (itemp(i) .eq. m) then
                        found = .true.
                      else
                        i = i + 1
                      endif
                    enddo
                    if (.not. found) then
                      if (m .lt. kt) then
                        go to 2170
                      else
                        ilast = ilast + 1
                        itemp(ilast) = m
                      endif
                    endif
                  endif
                enddo
              endif
              ifirst = ifirst + 1
            enddo
c
c           Extract ratio if in TXTIE
c
            do i = 1, ilast
              mt = itemp(i)
              jt = ikk(5,mt+ntota)
              do while ((jt .gt. 0 .and. jt .le. njndxx) .and. 
     &                  (iabs(jndx(1,jt)) .eq. mt+ntota))
c
c               Search all bus-tie components
c
                if (jndx(2,jt) .eq. 10) then
                  it = jndx(3,jt)
                  if (txtie(1,it) .eq. mt) then
                    do j = 1, ilast
                      if (itemp(j) .eq. txtie(2,it)) then
                        go to 2162
                      endif
                    enddo
                  else
                    write (errbuf(1), 11734) intbus(kt), intbas(kt), 
     &                intbus(mt), intbas(mt)
11734               format (' Program error processing bus tie ',
     &                 a8, f6.1, 1x, a8, f6.1)
                    call prterx ('W',1)   
                    go to 2170
                  endif
                endif
                jt = jt + 1
              enddo
 2162         continue
            enddo

            ipvnode = 0
            ivcnode = 0
            do i = 1, ilast
              mt = itemp(i)
c
c             Identify any PV buses (ipvnode) or V-controlled buses
c             (ivcnode)
c
              if (ikk(1,mt+ntota) .eq. 1) then
                ipvnode = mt
              else if (ntypu(mt) .eq. 4 .or. ntypu(mt) .eq. 10) then
                ivcnode = mt
              endif
            enddo
c
c           If any pv node found, use as vstart basis
c           for all remaining buses. 

c           If any voltage-controlled node found, use vlimits
c           for all remaining buses. 
c
c           Set all PQ-buses in subsystem (except types BC and BT) to 
c           same V-limits
c
            icount = 0
            vavemin = 0.0
            vavemax = 0.0
            vavstart = 0.0
            do i = 1, ilast
              mt = itemp(i)
              if (ikk(1,mt+ntota) .eq. 2 .and. ivcnode .eq. 0) then
                icount = icount + 1
                vavemin = vavemin + vlimn(mt)
                vavemax = vavemax + vlimx(mt)
              endif
              vavstart = vavstart + vstart(opt2inp(mt))
            enddo
            if (ilast .gt. 1) then
              if (icount .gt. 0) then
                ivavemin = 1000.0 * vavemin / float(icount)
                ivavemax = 1000.0 * vavemax / float(icount)
                vavemin = float(ivavemin) / 1000.0
                vavemax = float(ivavemax) / 1000.0
              endif
              ivavstart = 1000.0 * vavstart /float(ilast)
              vavstart = float(ivavstart) / 1000.0
c
c             Search subsystem for actual inconsistency before
c             printing warning diagnostics
c
              found = .false.
              do i = 1, ilast
                mt = itemp(i)
                if (ikk(1,mt+ntota) .eq. 2) then
                  if (ipvnode .ne. 0) then
                    vratio = vstart(opt2inp(ipvnode)) 
     &                     / vstart(opt2inp(mt))
                  else
                    vratio = vavstart / vstart(opt2inp(mt))
                  endif
                  if (abs(vratio - 1.0) .gt. 0.001) found = .true.
                  if (ivcnode .eq. 0) then
                    if (abs(vlimx(mt) - vavemax) .gt. 0.001 .or.
     &                  abs(vlimn(mt) - vavemin) .gt. 0.001) 
     &                found = .true.
                  else
                    if (abs(vlimx(mt) - vlimx(ivcnode)) .gt. 0.001 .or.

     &                  abs(vlimn(mt) - vlimn(ivcnode)) .gt. 0.001)
     &                found = .true.
                  endif
                else if (mt .ne. ipvnode) then
                  vratio = vstart(opt2inp(ipvnode)) 
     &                   / vstart(opt2inp(mt))
                  if (abs(vratio - 1.0) .gt. 0.001) found = .true.
                  if (abs(vlimx(mt) - vlimx(ipvnode)) .gt. 0.001 .or.
     &                abs(vlimn(mt) - vlimn(ipvnode)) .gt. 0.001)
     &              found = .true.
                endif
              enddo
              if (.not. found) go to 2170

              ierr = 1
              write (errbuf(ierr), 11740)
11740         format (' Inconsistent bus voltage limits in a bus-tie sub
     &system are changed')
              do i = 1, ilast
                mt = itemp(i)
                if (ikk(1,mt+ntota) .eq. 2) then
                  if (ierr .eq. 10) then
                    call prterx ('I', ierr)
                    ierr = 0
                  endif
                  if (ipvnode .ne. 0) then
                    vratio = vstart(opt2inp(ipvnode)) 
     &                     / vstart(opt2inp(mt))
                  else
                    vratio = vavstart / vstart(opt2inp(mt))
                  endif
                  if (ipvnode .eq. 0 .and. ivcnode .eq. 0) then
                    ierr = ierr + 1
                    call typno (type,ntypu(mt))               
                    write (errbuf(ierr), 11750) type, intbus(mt), 
     &                intbas(mt), vlimn(mt), vlimx(mt), vavemin, 
     &                vavemax, vavstart
11750               format (' Bus B', a1, 1x, a8, f7.1, 1x, 
     &                'old V-limits (', f5.3, ',', f5.3, 
     &                ') new V-limits (', f5.3, ',', f5.3, 
     &                ') vstart (', f5.3, ')')
                    vlimx(mt) = vavemax
                    vlimn(mt) = vavemin
                    e(mt) = e(mt) * vratio
                    f(mt) = f(mt) * vratio
                    vstart(opt2inp(mt)) = vavstart
                  else if (ipvnode .ne. 0) then
                    ierr = ierr + 1
                    call typno (type,ntypu(mt))               
                    write (errbuf(ierr), 11750) type, intbus(mt), 
     &                intbas(mt), vlimn(mt), vlimx(mt), vlimn(ipvnode),
     &                vlimx(ipvnode), vstart(opt2inp(ipvnode))
                    vlimx(mt) = vlimx(ipvnode)
                    vlimn(mt) = vlimn(ipvnode)
                    e(mt) = e(mt) * vratio
                    f(mt) = f(mt) * vratio
                    vstart(opt2inp(mt)) = vstart(opt2inp(ipvnode))
                  else 
                    ierr = ierr + 1
                    call typno (type,ntypu(mt))               
                    write (errbuf(ierr), 11750) type, intbus(mt), 
     &                intbas(mt), vlimn(mt), vlimx(mt), vlimn(ivcnode),
     &                vlimx(ivcnode), vavstart
                    vlimx(mt) = vlimx(ivcnode)
                    vlimn(mt) = vlimn(ivcnode)
                  endif
                else
                  if (ierr .eq. 10) then
                    call prterx ('I', ierr)
                    ierr = 0
                  endif
                  ierr = ierr + 1
                  vratio = vstart(opt2inp(ipvnode)) 
     &                   / vstart(opt2inp(mt))
                  call typno (type,ntypu(mt))               
                  write (errbuf(ierr), 11750) type, intbus(mt), 
     &              intbas(mt), vlimn(mt), vlimx(mt), vlimn(ipvnode),
     &              vlimx(ipvnode), vstart(opt2inp(ipvnode))
                  vlimx(mt) = vlimx(ipvnode)
                  vlimn(mt) = vlimn(ipvnode)
                  e(mt) = e(mt) * vratio
                  f(mt) = f(mt) * vratio
                  vstart(opt2inp(mt)) = vstart(opt2inp(ipvnode))
                endif
              enddo
              if (ierr .gt. 1) call prterx ('I', ierr)
            endif
          endif
 2170     continue
        enddo
      endif
c
C     Debug dump
C       
      idebug=0  
      do i=12,15   
         idebug=idebug+iopton(i)   
      enddo
C       
      do i=29,31   
         idebug=idebug+iopton(i)   
      enddo
C       
      if (idebug .eq. 0) go to 2260   
      do j=1,ntot,5
         ks=min0(5,ntot-j+1)   
         do k=1,ks  
            busnu(k)=intbus(k+j-1)
            basenu(k)=intbas(k+j-1)   
         enddo
         jt=j+ks-1   
         write (dbug,2200) (k,busnu(k-j+1),basenu(k-j+1),k=j,jt) 
 2200    format (1x,'(',i5,')',2x,a8,f6.1,3x,'(',i5,')',2x,a8,f6.1,3x, 
     &    '(',i5,')',2x,a8,f6.1,3x,'(',i5,')',2x,a8,f6.1,3x,'(',i5,')',
     &    2x,a8,f6.1)  
      enddo
C       
      if (itsw .gt. 0) then
         do jt=1,ntota
            ityp = mod(ltran(10,jt),100)  
            lt = ltran(10,jt)/100 
            kt=ltran(1,jt)
            mt=ltran(9,jt)
            write (outbuf,2230) jt,kt,intbus(kt),intbas(kt),mt,
     &          intbus(mt),intbas(mt)
 2230       format (' LTC transformer ',i3,2(2x,'(',i5,')',2x,a8,f6.1))
            if (ityp .gt. 10) outbuf(70:) = '(inactive)'  
            write (dbug,2240) outbuf  
 2240       format(a) 
         enddo
      endif
        
 2260 continue  
        
      if (minerr .ne. 0) then   
         write (errbuf(1),2400) 
 2400    format ('Minor errors have disabled some devices.')
         call prterx ('W', 1)   
      endif 
        
      if (kabort .ne. 0) then   
         write (errbuf(1),2420) 
 2420    format ('Major errors have aborted area interchange control.')

         call prterx ('W', 1)   
      endif 
        
      if (idcsw .ge. 2) then
         write (errbuf(1),2440) 
 2440    format ('Major errors have aborted d-C control.')  
         call prterx ('W', 1)   
      endif 
        
 2500 continue  
        
      return
      end   
