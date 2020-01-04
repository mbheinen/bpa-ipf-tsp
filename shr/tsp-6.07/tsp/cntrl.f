C    %W% %G%
      subroutine cntrl
c      
c     This subroutine serves as the control module for the solution
c     portion of the program.  It determines the time step, checks
c     to see if a network modification is needed, calls the needed
c     subroutines for a time step solution, and checks tolerances to
c     to see if a successful solution has been reached.  

C     It is called by SWINGM.  It calls : ANGSOL, BRAKE, CNTRLA,
C     DERIV, ENDSUM, FREQC, MAXMIN, RELAY, SHDSOL, SVSSOL, VTCHK,
c     VYSOL,WRTHIS.
c      
      include 'tspinc/params.inc'
      include 'tspinc/blkcom1.inc'
      include 'tspinc/cntrl2.inc'
      include 'tspinc/prt.inc'
      include 'tspinc/toler.inc'
      include 'tspinc/param.inc'
      include 'tspinc/contrl.inc'
      include 'tspinc/search.inc'
      include 'tspinc/lnk12.inc'
      include 'tspinc/lnk1a.inc'
      include 'tspinc/vrgov.inc'
      include 'tspinc/ecsind.inc'
      include 'tspinc/bname.inc'
      include 'tspinc/buskv.inc'
      include 'tspinc/brake1.inc'
      include 'tspinc/ecstbb.inc'
      include 'tspinc/ecstbc.inc'
      include 'tspinc/ecstbd.inc'
      include 'tspinc/ecstbh.inc'
      include 'tspinc/ecstbj.inc'
      include 'tspinc/machd1.inc'
      include 'tspinc/machd2.inc'
      include 'tspinc/outaux.inc'
      include 'tspinc/igentn.inc'
      include 'tspinc/gentbla.inc'
      include 'tspinc/gentblb.inc'
      include 'tspinc/gentblc.inc'
      include 'tspinc/dateq.inc'
      include 'tspinc/lshed1.inc'
      include 'tspinc/busdta.inc'
      include 'tspinc/pointr.inc'
      include 'tspinc/relays.inc'
      include 'tspinc/amorts.inc'
      include 'tspinc/prate.inc'
      include 'tspinc/vtchkc.inc'
      include 'tspinc/busvolt.inc'
      common/vregl/em,vmqt,vmdt
C 
C     The following common block is due to split between CNTRL and CNTRLA
C 
      include 'tspinc/comvar.inc'
      include 'tspinc/znox.inc'
      include 'tspinc/spare1.inc'

      common /ckpnt/ dnxmin,d13
      equivalence (creg(9), dr), (creg(21), rcex), (creg(23),
     1    xcex), (creg(3), vref), (creg(27), vco), (creg(7), cckp,
     2    ckpr), (creg(28), theta, ckpi), (creg(20), xl), (creg(10),
     3    ckc), (creg(11), v1max), (creg(13), va), (creg(30), vi),
     4    (creg(15), hcb), (creg(16), ckj), (creg(17), db),
     5    (creg(18), dc), (creg(19), vgmax), (creg(1), vrmax),
     6    (creg(2), vrmin), (creg(22), vr), (creg(8), cka), (creg(24),
     7    ckg), (creg(25), da), (creg(26), hba), (creg(4), efdmax),
     8    (creg(5), cki), (creg(29), efd), (creg(6), hbr),
     9    (creg(12), v1min), (creg(14),vfldo)

      include 'tspinc/deltfq.inc'

c *** csw 9/93 added this common block to hold govmin/govmax terms
      common /govminmax/ govmn(MAXGEN), govmx(MAXGEN)
c *** csw end addition

      dimension difpwr(MAXGEN)
      equivalence (aeps,ar), (aes,brs,af1), (bes,bf1), (ba1s,br),
     1            (baps,a1), (bfs,a2), (ae, vfdo), (aep, bfx), 
     2            (be, ba1x), (bf1, bapx)
      dimension istem(MAXLS)

      equivalence (istem,isorti)
      equivalence(csupp(26),x5o)

      dimension itempn(4,5), ktemp1(9), temp1(9), tempn(2,13),
     1          ecsn2(2), iecsn(6)
      equivalence (ktemp1,temp1), (pgo,pgor), (sat,rt), (sater,dtc),
     1            (isg2,istp), (csupp(27),ivcsw)
      character*1 idm, id, id1
      character*8 name, name1, name2
c
      logical debug, dbghere, finished, flag                            !dem
      character gname*16                                                !dem

      data twopi  / 6.2831853 /
      data degrad /0.0174533/
      data pi     /3.1415927 /
C     begin     begin     begin     begin     begin     begin

      debug = dbghere ('CNTRL   ')                                      !dem
      if (debug) then                                                   !dem
        call dbgeko ('CNTRL - At start')                                !dem
        call dbgwrf ('  TO /time/ = ',to)                               !dem
        call dbgwri ('  IDSW /discont stat/ = ',idsw)                   !dem
        call dbgwri ('  IDISW /discont sw/  = ',idisw)                  !dem
        call dbgwri ('  LPPWR /iteratn cnt/ = ',lppwr)                  !dem
      endif                                                             !dem
      mts = 39
      igv = 19
      iex = 35
      isup = 33
      isup1 = isup
c
c     If change in time step, resolve machine differential equations
c
      if (al.eq.1.) go to 3520
      irstrt = 0

c     Resolve the network

  100 call xtime(-3)
      call deriv
      call xtime(3)
c
c     LINK > 10 flags fatal error return from DERIV
c
      if (link .gt. 10) then
         link = link - 10
         jexit=1
         go to 2200
      endif

      idsw7=1
      asum=0.0
      i7=1
      i8=1
c      
c     Calculate accelerating power
c      
      do i = 1, isg
         mgen=igndta(1,i)
c      
c        If machine is an induction motor, calculate accelerating power 
c        only
C      
         if (mgen .eq. 2) then
            difpwr(i)=govpwr(i)-genp(i)
            go to 2440
         endif
c      
C        Bypass adjustments for fst valving/flt damping except for 
c        first pass
c      
         if (lppwr.ne.0) go to 2432
         if (idsw.eq.7) go to 2432
c      
C        Check for fast valving
c      
         if (i8.gt.i8max) go to 2432
         do jj=1,i8max
            if (i .eq. ifvl(jj)) then
               if (abs(dfvl(jj)) .gt. 0.001) then
                  govpwr(i) = govpwr(i) + dfvl(jj) * edt / frqbse
               endif
               i8=i8+1
               go to 2432
            endif
         enddo
 2432    if (mfdep .eq. 5) then
            rcmfac = 2.0*pi/(2.0*pi + angp2(i))
            difpwr(i) = govpwr(i) * rcmfac - genp(i)
         else
            difpwr(i) = govpwr(i) - genp(i)
         endif
c      
C        Check for DC offset damping
c      
         if (i7 .gt. i7max) go to 2440
         do kk=1,i7max
            if (i .eq. iflt(kk)) then
               i7=i7+1
               if (abs(dflt(kk)) .ge. 0.001) then
                  difpwr(i)=difpwr(i)-dflt(kk)*govpwr(i)
               endif
               go to 2440
            endif
         enddo
 2440    asum=asum+abs(difpwr(i))

c        call genname (i,gname)                                         !dem
c        if (gname .eq. 'GEN TD1   20.0 L') then
c           sparpt(8) = difpwr(i)
c        else if (gname .eq. 'GEN TD1   20.0 H') then
c           sparpt(10) = difpwr(i)
c        endif

      enddo
C 
C     VREG + SPGOV DEBUG 
C 
      if (keybrd(16) .ne. 0) then
         i = 900
         write (outbuf, 2460) i, idsw, lppwr, tc1, tc2, tc3, edt, 
     1                        ddt1, ddt2, dnx, tnx, asum
 2460    format(' CX ', 3i5, 9e12.5)
         call prtout (1)
         do jjj = 1, isg, 4
            kkk = min0 (jjj+3, isg)
            write (outbuf, 2461)  
     1            (k, genp(k), difpwr(k), k = jjj,kkk) 
 2461       format(1x, 4(i5, 2e13.5))
            call prtout (1)
         enddo
      endif

      if (asum .gt. asumt) then

C        SETUP NEG SEQ SW

         if (iesc.ne.1.and.gfr.ne.0.0.or.bfr.ne.0.0) then
            negseq=2
            eyrmx = eyr(nmx)
            eyimx = eyi(nmx)
            fltcr=eyrmx*gfr-eyimx*bfr
            fltci=eyrmx*bfr+eyimx*gfr
         else
            negseq=1
         endif

         if (igtmax .ge. 3) then
            do ispf = 1, isg
               iecs=igentn(2,ispf)
               igbn=igentn(1,ispf)
               call redecs (datat(7),iecs+6,1)
               disf2 = 0.0
               if (dmpfac .ne. 0.0 .and. disf2 .ne. 0.0) then

C                 Test for NEG SEQ braking (Note: Code is disabled!)

                  if (negseq .eq. 2) then

C                    Obtain NEGSEQ DMP PWR

                     cr=fltcr*disf2
                     ci=fltci*disf2

c                    Send error msg since RNEG & RPOS have no values 
c                    up to now

                     write (errbuf(1),'(2a)') 
     &          'CNTRL - undefined variables in neg seq braking calcs.'
                     call prterr ('E',1)                                                  !dem
                     dampn=(cr*cr+ci*ci)*(rneg-rpos)
                     difpwr(ispf)=difpwr(ispf)-dampn
                     if (keybrd(3) .ne. 0) then
                        i=1295
                        write (outbuf,3500) i,dampn
 3500                   format (' 1c', i5, 'dampn', e15.7)
                        call prtout (1)
                     endif
                  endif
               endif
            enddo
         endif
      else
         dampn=0.0
      endif

      if (idsw.ne.7) go to 2560

c     -  Last chance to change any states at a discont before they're
c        stored in the history file.
C     call ssstep
C     I140=0 Denotes coming out of discontinuity

      i140 = 0
      to = tsim
      iskkt = 0
c      
c     Call WRTHIS to write solution data to history file FOR008
c     -  Save the plus side of a discontinuity.  We also save at
c        t = 0+, even if no switching event then.

      if (debug) then                                                   !dem
        call dbgeko ('CNTRL - saving history file at 2nd WRTHIS call')  !dem
      endif                                                             !dem
      call wrthis
      do i=1, isg
        paccex(i) = difpwr(i)
      enddo
c     -  Change progress status from processing discont point to proces-
c        sing regular interval with a discont at its start.
      idsw=3
      idisw=2
      idsw7=1

C     Initialize load mod. sw. + first bus

  120 ivpc=1
      if (dnxrly .le. to+0.0001) dnxrly = endt
      lfrst=nmx+1
c      
c     Call RELAY for relay solution logic
c      
      call relay
c      
c     Call BRKSOL for resistive brake logic
c      
      if (nbrake .ne. 0) call brksol
c      
c     Call SHDSOL for load shedding logic
c      
      if (ls .ne. 0) call shdsol
c      
c     Call FREQC to update bus frequency tables
c      
      call freqc
      if (idisw.eq.1) go to 2660
      idiskt=idiskt+1
      if (ivpc.eq.1) then
         idiskt=0
         idisw=1
         itrace = 3
         go to 3240
      endif
      idsw=7
      matsw=2
      link=2
      istat = 4
      return
c
c     Normal iteration (no discontunity)
c
 2560 itol=1
      itrace = 1
      itoldc = 1
      if (lppwr .eq. 0 ) lpdc = 1
      if (lpdc .eq. 2) go to 3600
      idsw7 = 0
      dcrepm = 0.0
      angdim = 0.0
      acperr = 0.0
      do i = 1, isg
         dcrep = abs(difpwr(i) - paccex(i))
         acperr = acperr + dcrep
C 
C        Bypass blocked ind mtr--see S 3045-1
C 
         if (paccex(i) .ne. -4000.0) then
            paccex(i) = difpwr(i)
            if (dcrepm .le. dcrep) then
               dcrepm = dcrep
               imx = i
            endif
         endif
      enddo

C     Conditionally print solution residuals at each iteration

      if (toli .ge. dcrepm .and. toli .ge. cmax) then
         if (keybrd(15) .gt. 0) then
            iresid = 1
            call prtresid (imx, acperr, dcrepm)
         endif
      else
         itol=2
         if (keybrd(15) .gt. 0) then
            call prtresid (imx, acperr, dcrepm)
         endif
      endif

C     tally loop cnt

      lppwr=lppwr+1
      go to 3520

C     Init pwr tol loop cntr

 2660 lppwr=0
      itrace = 2
      tc1=tc2
      tc2=tc3
      tc3 = tsim + edt
      ddt1=ddt2
      ddt2=edt
      d13=ddt1+ddt2
      tsim = tc3
      tnx = tsim + dt
 
      go to (2820,2920,2780,2800,2920,2920), idsw
 2780 idsw=4
      go to 2820
 2800 idsw=1
 2820 if (ivpc.eq.1) go to 2840
      matsw=2
      idsw=7
      link=2
      istat = 4
      return
 2840 go to (3240,2860,3180,3240,2880,2900), idsw
 2860 idsw=1
      go to 3240
 2880 idsw=6
      go to 3240
 2900 idsw=2
      go to 3240
 2920 if (dnx.ne.dnxmin) then
         if (ivpc.eq.1) go to 2840
         matsw=2
         idsw=7
         link=2
         istat = 4
         return
      endif
      ist=0
      if (keybrd(3) .ne. 0) then
         i=1081
         write (outbuf,2940) i,idsw
         call prtout (1)
 2940    format (' 1c', 2i5)
      endif
 2960 if (iswt.gt.ifcd) go to 3100
      if (dnx.ne.cyc(iswt)) go to 3120
      if (dnx .gt. tsim + 0.001) then
         if (ivpc.eq.1) go to 2840
         matsw=2
         idsw=7
         link=2
         istat = 4
         return
      endif
      icntl2=2
      ist=ist+1
      istem(ist)=iswt
      iswt=iswt+1
      go to 2960

 3100 dnx=endt+4.0*dt
      go to 3160

 3120 dnx=cyc(iswt)

 3160 go to (3180,3220,3180,3180,3220,3220), idsw
 3180 write (errbuf(1),3200) idsw, ivpc
 3200 format( '0CNTRL ... error in switching code .. idsw=', i2,
     1 '  ivpc=', i2)
      call prterr ('E',1)
      jexit=1
      go to 2200
 3220 idsw=7
      matsw=3
      link=2
      istat = 4
      return
c
c     Begin time step loop
c
 3240 dnxmin=amin1(dnx,dnxrly)
      finished = .false.
      flag = .true.              ! Flag set for bus freq calc.
      do while (.not. finished)
         if (dnxmin .le. tnx+0.0001) then
            edt = dnxmin - tsim
            isg3 = 0
            if (idsw .eq. 1) then
               idsw=2
            else if (idsw .eq. 2 .or. idsw .eq. 6) then
               finished = .true.
            else if (idsw .eq. 3) then
               idsw=5
               finished = .true.
            else if (idsw .eq. 4) then
               idsw=5
               finished = .true.
            else
               write (errbuf(1),3200) idsw, ivpc
               call prterr ('E',1)
               jexit=1
               go to 2200
            endif
         else
            isg3 = isg3 + 1
            if (isg3 .lt . istp) then
               if (edt .eq. dt) flag = .false.
               edt=dt
               finished = .true.
            else
               dt=dtc
               edt=dt
               finished = .true.
            endif
         endif
      enddo
c      
c     Calculate time factor for bus frequency filter
c      
      if (flag) then
         if (tbusf .ne. 0.0) then
            abus = 1.0 + 2.0*tbusf/edt
            abusr = 1.0/abus
         endif
         if (edt .ne. ddt2) then
            write (outbuf, 3360) to, ddt2, edt
 3360       format ('0',5x,'Change in time step at ',f7.2,
     1              ' cycles.  Old time step was ',f4.2,
     1              ' new time step is ',f4.2)
            call prtout (1)
            al=1.
            tfac=ddt2/edt
         endif
      endif

 3520 itsl = 0
c      
c     Begin machine equation solution loop.
c      
c     If any variable admittance elements exist, call VYSOL to
C     solve their differential equations
c      
      if (iznmax .gt. 0) call vysol
c
c     A "do while" statement is used because "ispf" is adjusted to
c     solve HP/LP governors in LP/HP order.
c
      ispf = 1
      do while (ispf .le. isg)    
         ispm = ispf-1
         igbn=igentn(1,ispf)
         iecs=igentn(2,ispf)
         idm = igentc(ispf)
         call redecs (datat,iecs, mts)
         mgen=igndta(1,ispf)
         lvref=igndta(2,ispf)

         if (mgen .eq. 9) then
c      
c           MGEN = 9 IS A STATIC VAR SOURCE NOT A MACHINE SO CALL SVSSOL
c      
            call svssol

         else if (consm .eq. 999999.) then
c      
c           CONSM = 999999. implies an infinite bus and is not processed
C           as a normal machine
c      
         else if (consm .eq. 0.) then
c      
c           CONSM = 0. implies a machine who's power has been reduced to
C           zero by generator dropping. it is not processed.

            angl(ispf)=-40000.
         else

            dampp = dmpfac
            if (mgen .ne.5 .and. mgen .ne. 8)  mex = 0
            if (mgen .eq. 2) dampp=0.
c      
c           Solve swing equation using trapezoidal rule.
c      
            call angsol
c      
c           Calculate field curr for 2 axis mach
c      
            if (mgen .ge. 6) then
               cfd = epqo + (epqo - edpqo) * (xd - xdp) / (xdp - xdpp)
               cfd = cfd * (1.0 + satd)                                 !wlp
               vfldtn(2,ispf) = cfd                                    
            endif

c           solve gov equations
            call govsol(ispf, edt, ddt2, wnew, wnow, lppwr, tsim)

            if (mgen .eq. 5 .or. mgen .eq. 8) then
C
C              Solve exciter and PSS equations
C
               call cntrla(itrnfr)
               if (itrnfr .eq. 2) go to 3540

            endif
            if (mgen .eq. 1) then
               igr=7
            else if (mgen.eq.2) then
               igr = 26
               vfldtn(1,ispf)=1000.
               vfldtn(2,ispf)=0.0
               call ritecs(datat, iecs, 26)
            else 
               igr=mts
               if (mgen.lt.6)igr=27
               call ritecs (datat(17),iecs+16,11)
            endif
            if (mgen .eq. 5 .or. mgen .eq. 8) then
               call ritecs (creg(6),mexecs+5,iex-5)
               igr=igr+iex
               if (msupp .gt. 0) then
                  call ritecs (csupp(4),msupec+3,isup1-3)
                  igr=igr+isup1
c    
c                 Check for PSS notch filter
c      
                  if (dnf .gt. 0.0) then
                     call ritecs(csupp(35),msupec+isup1+1,8)
                     igr = igr + 9
                  endif
               endif
            endif
c
c           Update
c
c      
            if (mgov .eq. 0) then
            else if (mgov .eq. 3 .and. idm .eq. 'H') then
c
c              Store the hp gov indices. Compute after lp gov 
c              solved.
c      
               igrh = igr
               mgvech = mgvecs
               spdslh = spdslp
               ispfh = ispf
               iecsh = iecs

            else

c              call ritecs (cgov(2),mgvecs+1,igv-1)
               igr=igr+igv
               if (keybrd(18) .ne. 0) then
                  name = bname(igbn)
                  call redecs (datat,iecs,igr)
                  write (outbuf,6360) name, mgen, mgov, mex,
     1                                msupp, lvref
 6360             format('0',a8,5i5,' mgen,mgov,mex,msupp,lvref')
                  call prtout (1)
                  do jjj = 5,igr,8
                     kkk = min0 (jjj+7,igr)
                     write (outbuf,6361) (i,datat(i),i=jjj,kkk)
 6361                format(2x,8(i3,e13.6))
                     call prtout (1)
                  enddo
               endif

            endif

            if (mgov .eq. 3 .and. igovc .eq. 1) then
c
c              If lp gov processed, go back an process hp gov
C              Pick up indexes and variables for HP M/C  ***
c      
               ispf = ispfh
               mgvecs = mgvech
               spdslp = spdslh
               iecs = iecsh
               igr = igrh
               igovc = -1
c      
c              call ritecs (cgov(2),mgvecs+1,igv-1)
               igr=igr+igv
               if (keybrd(18) .ne. 0) then
                  name = bname(igbn)
                  call redecs (datat,iecs,igr)
                  write (outbuf,6360) name, mgen, mgov, mex,
     1                                msupp, lvref
                  call prtout (1)
                  do jjj = 5,igr,8
                     kkk = min0 (jjj+7,igr)
                     write (outbuf,6361) (i,datat(i),i=jjj,kkk)
                     call prtout (1)
                  enddo
               endif
c      
C              Retrieve current index  ***
c     
               ispf=ispfl
               igovc=0

            endif
 3540    continue
         endif
         ispf = ispf + 1
      enddo
      ispm = isg

      al=0.0
 3600 continue
c
c     End of machine solution loop.
C     If first iteration, resolve network

      if (lppwr .eq. 0) go to 100
      if (itol .eq. 2) go to 3660

 3640 if (lppwr .eq. 1) go to 3660
      if (itoldc .eq. 2) go to 3660

C     Store past values of E,F

      to = tsim + edt
      idisw=1
      dnxmin=amin1(dnx,dnxrly)
c      
c     Update saturation factors
c      
      do i=1,isg
         iecs=igentn(2,i)
         call redecs(datat,iecs,27)
         mgen=igndta(1,i)
         if (mgen .gt. 3 .and. mgen .ne. 9) then
            if (mgen .gt. 5) call redecs(datat,iecs,39)
            if (esat .gt. 0.0) then
               i1= igentn(1,i)
               ed = eyri(i)
               eq = eyii(i)
               vpd = ed + ra*oid-xp*oiq
               vpq = eq + ra*oiq+xp*oid
               vpm = sqrt(vpd**2 + vpq**2)
               if (vpm.gt.esat) then
                  satd = (csat*(vpm-esat)*(vpm-esat))/vpm
                  satq= (xq/xd)*satd
               else
                  satd=0.0
                  satq=0.0
               endif
               call ritecs (satd,iecs+25,2)
               if (keybrd(26) .ne .0) then
                  write (outbuf,3657)i,vpm,satd,satq
 3657             format(' Ith GEN,VPOTIER,SATD,SATQ ', i5, 3e15.6)
                  call prtout (1)
               endif
            endif
         endif
      enddo

      i140 = 1
      do i=1, isg
         anglt(i)=angl(i)*57.2957795
      enddo
c      
c     Calculate maximum angle difference
c      
      call maxmin
c      
c     After 30 cycles check for low voltage condition.
c     (Call only for nonshort circuit condition (IESC =1))
c      
      if (lovtex .eq. 0 .and. to .gt. 30. .and. iesc .lt. 2) then
         call vtchk
      endif
      kexit=0
c    
c     Jexit = 3 Is the last time step in the study
c    
      if (to.ge.endt) jexit=3
c    
c     The following logic will write data to the .SOL file (l8) every
C     ITSKP time steps.  Discontinuities, manual exits, and the last
C     time step in the study are always written to the .SOL file.
c    
 2200 if (idsw .eq. 2 .or. idsw .gt. 4) go to 2201
      if (jexit .eq. 1 .or. jexit .eq. 3) go to 2201
      if (itskp .ne. 0) then
         iskkt = iskkt +1
         if (iskkt .ne. itskp) go to 2280
      endif
 2201 iskkt = 0
c      
c     Call WRTHIS to write solution data to history file for008
c     -  Save the solution for a normal step.  Here we also save the
c        minus side of a discontinuity.

      if (debug) then                                                   !dem
        call dbgeko ('CNTRL - saving history file at 1st WRTHIS call')  !dem
      endif                                                             !dem
      call wrthis
c    
c        JEXIT = 1 MEANS A MANUAL EXIT
c    
 2280 if (jexit .eq. 1) then
         write (errbuf(1), 2320)
 2320    format ('0Manual exit')
         call prterr ('W',1)
c      
c        Call ENDSUM to write underfrequency load shedding summary
c      
         call endsum

c        -  Add e-o-f marker to history file

         call puthisi (-1,1)
         call filhis 
         ntry=2
         link=3
         return
c      
c        JEXIT = 2 IS AN NORMAL TIME STEP
c      
      else if (jexit .eq. 2) then
         idsw7=0
         go to 120
c      
c        JEXIT = 3 IS THE LAST TIME STEP IN THE STUDY OR LOW VOLTAGE
c      
      else if (jexit .eq. 3) then
         if (lovlt .eq. 1) then
           write(errbuf(1),2350)
 2350      format('0 PROGRAM MANUALLY STOPPED DUE TO LOW VOLTAGE ',
     1            'CONDITION')
           call prterr('W',1)
         else
             write (outbuf, 2360)
 2360        format ('0 Scheduled time limit reached')
             call prtout (1)
         endif
c      
c        Call ENDSUM to write underfrequency load shedding summary
c      
         call endsum

c        -  Add e-o-f marker to history file

         call puthisi (-1,1)
         call filhis 
         ntry=2
         link=3
         return
      endif

c     -  Go back & solve network unless maxed out on iterations

 3660 if (lppwr .lt. lpqit) go to 100
      write (outbuf,3680)
 3680 format (' Power exterpolation loop limit reached')
      call prtout (1)
c      
c     Check if interface iter limit termination
c      
      if (lsolqt.eq.1) then
        jexit=1
        go to 2200
      else
         iresid=2
         call prtresid (imx, acperr, dcrepm)
         go to 3640
      endif
      end
