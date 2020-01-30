C    @(#)nrsoln.f	20.8 1/4/99
      subroutine nrsoln (iter, itstop, idchgx, kswsln, itlow) 
C
C     Input parameters:
C
C     ITER   : Beginning iteration count.
C     ITSTOP : Limiting iteration count.
C     IDCHGX : D_C initialization switch -
C              0 - D-C system must be initialized for NR solution.
C              1 - D-C system ready.
C     KSWSLN : Auxiliary variable KSW -
C              1 - Seek continuous solution.
C              2 - Convert to discrete values.
C              3 - Seek discrete solution.
C
C     Output parameters:
C
C     ITER   : Final iteration count.
C     KSWSLN : Auxiliary variable KSW -
C              1 - Seek continuous solution.
C              2 - Convert to discrete values.
C              3 - Seek discrete solution.
C     ITLOW  : Iteration number yielding best solution.
C
 
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/alpha2.inc'
      include 'ipfinc/amtrx.inc'
      include 'ipfinc/area.inc'
      include 'ipfinc/beta2.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/dcinit.inc'
      include 'ipfinc/ecvar.inc'
      include 'ipfinc/errbus.inc'
      include 'ipfinc/gamma.inc'
      include 'ipfinc/gendrp.inc'
      include 'ipfinc/intbus.inc'
      include 'ipfinc/itrhis.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/optim2.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/pqcurves.inc'
      include 'ipfinc/slnopt.inc'
      include 'ipfinc/slnphs.inc'
      include 'ipfinc/tbx.inc'
      include 'ipfinc/tran.inc'
      include 'ipfinc/trmdbg.inc'
      include 'ipfinc/ordsta.inc'
C
      include 'ipfinc/dcparm.inc'   
      include 'ipfinc/grlf01.inc'
      include 'ipfinc/flag.inc'  
 
      common /txstat/ txstat(MAXLTC)
      integer txstat

      common /txsens/ ltxsen(MAXLTC), txsens(2,MAXLTC)

      common /drptmp/ drptmp
 
      common /aant/ pintdc(100), aant(60) 
      common /airea/ iswnga  

      external nrpqv
      logical dropst, dc, full, final, updtp 
      integer       dcdate(3), dcpage, dctime(3), status
      character*20  dchead 
      double precision rh, rj, rn, rl, rhin, rlin, dptk, dqtk, dptm, 
     &       dqtm, an, ah, xh, xj, xn, xl

      save pqstop, dpqlo
C
C     DROPST : Status of Generation Dropping
C
C     Set up pointers to TBX data

      if (tbx_loaded .ne. ordtbx) then
        do nb = 1, ntot
          ptrtbx(nb) = 0
        enddo

        do i = 1, ntotb
          nb = tbx(2, i)
          if (nb .gt. 0) then
            kt = inp2opt(nb)
            ptrtbx(kt) = i
          endif
        enddo
        tbx_loaded = ordtbx
      endif

      msw = kswsln
      idchg = idchgx
      dropst = (numdrp .gt. 0)
      updtp = .false. 
      full = .false. 
      final = .false. 
      interv = 0
      dc = .not. (ndccon .eq. 0 .and. ndclin .eq. 0) 
C
C     Initialize LTC status
C
      do i = 1,ntota
         txstat(i) = 0
         ltxsen(i) = 0
         txsens(1,i) = 0.0
         txsens(2,i) = 0.0
      enddo

      if (iter .eq. 0) then
         pqstop = 0.0
         dpqlo = 1.0e10
      endif
 
  100 iter = iter + 1
      ittot = iter
      if (iter .gt. itstop) go to 1170
      ikec=1
      ierr=0
      kownt=0
      kownta=0
      kowntb=0
      kowntc=0
      kowntd=0
      dptot=0.0
      dqtot=0.0
      dttot=0.0
      datot=0.0
      ddtot=0.0
C
C     Initialize VOLT array
C
      do i = 1,ntot
         volt(i) = 0.0
      enddo
C
C     Determine DC terminal quantities
C
      if (idcsw .eq. 1) then
         if (iopton(38) .eq. 0) then
            call nrdc
         else
            call nrdclp
         endif
      endif
C
C     Preprocess 1. Type BQ buses spanned with an LTC transformer, and
C                2. % VAR generators
C                3. Special bus types
C
      if (ittot.ge.3) then
c
c        Update Qmax, Qmin limits
c
         do i = 1, numcurv
           nb = pqbusptr(i)
           kt = inp2opt(nb)
           call pqcurv (i, ptrtbx(kt), kerr)
         enddo

         call nrycmp
         call nrtx
         call nrqpct
         call nrqlim
         call nragc
      endif
C
C     Compute Jacobian elements for LTC
C
      do jt=1,ntota
         jndex(1,jt) = ikec
         kstat = jacltc (jt,0)
         i = 1

C        Normalize row

         rh=rowh(i)
         rhin=dble(1.0)/rh
         an=rown(i)*rhin
         rj=rowj(i)
         rl=rowl(i)
         rlin=rl-rj*an
         if (rlin .eq. 0.0) rlin = 1.0d-10
         rlin=dble(1.0)/rlin
         if (koptsw.gt.2) then
            amtrx(ikec)=rhin
            amtrx(ikec+1)=rj
            amtrx(ikec+2)=rlin
            ikec=ikec+3
         endif
         dpt(1,jt) = dpt(1,jt)*rhin
         dpt(2,jt) = (dpt(2,jt) - rj*dpt(1,jt))*rlin
         jndex(2,jt)=ikec
         amtrx(ikec)=an
         ikec = ikec + 1
         do i = 2,lp
            mt = kolum(i)
            ah = rowh(i)*rhin
            an = rown(i)*rhin
            amtrx(ikec)=mt
            amtrx(ikec+1) = ah
            amtrx(ikec+2) = (rowj(i) - rj*ah)*rlin
            amtrx(ikec+3) = an
            amtrx(ikec+4) = (rowl(i) - rj*an)*rlin
            ikec=ikec+5
         enddo
      enddo
C
C     Add identity row for slack bus elements
C
      do kta=ntota+1,ntota+nbslck
         jndex(1,kta) = ikec

C        NORMALIZE ROW

         if (koptsw.gt.2) then
            amtrx(ikec)=dble(1.0)
            amtrx(ikec+1)=0.0
            amtrx(ikec+2)=dble(1.0)
            ikec=ikec+3
         endif
         dpt(1,kta) = 0.0
         dpt(2,kta) = 0.0
         jndex(2,kta)=ikec
         amtrx(ikec)=0.0
         ikec = ikec + 1
      enddo
C
C     Define Jacobian elements for bus constraints
C
      do 1020 kt = nbslck+1,ntot
      kta = kt + ntota
      jndex(1,kta) = ikec
      call jacbus (kt,0)
      dptk = dpt(1,kta)
      dqtk = dpt(2,kta)
      mel=0
  640 mel=korder(mel)
      mt=kolum(mel)
      if (mt-kta) 740,644,642
C
C     Eliminate column MT from working row
C
  740 ik=jndex(2,mt)
      ikstop=jndex(1,mt+1)
      xn = amtrx(ik)
      ik = ik + 1
      rh=rowh(mel)
      rn=rown(mel)-rh*xn
      rj=rowj(mel)
      rl=rowl(mel)-rj*xn
      if (koptsw.gt.2) then
         amtrx(ikec)=mt
         amtrx(ikec+1)=rh
         amtrx(ikec+2)=rn
         amtrx(ikec+3)=rj
         amtrx(ikec+4)=rl
         ikec=ikec+5
      endif
      dptm=dpt(1,mt)
      dqtm=dpt(2,mt)
      dptk=dptk-rh*dptm-rn*dqtm
      dqtk=dqtk-rj*dptm-rl*dqtm
      krw=mel
C
C     Perform Row MT elimination
C
  800 if (ik.ge.ikstop) go to 640
      ml=amtrx(ik)
      if (ml.gt.max) go to 980
  960 if (kolum(krw)-ml) 970,1010,990
  970 ko=krw
      krw=korder(krw)
      go to 960

 1010 mlc=krw
      go to 1000

  980 max=ml
      ko=lp
      lp=mend
  990 korder(mend)=korder(ko)
      kolum(mend) = ml
      korder(ko)=mend
      mlc=mend
      ko=mend
      mend=mend+1
      rowh(mlc) = 0
      rown(mlc) = 0
      rowj(mlc) = 0
      rowl(mlc) = 0
 1000 continue
      xh=amtrx(ik+1)
      xj=amtrx(ik+2)
      rowh(mlc) = rowh(mlc) - rh*xh - rn*xj
      rowj(mlc) = rowj(mlc) - rj*xh - rl*xj
      xn=amtrx(ik+3)
      xl=amtrx(ik+4)
      rown(mlc) = rown(mlc) - rh*xn - rn*xl
      rowl(mlc) = rowl(mlc) - rj*xn - rl*xl
      ik=ik+5
      go to 800
C
C     Error - no residual diagonal element
C
  642 call erexit
C
C     Normalize row
C
  644 rh=rowh(mel)
      rhin=dble(1.0)/rh
      an=rown(mel)*rhin
      rj=rowj(mel)
      rl=rowl(mel)
      rlin=rl-rj*an
      if (rlin .eq. 0.0) rlin = 1.0d-10
      rlin=dble(1.0)/rlin
      if (koptsw.gt.2) then
         amtrx(ikec)=rhin
         amtrx(ikec+1)=rj
         amtrx(ikec+2)=rlin
         ikec=ikec+3
      endif
      dpt(1,kta) = dptk*rhin
      dpt(2,kta) = (dqtk - rj*dpt(1,kta))*rlin
      jndex(2,kta)=ikec
      amtrx(ikec)=an
      ikec = ikec + 1
  690 mel = korder(mel)
      if (mel.eq.0) go to 1020
      mt = kolum(mel)
      ah = rowh(mel)*rhin
      an = rown(mel)*rhin
      amtrx(ikec)=mt
      amtrx(ikec+1) = ah
      amtrx(ikec+2) = (rowj(mel) - rj*ah)*rlin
      amtrx(ikec+3) = an
      amtrx(ikec+4) = (rowl(mel) - rj*an)*rlin
      ikec=ikec+5
      go to 690

 1020 continue
      jndex(1,ntotx) = ikec
C
C     END OF BUS LOOP
C
      write (outbuf,1040) ittot,dptot,dqtot,dttot,datot,ddtot,
     1   kownt,kownta,kowntc,kowntd,kowntb,jndex(1,ntotx)
 1040 format(8x,i3,2f14.5,3f13.5,i7,3i6, 7x, i7,i10 )
      call prtout(1)

      itx = itrhis(1) + ittot
      do i = ierr+1,8
         ibuser(1,i,itx) = 0
      enddo

      do i = 1,8
         buser(5,i,itx) = 0
      enddo

      dpqnu=sqrt(dptot**2+dqtot**2)
      if (ittot.gt.3) go to 1090
      dpqlo = amin1 (dpqnu, dpqlo)
      pqstop = amax1 (pqstop,50.0*dpqnu)
      itlow=ittot
      if (knum.eq.2) go to 1080
      knum=2
      knew=2
      go to 1100

 1080 continue
      pqstop=amax1(pqstop,10000.0)
      go to 1100

 1090 if (dpqnu.gt.pqstop) go to 1180
      if (dpqnu.gt.dpqlo) go to 1100
      itlow=ittot
      dpqlo=dpqnu
 1100 continue
C
C     Initialize correction history array.
C
      itx = itrhis(1) + ittot
      do 1102 i = 1, 8
 1102 buser(5,i,itx) = 0
      if (msw.eq.1) then
         ierr = 0
         status = nrbksl()
         if (status .ne. 0) go to 1180
         ko=kownta/1000
         if (ko .gt. 0) then
            write (outbuf,1130) ko
 1130       format('+',101x,i7)
            call prtout(1)
         endif

         if (kownt + kownta + kowntb + kowntc + kowntd .eq. 0 .and.
     1      ittot .gt. 3) then
C
C           Converged continuous solution - Process /GEN_DROP
C
            itstop = max0(itstop,ittot+8)
            if (dropst) then
               call comdrp ('NR Iteration', ittot, num1, drptmp, num2,
     1                      drop2, status, nrpqv, itrx)
               if (abs (drptmp) .le. drptol .or. itrx .gt. 5) then
                  dropst = .false.
               endif
               go to 100
            else
               msw = 2
            endif
         endif
      else if (msw.eq.2) then
         msw = 3
         if (kownt+kownta+kowntb+kowntc+kowntd.eq.0) then
            go to 1160
         else
            ierr = 0
            status = nrbksl()
            if (status .ne. 0) go to 1180
            ko=kownta/1000
            if (ko .gt. 0) then
               write (outbuf,1130) ko
               call prtout(1)
            endif
         endif
      else
         if (kownt+kownta+kowntb+kowntc+kowntd.eq.0) then
            go to 1160
         else
            ierr = 0
            status = nrbksl()
            if (status .ne. 0) go to 1180
            ko=kownta/1000
            if (ko .gt. 0) then
               write (outbuf,1130) ko
               call prtout(1)
            endif
         endif
      endif
      go to 100

 1160 write (outbuf,1162)
 1162 format ('0 SUCCESSFUL SOLUTION REACHED.')
      call prtout(1)
      itlow=ittot
      dpqlo=dpqnu
      lskp=1
      go to 1390
 
 1170 ittot=ittot-1
      iter = ittot
 
 1180 continue
      write (outbuf,1190) itlow
 1190 format ('0 SUCCESSFUL SOLUTION NOT REACHED - VOLTAGES AND TAPS ',
     1'FROM ITERATION',i3,' WILL BE USED IN THE FOLLOWING LISTINGS.')
      call prtout(1)
      lskp=3
 
      write (*,1360)
 1360 format (' FAILED SOLUTION')
C
C     Update solution parameters
C
 1390 idchgx = idchg
      kswsln = msw
      return
      end
