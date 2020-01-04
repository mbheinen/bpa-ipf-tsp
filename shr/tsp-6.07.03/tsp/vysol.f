C    %W% %G%
      subroutine vysol
 
C     THIS SUBROUTINE SOLVES THE DIFFERENTIAL EQUATIONS ASSOCIATED
C     WITH THE VARIABLE ADMITTANCE MODEL
 
      include 'tspinc/params.inc'
      include 'tspinc/param.inc'
      include 'tspinc/comvar.inc'
      include 'tspinc/lnk12.inc'
      include 'tspinc/vrgov.inc'
      include 'tspinc/vym.inc'
      include 'tspinc/znox.inc'
      include 'tspinc/busvolt.inc'
      include 'tspinc/znox2.inc'
      include 'tspinc/vy1.inc'
      include 'tspinc/ecsind.inc'
      include 'tspinc/prt.inc'
      include 'tspinc/namec.inc'
      include 'tspinc/bname.inc'
      include 'tspinc/buskv.inc'
C     added by yu wang
      include 'tspinc/vymn.inc'
      include 'tspinc/spare1.inc'
 
      complex zsyn1, zsyn2, zsyn2p, vmeas, aimeas, vsyn1, vsyn2

      data pi/3.1415927/

      do i = 1, iznmax
        if (vyalfi(i) .ne. 0.0) then
 
          zbase = buskv(iznbus(i))**2/bmva
 
C         added by YU Wang
 
          rrgb = vygtot(i)**2 + vybtot(i)**2
          rtcsc = vygtot(i)/rrgb
 
          if (lppwr .eq. 0) then
 
C           CALCULATE STATE VECTORS FROM LAST TIME STEP
 
            vyalfo(i) = vyalfa(i)
            vyvb1(i) = (vyvbo(i)+vyhb1(i))/vyar1(i)
            vyil1(i) = (vyilo(i)+vyhb2(i))/vyar2(i)
            if (ivypi(i) .eq. 1) then
              cref = vypint(i)/vyvb1(i)
            else
              cref = vypint(i)
            endif
            if (cref .gt. vyimax(i)) cref = vyimax(i)
            if (cref .lt. vyimin(i)) cref = vyimin(i)
            vyx1 = (cref+vymdo(i)-vyil1(i))*vyk1(i)
 
C           IF MODULATION IS PRESENT, USE ONLY MODULATION SIGNAL
C           AS INPUT
 
            if (ivymsw(i) .ne. 0) vyx1 = vymdo(i)*vyk1(i)
            vyx2 = (vyx1*vyar3(i)+vyhb3(i))/vyar5(i)
 
C           MODIFY TIME FACTORS IF THE TIME STEP HAS CHANGED
 
            if (al .ne. 0.0) then
              vyar1(i) = vyar1(i)*tfac - tfac + 1.
              vyar2(i) = vyar2(i)*tfac - tfac + 1.
              vyar3(i) = vyar3(i)*tfac - tfac + 1.
              vyar4(i) = vyar4(i)*tfac - tfac + 1.
              vyar5(i) = vyar5(i)*tfac - tfac + 1.
              vyar6(i) = vyar6(i)*tfac - tfac + 1.
            endif
 
C           UPDATE PAST ANGLE FOR FREQ CALCULATION
 
            eg1 = eyr(iznbus(i))
            fg1 = eyi(iznbus(i))
            vyango(i) = atan2(eg1, fg1)
 
C           AT A DISCONTINUITY INPUT STATE VECTORS MUST BE
C           RE INITITALIZED
 
            if (idsw .eq. 3 .or. idsw .eq. 5) then
              eg2 = eyr(jznbus(i))
              fg2 = eyi(jznbus(i))
              zng = zngij(i) + zngii(i)
              znb = znbij(i) + znbii(i)
              c1r = eg1*zng - fg1*znb
              c1i = eg1*znb + fg1*zng
              c2r = eg2*zngij(i) - fg2*znbij(i)
              c2i = eg2*znbij(i) + fg2*zngij(i)
              ctr = c1r - c2r
              cti = c1i - c2i
              vyil(i) = sqrt(ctr*ctr+cti*cti)
              ptie(i) = eg1*ctr + fg1*cti
              vyilo(i) = vyil(i)
 
C             CALCULATE  BUS VOLTAGE MAGNITUDE
 
              vyvb(i) = sqrt(eg1*eg1+fg1*fg1)
              vyvbo(i) = vyvb(i)
            endif
 
C           CALCULATE PAST VALUE FACTORS
 
            vyhb1(i) = vyvb(i) - vyvb1(i)*(2.-vyar1(i))
            vyhb2(i) = vyil(i) - vyil1(i)*(2.-vyar2(i))
            vyhb3(i) = vyx1*(2.-vyar3(i)) - vyx2*(2.-vyar5(i))
            vyhb4(i) = vyx2*(2.-vyar4(i)) - vyalfa(i)*(2.-vyar6(i))
            vyaa(i) = vyar3(i)*vyar4(i)/(vyar5(i)*vyar6(i))
            vyab(i) = vyhb3(i)*vyar4(i)/(vyar5(i)*vyar6(i)) + vyhb4(i)
     &       /vyar6(i)
          endif
 
C         CALCULATE  LINE CURRENT
 
          eg1 = eyr(iznbus(i))
          fg1 = eyi(iznbus(i))
          eg2 = eyr(jznbus(i))
          fg2 = eyi(jznbus(i))
          zng = zngij(i) + zngii(i)
          znb = znbij(i) + znbii(i)
          c1r = eg1*zng - fg1*znb
          c1i = eg1*znb + fg1*zng
          c2r = eg2*zngij(i) - fg2*znbij(i)
          c2i = eg2*znbij(i) + fg2*zngij(i)
          ctr = c1r - c2r
          cti = c1i - c2i
          vyil(i) = sqrt(ctr*ctr+cti*cti)
          ptie(i) = eg1*ctr + fg1*cti
          vyilo(i) = vyil(i)
 
C         added by Yu Wang for TCSC test
          tyw = tnx - 1.0
          ptcsc = (eg1*ctr+fg1*cti)*bmva
          aitcsc = sqrt(ctr*ctr+cti*cti)*200.	! This coefficient is unkn
 
C         CALCULATE BUS FREQUENCY DEVIATION
 
          presa = atan2(eg1, fg1)
          delan = presa - vyango(i)
          if (delan .lt. -3.14159) delan = delan + 6.28318
          if (delan .gt. 3.14159) delan = delan - 6.28318
          delfeq = abs(delan/edt)
 
C         IF DEVIATION IS GREATER THAN OMEGA MAX SET ANGLE TO MIN
 
          if (delfeq .gt. vyfreq(i)) then
            alfa =  - 1.
          else
 
C           SET SIGN OF GAIN ACCORDING TO LINE COMPENSTATION
C           GAIN IS POSITIVE FOR CAPACITIVE LINE
C           GAIN IS NEGATIVE FOR INDUCTIVE LINE
 
            vyil(i) = sqrt(ctr*ctr+cti*cti)
            vyilo(i) = vyil(i)
 
C           CALCULATE  BUS VOLTAGE MAGNITUDE
 
            vyvb(i) = sqrt(eg1*eg1+fg1*fg1)
            vyvbo(i) = vyvb(i)
 
C           CALCULATE STATE VECTORS
 
            vyvb1(i) = (vyvb(i)+vyhb1(i))/vyar1(i)
            vyil1(i) = (vyil(i)+vyhb2(i))/vyar2(i)
 
C           separate different controls
 
            itcsc = iranityp(i)
            if (itcsc .eq. 2) then
 
C             BPA RANI
 
C             CALL VYMDS TO SOLVE EQUTAIONS FOR MODULATION
 
              if (ivymsw(i) .ne. 0) call vymds(i)
              if (ivypi(i) .eq. 1) then
                cref = vypint(i)/vyvb1(i)
              else
                cref = vypint(i)
              endif
              if (cref .gt. vyimax(i)) cref = vyimax(i)
              if (cref .lt. vyimin(i)) cref = vyimin(i)
              cerr = cref + vymmd(i) - vyil1(i)
 
C             IF MODULATION IS PRESENT, USE ONLY MODULATION SIGNAL
C             AS INPUT
 
              if (ivymsw(i) .ne. 0) cerr = vymmd(i)
              vyx1 = cerr*vyk1(i)
 
              xxc = vyx1

            elseif (itcsc .eq. 3) then
 
C             Direct TCSC control. The TCSC reactance is defined by
C             four corner points from the RZ F record (see ZNOINP),  
c             Tsu-huei Liu, Nov. 1993.
 
              if (tyw .le. vymca(i)) then
                xxc = 0.0
              elseif (tyw .le. vymcc(i)) then
                xxc =  - vymcb(i)
              elseif (tyw .le. vymce(i)) then
                xxc =  - vymcd(i)	! In p.u.
              elseif (tyw .le. vymarf0(i)) then
                slope = (vymca0(i)-vymcf(i))/(vymarf0(i)-vymce(i))
                xxc =  - (tyw-vymce(i))*slope - vymcf(i)
              else
                xxc =  - vymca0(i)

C               End of using data entered on RZ F card:  Time and Xorder
              endif
c
c             Compute synthetic angle for output
c
              riparl = real(iparl(i))
              xtcsc = (xxc + xtcscyw(i)) / riparl
              air = ctr*riparl
              aii = cti*riparl
              zsyn1 = cmplx(0.0, zsyn1ge(i))
              zsyn2 = cmplx(0.0, zsyn2ge(i))
 
C             Rotate reference angle and get Vmeas and Imeas
 
              vmag = sqrt(eg1*eg1+fg1*fg1)
              vmeas = cmplx(vmag, 0.0)
              tha = atan2(fg1, eg1)
              aim = sqrt(air*air+aii*aii)
              thi0 = atan2(aii, air)
              thi = thi0 - tha
              aimeas = cmplx(aim*cos(thi), aim*sin(thi))
 
C             Calculate synthesis voltages
 
              zsyn2p = zsyn2 + cmplx(0.0, xtcsc)
              vsyn1 = vmeas + zsyn1*aimeas
              vsyn2 = vmeas - zsyn2p*aimeas
 
C             calculate equivalant angle diference
 
              angle1 = aimag(vsyn1)/real(vsyn1)
              angle2 = aimag(vsyn2)/real(vsyn2)
              dangl(i) = (angle1-angle2)

            elseif (ivymsw(i) .ne. 0) then

C             GE control

              call vymds(i)
 
C             Set up a control blocker to avoid the negative 
c             contribution.  Bill Mittelstadt and Pat Dolan requested 
c             removing the multiple vyk1(i) from following line 
c             Tsu-huei Liu, 11/18/93.
 
              xxc = vymmd(i)	
              write (57, 10000) xxc
 
C             SEPARATE DIFFERENT CONTROL BLOCKERS

              if (ibtyp(i) .eq. 2) then
                blocker(i) = 1.0
                if (tnx .le. 60.0 .and. xxc .ge. 0.0) blocker(i) = 0.0
              elseif (ibtyp(i) .ne. 3) then
                blocker(i) = 1.0
              elseif (iblok(i) .eq. 0) then
                blok = ptie(i)
                dblok = (blok-blok0(i))/edt
                rp = (p4blok0(i)-blok)/p4blok0(i)
                if (abs(rp) .lt. 0.01) rp = 0.
                if (tnx .gt. tnxyw0 .and. dblok .gt. 0.0 .and. rp .le.
     &           0.1 .and. rp .gt. 0.03) then
                  blocker(i) = 1.0
                  iblok(i) = 1
                endif
              endif
 
              xxc = xxc*blocker(i)
 
              icomb = 0
              if (icomb .eq. 1) then
 
C               Combined control of GE TCSC (after 2 seconds) and fixed
C               logic control(before 2 seconds)
 
C               Turn on GE TCSC control (in VYMDI, IGEON=0)
 
                if (tnx .gt. 140.00 .and. abs(xxc) .le. 0.0005) igeon
     &           (i) = 1
                if (igeon(i) .eq. 1) then
                  xxc1 = xxc
                else
                  xxc1 = 0.0
                endif
 
C               no GE control component(only fixed logic control applied
C               if IGETCSC=0
 
                igetcsc = 1
                if (igetcsc .eq. 0) xxc1 = 0.0
 
                xxc0 = 0.
                tnx1 = tnx - 1.0
 
                if (tnx1 .ge. 30.00 .and. tnx1 .le. 140.0000) xxc0 =  -
     &           21.09/zbase
                if (tnx1 .gt. 140.00 .and. tnx1 .le. 240.00) xxc0 =
     &           (21.09*tnx1/100.0-50.62)/zbase
                if (tnx1 .gt. 240.0) xxc0 = 0.0
 
                xxc = xxc0 + xxc1
 
              endif
            endif
 
C           CHECK RATE OF CHANGE OF ALPHA FOR LIMIT VIOLATION
 
            alfa = vyalfi(i) + vyalfa(i)
          endif
          sigma = 2.*(pi-alfa)
          bv = (sigma-sin(sigma))/(pi*vyxint(i))
          if (.not. (vybmax(i) .eq. 0.0 .and. vybmin(i) .eq. 0.0)) then
            if (cos(2.*alfa) .ne. -1.) then
              alfmax = vybmax(i)*bv*bv*pi*vyxint(i)/(2.*(1.+cos(2.
     &         *alfa)))
              alfmin =  - vybmin(i)*bv*bv*pi*vyxint(i)/(2.*(1.+cos(2.
     &         *alfa)))
              dalfa = (vyalfa(i)-vyalfo(i))/edt
              if (dalfa .gt. 0.0) then
                if (dalfa .gt. alfmax) vyalfa(i) = alfmax*edt + vyalfo
     &           (i)
              endif
              if (dalfa .lt. 0.0) then
                if (dalfa .lt. alfmin) vyalfa(i) = alfmin*edt + vyalfo
     &           (i)
              endif
            endif
          endif
 
C         CHECK FOR ALPHA LIMIT VIOLATION
 
          if (alfa .gt. vyamax(i)) vyalfa(i) = vyamax(i) - vyalfi(i)
          if (alfa .lt. vyamin(i)) vyalfa(i) = vyamin(i) - vyalfi(i)
          alfa = vyalfa(i) + vyalfi(i)
 
C         CALCULATE NEW ADMITTANCE USING FIRING ANGLE ALPHA
 
          sigma = 2.*(pi-alfa)
          bv = (sigma-sin(sigma))/(pi*vyxint(i))
 
C         COMBINE EXISTING FIXED ADMITTANCE WITH NEW VARIABLE Y
 
          vygtot(i) = vygo(i) + vygv(i)
          vybtot(i) =  - vybo(i) - bv
 
C         Added by Yu Wang for TCSC control test
 
          xtcsc = xxc + xtcscyw(i)
          if (xtcsc .lt. xtcscmi(i)) xtcsc = xtcscmi(i)
          if (xtcsc .gt. xtcscma(i)) xtcsc = xtcscma(i)
 
C         print out XXC
 
          xxc = xtcsc - xtcscyw(i)
          xxcp = xxc*zbase
          write (65, 10000) xxcp
10000     format (1x, 1pe15.3)
 
C         Store XTCSCGE for GESIGL.FOR
 
          xtcsc = xxc + xtcscyw(i)
          xtcscge(i) = xtcsc
 
          rrr2 = rtcsc*rtcsc + xtcsc*xtcsc
          vygtot(i) = rtcsc/rrr2
          vybtot(i) =  - xtcsc/rrr2
          xtcscp =  - xtcsc*zbase
          if (lppwr .eq. 0) write (58, 10010) tyw, xtcscp
10010     format (1x, 1pe15.3, 3x, 1pe15.3)
          ptie1(i) = ptie(i)
          tnxyw0 = tnx
          blok0(i) = blok
        endif
      enddo
      return
      end
