C    %W% %G%
      subroutine znoint
C     
C     THIS SUBROUTINE CALCULATES THE INITAL VALUES FOR
C     THE VARIABLE ADMITTANCE MODEL
C     
      include 'tspinc/params.inc'
      include 'tspinc/znox.inc'
      include 'tspinc/znox2.inc'
      include 'tspinc/vy1.inc'
      include 'tspinc/ecsind.inc'
      include 'tspinc/busvolt.inc'
      include 'tspinc/comn34.inc'
      include 'tspinc/namec.inc'
      include 'tspinc/pointr.inc'
      include 'tspinc/prt.inc'
      include 'tspinc/vymn.inc'

      complex zsyn1, zsyn2, zsyn2p, vmeas, aimeas, vsyn1, vsyn2
      character*8 name1, name2

      do i = 1, iznmax
        name1 = exnamc(iznbus(i))
        name2 = exnamc(jznbus(i))
C       
C       IF A VARIABLE ADMITTANCE ELEMENT EXISTS ON THIS LINE, THEN
C       COMBINE TOTAL VARIABLE ADMITTANCE WITH EXISTING ZNO ADMITT
C       
        if (vygtot(i) .ne. 0.0 .or. vybtot(i) .ne. 0.0) then
          recp1 = 1./(vygtot(i)*vygtot(i)+vybtot(i)*vybtot(i))
        else
          recp1 = 0.0
        endif
        if (zngc(i) .ne. 0.0 .or. znbc(i) .ne. 0.0) then
          recp2 = 1./(zngc(i)*zngc(i)+znbc(i)*znbc(i))
        else
          recp2 = 0.0
        endif
        rtot = vygtot(i)*recp1 + zngc(i)*recp2
        xtot =  - (vybtot(i)*recp1+znbc(i)*recp2)
        recp1 = 1./(rtot*rtot+xtot*xtot)
        zng12(i) = rtot*recp1
        znb12(i) =  - xtot*recp1
        call znmat(i)
        if (vyalfi(i) .ne. 0.0) then
C         
C         CALCULATE  INITIAL LINE CURRENT
C         
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
          vyilo(i) = vyil(i)
c
c         Initialize synthethic angle for output
c
          riparl = real(iparl(i))
          xtcsc = xtcscge(i) / riparl
          air = ctr*riparl
          aii = cti*riparl
          zsyn1 = cmplx(0.0, zsyn1ge(i))
          zsyn2 = cmplx(0.0, zsyn2ge(i))
 
C         Rotate reference angle and get Vmeas and Imeas
 
          vmag = sqrt(eg1*eg1+fg1*fg1)
          vmeas = cmplx(vmag, 0.0)
          tha = atan2(fg1, eg1)
          aim = sqrt(air*air+aii*aii)
          thi0 = atan2(aii, air)
          thi = thi0 - tha
          aimeas = cmplx(aim*cos(thi), aim*sin(thi))
 
C         Calculate synthesis voltages
 
          zsyn2p = zsyn2 + cmplx(0.0, xtcsc)
          vsyn1 = vmeas + zsyn1*aimeas
          vsyn2 = vmeas - zsyn2p*aimeas
 
C         calculate equivalant angle diference
 
          angle1 = aimag(vsyn1)/real(vsyn1)
          angle2 = aimag(vsyn2)/real(vsyn2)
          dangl(i) = (angle1-angle2)
C         
C         INITIALIZE PAST ANGLE FOR FREQ CALCULATION
C         
          vyango(i) = atan2(eg1, fg1)
C         
C         CALCULATE INITIAL BUS VOLTAGE MAGNITUDE
C         
          vyvb(i) = sqrt(eg1*eg1+fg1*fg1)
          vyvbo(i) = vyvb(i)
C         
C         INITIALIZE INPUT IVYPI =1 IS LINE POWER
C         IVYPI = 2 IS LINE CURRENT
C         
          if (ivypi(i) .eq. 1) then
            vypint(i) = eg1*ctr + fg1*cti
          else
            vypint(i) = vyil(i)
          endif
          if (ivypi(i) .eq. 1) then
C           
C           CHECK FOR INITIAL LIMIT VIOLATIONS
C           
            if (vypint(i) .gt. vypmax(i)) then
              write (errbuf(1), 10000) name1, name2
10000         format ('0', 5x, 'VARIABLE ADMITTANCE ELEMENT BETWEEN ',
     &         a8, ' AND ', a8, ' SINT EXCEEDS SMAX')
              call prterr('E', 1)
              iabort = 1
            endif
            if (vypint(i) .lt. vypmin(i)) then
              write (errbuf(1), 10010) name1, name2
10010         format ('0', 5x, 'VARIABLE ADMITTANCE ELEMENT BETWEEN ',
     &         a8, ' AND ', a8, ' SINT LESS THEN SMIN')
              call prterr('E', 1)
              iabort = 1
            endif
          endif
C         
C         CALCULATE INITIAL STATE VECTORS
C         
          if (ivypi(i) .eq. 1) then
            cref = vypint(i)/vyvb(i)
          else
            cref = vypint(i)
          endif
          if (cref .gt. vyimax(i)) then
            write (errbuf(1), 10020) name1, name2
10020       format ('0', 5x, 'VARIABLE ADMITTANCE ELEMENT BETWEEN ',
     &       a8, ' AND ', a8, ' C REF EXCEEDS IMAX')
            call prterr('E', 1)
            iabort = 1
          endif
          if (cref .lt. vyimin(i)) then
            write (errbuf, 10030) name1, name2
10030       format ('0', 5x, 'VARIABLE ADMITTANCE ELEMENT BETWEEN ',
     &       a8, ' AND ', a8, ' CREF LESS THEN IMIN')
            call prterr('E', 1)
            iabort = 1
          endif
          vyx1 = (cref-vyil(i))*vyk1(i)
          vyalfa(i) = vyx1
          vyalfo(i) = vyx1
C         
C         CALCULATE INITIAL PAST VALUE FACTORS
C         
          vyhb1(i) = vyvb(i)*(vyar1(i)-1)
          vyhb2(i) = vyil(i)*(vyar2(i)-1)
          vyhb3(i) = vyx1*(vyar5(i)-vyar3(i))
          vyhb4(i) = vyx1*(vyar6(i)-vyar4(i))
        endif
      enddo
      return
      end
