C    %W% %G%
      subroutine rmsol
C     
C     This subroutine contains the solution logic for the
C     underfrequency generator dropping relay.  It is called
C     by RELAY.
C     
      include 'tspinc/params.inc'
      include 'tspinc/rmcom.inc'
      include 'tspinc/vrgov.inc'
      include 'tspinc/prate.inc'
      include 'tspinc/busvolt.inc'
      include 'tspinc/relays.inc'
      include 'tspinc/igentn.inc'
      include 'tspinc/param.inc'
      include 'tspinc/lnk12.inc'
      include 'tspinc/prt.inc'
      include 'tspinc/bname.inc'
      include 'tspinc/buskv.inc'

      dimension indt(2)
      character*8 name1c
      character*1 indc
      do i = 1, ngenf
C       
C       Bypass if relay is tripped
C       
        if (irmtcd(i) .ne. 2) then
          ibus = irmbno(i)
C         
C         Check if breaker timer activated to trip
C         
          if (rmbtim(i) .eq. 0.0) then
C           
C           Discontinuity test
C           
            if (idisw .eq. 2) then
C             
C             Store voltage and bypass
C             
              rmeyr(i) = eyr(ibus)
              rmeyi(i) = eyi(ibus)
              goto 110
            else
C             
C             Calculate bus freq and test relay
C             
              enew = eyr(ibus)
              fnew = eyi(ibus)
              efsq = enew**2 + fnew**2
              if (efsq .eq. 0.0) then
                wnew = rmfreq(i)
              else
                wnew = (fnew*rmeyr(i)-enew*rmeyi(i))/(efsq*edt)
C               
C               Process frequency and voltages
C               
                rmeyr(i) = enew
                rmeyi(i) = fnew
                rmfreq(i) = wnew
C               
C               Check for instantaneous trip
C               
                if (wnew .lt. rmfq2(i)) goto 100
              endif
C             
C             Check for relay trip with delay
C             
              if (wnew .ge. rmfq1(i)) then
C               
C               Reset relay timer and exit
C               
                rmrtim(i) = 0.0
                goto 110
              elseif (rmrtim(i) .eq. 0.0) then
C               
C               Set relay timer and exit

                rmrtim(i) = rmtrp1(i) + to
                goto 110
              elseif (rmrtim(i)-0.0001 .gt. to) then
                goto 110
              endif
C             
C             Check if breaker delay exists and set timer or trip
C             
  100         if (rmtrp2(i) .eq. 0.0) then
C               
C               No breaker time delay
C               
                dnxrly = to
              else
C               
C               Add breaker time delay and set next trip time--DNX
C               
                rmbtim(i) = rmtrp2(i) + to
                rtemp = rmbtim(i)
                if (rtemp .lt. dnxrly) dnxrly = rtemp
                goto 110
              endif
            endif
          elseif (rmbtim(i)-.0001 .gt. to) then
            goto 110
          endif
C         
C         Initiate to trip
C         
          irmtcd(i) = 2
          igind = irmgno(i)
          indc = igentc(igind)
          indt(1) = igentn(1, igind)
          indt(2) = igentn(2, igind)
          ivpc = 2
          if (ibus .lt. lfrst) lfrst = ibus
          name1c = bname(ibus)
          bkv1 = buskv(ibus)
          write (outbuf, 10000) name1c, bkv1, indc, to
          call prtout(1)
10000     format ('0', 5x, 'UNDERFREQ RELAY GEN DROP AT', 5x, a8, 1x,
     &     f5.1, 1x, a1, ' AT ', f6.1, ' CYCLES')
          gndrop = 0
          call gendrop(igind, gndrop, indt(1), indt(2), indc)
        endif
  110   continue
      enddo
      return
      end
