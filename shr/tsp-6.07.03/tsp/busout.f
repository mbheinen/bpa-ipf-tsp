C    %W% %G%
      subroutine busout
 
C     THIS SUBROUTINE CALCULATES AND OUTPUTS BUS QUANTITIES
 
C     Revisions:
C     10/05/92 -  DEM
C     For alternate auxilary file output, widend time to 6 dec places
C     and paramaters to 15 characters.
C     -
      include 'tspinc/params.inc'
      include 'tspinc/blkcom1.inc'
      include 'tspinc/comn56.inc'
      include 'tspinc/link56.inc'
      include 'tspinc/dc.inc'
      include 'tspinc/worst.inc'
      include 'tspinc/out512.inc'
      include 'tspinc/newtab.inc'
      include 'tspinc/mgen.inc'
      include 'tspinc/lodbus.inc'
      include 'tspinc/jbusdt.inc'
      include 'tspinc/kbusno.inc'
      include 'tspinc/room.inc'
      include 'tspinc/geno.inc'
      include 'tspinc/mvn.inc'
      include 'tspinc/prt.inc'
      include 'tspinc/prtmax.inc'
      include 'tspinc/filter.inc'
      include 'tspinc/busanl.inc'
      include 'tspinc/fltim.inc'
 
      dimension msub1(2), msub2(2), msub4(2), msub7(2), msub10(2),
     &          msub12(2), msub13(2), msubnk(2)
      equivalence (msub(1), msub1), (msub(3), msub2), (msub(7), msub4),
     &            (msub(13), msub7), (msub(19), msub10), (msub(23),
     &            msub12), (msub(25), msub13)
      equivalence (mbust, mbus)
 
      common /nam/jdelet, jdbus(100)
      character*8 ewbus
      character*20 atem, atem2
 
      character crvend*12
 
C     In-line functions
 
      logical prntop, plotop, auxop, anlop
      prntop(i) = i .eq. 1 .or. i .eq. 3 .or. i .eq. 5 .or. i .eq. 7
      plotop(i) = i .eq. 2 .or. i .eq. 3 .or. i .eq. 6 .or. i .eq. 7
      auxop(i) = i .ge. 4
      anlop(i) = i .le. 8
 
C     begin     begin     begin     begin     begin     begin
 
      call mpost('BUSOUT')
 
      if (auxfmt .eq. 'STD') then
        crvend = '  -100.00'
      else
        crvend = '/END'
      endif
 
      iadr = icnt2 + 3
      ktbus1 = 0
      ktbus2 = 0
      maxu(7) = 1
      mwkt = 0
      mvarkt = 0
      i = 0
      ibusal = 0
      do while (.true.)
        i = i + 1
        if (i .gt. nmx) goto 130
        if (.not. (jbusdt(2, i) .eq. 0 .and. jbusdt(3, i) .eq. 0 .and.
     &   jbusdt(4, i) .eq. 0 .and. jbusdt(5, i) .eq. 0)) then
          ktbus = kbusno(i)
          if (jbusdt(2, i) .ne. 0 .or. jbusdt(3, i) .ne. 0) then
            call redecp(msub13(2), ksub13+ktbus-1, 1)
            call readmp (13, worksp, icnt2, 1)
          endif
          ewbus = newtbc(i)
          kbase = inwtb(i)
          ewbase = basekv(kbase)
C         
C         IF A SUMMARY OF MAX AND MINS FOR THIS BUS HAS BEEN REQUE
C         INCREMENT THE SUMMARY INDEX (IBUSAL) AND SAVE THE BUS NA
C         
          if (anlop(jbusdt(2, i)) .or. anlop(jbusdt(3, i))) then
            ibusal = ibusal + 1
            anbnam(ibusal) = ewbus
            anbkv(ibusal) = ewbase
            do jtrr = 1, 2
              anbmax(jtrr, ibusal) = 0.0
              anbtax(jtrr, ibusal) = 0.0
              anbmin(jtrr, ibusal) = 0.0
              anbtin(jtrr, ibusal) = 0.0
            enddo
          endif
          if (ibdatt .ne. 0) then
            if (prntop(jbusdt(2, i)) .or. prntop(jbusdt(3, i)) .or.
     &       prntop(jbusdt(4, i)) .or. prntop(jbusdt(5, i))) then
              write (outbuf, 10000) ewbus, ewbase
              call prtout(1)
10000         format ('0', 5x, 'OUTPUT LISTING FOR BUS ', a8, f8.1)
            endif
            imvsw3 = 0
            if (jbusdt(2, i) .ge. 10) then
              jbusdt(2, i) = jbusdt(2, i) - 10
              imvsw3 = 1
            endif
            imvsw5 = 0
            if (jbusdt(3, i) .ge. 10) then
              jbusdt(3, i) = jbusdt(3, i) - 10
              imvsw5 = 1
            endif
C           
C           INITIALIZE ARRAYS TO HOLD MAX AND MIN VALUES FOR OUTPU
C           
            do itrr = 1, 18
              prtmax(itrr) =  - 10000.
              prtmin(itrr) = 10000.
            enddo
            nqty = jbusdt(2, i)/4 + jbusdt(3, i)/4 + jbusdt(4, i)/4 +
     &             jbusdt(5, i)/4
            if (nqty .gt. 0 .and. auxfmt .eq. 'STD') then

C             IF (NQTY .GT. 0) THEN

              write (l11, 10010) ewbus, ewbase, nqty
10010         format ('B  ', a8, f5.1, i2)
            endif
          endif
          if (jbusdt(3, i) .ne. 0) then
            worksp(iadr) = 0.0
            kadr = iadr + icount
            worksp(kadr) = 0.0
            worksp(kadr+1) = 0.0
            dfreq = 0.0
            dfreqo = 0.0
            flfeq = 0.0
            flfeqo = 0.0
            do ind = 2, icount
              i1 = iadr + ind - 1
              delt = t(ind) - t(ind-1)
              j1 = 2*ind - 1
              j2 = j1 + 1
              e1 = worksp(j1)
              f1 = worksp(j2)
              e0 = worksp(j1-2)
              f0 = worksp(j2-2)
              if (.not. ((e1 .eq. 0.) .and. (f1 .eq. 0.))) then
                if (.not. ((e0 .eq. 0.) .and. (f0 .eq. 0.))) then
                  presa = atan2(f1, e1)
                  pasta = atan2(f0, e0)
                  if (delt .eq. 0.0) then
                    worksp(i1) = worksp(i1-1)
                    dfreq = dfreqo
                    flfeq = flfeqo
                    goto 100
                  else
                    delang = presa - pasta
                    if (delang .lt. -3.14159) delang = delang + 6.28318
                    if (delang .gt. 3.14159) delang = delang - 6.28318
                    dfreq = (delang/delt)
C                   
C                    IF TBUSF .NE. ZERO, FILTER BUS FREQUENCY USING
C                    TIME CONSTANT
C                   
                    if (tbusf .eq. 0.0) then
                      worksp(i1) = dfreq*hzconv
                    else
                      abus = 1.0 + 2.*tbusf/delt
                      abusr = 1./abus
                      flfeq = (dfreq+dfreqo+flfeqo*(abus-2.))*abusr
                      dfreqo = dfreq
                      flfeqo = flfeq
                      worksp(i1) = flfeq*hzconv
                    endif
                  endif
                endif
              endif
              if (worksp(i1) .gt. worksp(kadr)) worksp(kadr) = worksp
     &         (i1)
              if (worksp(i1) .lt. worksp(kadr+1)) worksp(kadr+1) =
     &         worksp(i1)
C             
C              DO SAVE MAX AND MINS IF A FAULT IS APPLIED AT THIS T
C             
              if (ifltkt .ne. 0) then
                to = t(ind)
                if (ind .gt. 1) then
                  tlast = t(ind-1)
                else
                  tlast =  - 1.
                endif
                do l = 1, ifltkt
                  if (to .eq. fstrt(l) .and. to .eq. tlast) goto 100
                  if (to .eq. fstop(l) .and. to .ne. tlast) goto 100
                  if ((to .gt. fstrt(l)) .and. (to .lt. fstop(l))) goto
     &             100
                enddo
              endif
              if (t(ind) .ge. wtim1 .and. t(ind) .le. wtim2 .or. wtim2
     &         .eq. -1.) then
                if (worksp(i1) .gt. prtmax(1)) then
                  prtmax(1) = worksp(i1)
                  timax = t(ind)
                endif
                if (worksp(i1) .lt. prtmin(1)) then
                  prtmin(1) = worksp(i1)
                  timin = t(ind)
                endif
              endif
  100         continue
            enddo
C           
C           IF THIS BUS IS IN THE BUS SUMMARY, THEN SAVE MAX, MIN
C           TIMES WHEN THEY OCCURRED
C           
            if (anlop(jbusdt(3, i))) then
              anbmax(1, ibusal) = prtmax(1)
              anbtax(1, ibusal) = timax
              anbmin(1, ibusal) = prtmin(1)
              anbtin(1, ibusal) = timin
            endif
            if (ibdatt .eq. 0) goto 120
            if (prntop(jbusdt(3, i))) then
              write (outbuf, 10020) prtmax(1), prtmin(1)
10020         format ('0', 5x, 'BUS FREQUENCY (HERTZ)', 5x, ' MAX = ',
     &         f8.4, ' MIN = ', f8.4)
              call prtout(1)
              do jjj = 1, icount, 5
                kkk = min0(jjj+4, icount)
                write (outbuf, 10030) (t(jj), worksp(jj+iadr-1), jj =
     &           jjj, kkk)
10030           format (5(f8.2, ' CYCLES ', f8.4))
                call prtout(1)
              enddo
            endif

C           -  Write out bus frequency

            if (auxop(jbusdt(3, i))) then
              if (auxfmt .eq. 'STD') then
                write (l11, '(a)') 'FREQUENCY'
C               WRITE (L11,3000)
C               3000        FORMAT ('FREQUENCY')
              else
                write (l11, '(A)') '/CURVE'
                write (l11, '(A,A8,F5.1)') '  LEGEND  ', ewbus, ewbase
                write (l11, '(A)') '  ! FREQUENCY'
                write (l11, '(A)') '  DATA'
              endif
              do jj = 1, icount
C               -  New format has wider fields
                if (auxfmt .eq. 'STD') then
C               CALL VARFMT(WORKSP(JJ+IADR-1),ATEM,10)
C               WRITE(L11,3200) T(JJ),ATEM
C               3200        FORMAT(F9.2,1X,A10)
                  call varfmt(worksp(jj+iadr-1), atem, 10)
                  write (l11, '(F9.2,1X,A10)') t(jj), atem
                else
                  call varfmt(worksp(jj+iadr-1), atem, 15)
                  write (l11, '(F13.6,1X,A15)') t(jj), atem
                endif
              enddo
              write (l11, '(A)') crvend
C             WRITE (L11,3600)
C             3600      FORMAT ('  -100.00')
            endif
            if (plotop(jbusdt(3, i))) then
              call writmp (13, worksp(iadr), icount+2, 1)
              ktbus1 = ktbus1 + 1
              call ritecp(msub13(2), ksb13c+ktbus1-1, 1)
            endif
C           
C            IF BUS FREQUENCY DATA IS NEEDED FOR A TWO OPTION OUTPU
C            THEN STORE IT IN VSPCE
C           
            if (imvsw5 .ne. 0) then
              do n = 1, 2*mvkt
                if (mvgen(n) .eq. i) then
                  if (mvcde(n) .eq. 'B') then
                    if (mvopt(n) .eq. 2) then
                      mvadr(n) = madr1
                      do l = 1, icoun2
                        vspce(madr1+l) = worksp(iadr+l)
                      enddo
                      madr1 = madr1 + icoun2
                    endif
                  endif
                endif
              enddo
            endif
          endif
C         
C          START OF BUS VOLTAGE OUTPUT
C         
          if (jbusdt(2, i) .ne. 0) then
            if (prntop(jbusdt(2, i)) .or. plotop(jbusdt(2, i)) .or.
     &       auxop(jbusdt(2, i)) .or. anlop(jbusdt(2, i))) then
              ymax = 0.0
              ymin = 0.0
              do jj = 1, icount
                j1 = 2*jj - 1
                j2 = j1 + 1
                e = worksp(j1)
                f = worksp(j2)
                if ((e+f) .ne. 0.0) then
                  worksp(j1) = sqrt(e*e+f*f)
                  worksp(j2) = atan2(f, e)*conv
                  if (worksp(j1) .gt. ymax) ymax = worksp(j1)
                  if (worksp(j1) .lt. ymin) ymin = worksp(j1)
C                 
C                  DO NOT SAVE MAX AND MINS IF A FAULT IS APPLIED A
C                 
                  if (ifltkt .ne. 0) then
                    to = t(jj)
                    if (jj .gt. 1) then
                      tlast = t(jj-1)
                    else
                      tlast =  - 1.
                    endif
                    do l = 1, ifltkt
                      if (to .eq. fstrt(l) .and. to .eq. tlast) goto
     &                 110
                      if (to .eq. fstop(l) .and. to .ne. tlast) goto
     &                 110
                      if ((to .gt. fstrt(l)) .and. (to .lt. fstop(l)))
     &                 goto 110
                    enddo
                  endif
                  if (t(jj) .ge. wtim1 .and. t(jj) .le. wtim2 .or.
     &             wtim2 .eq. -1.) then
                    if (worksp(j1) .gt. prtmax(2)) then
                      prtmax(2) = worksp(j1)
                      timax = t(jj)
                    endif
                    if (worksp(j1) .lt. prtmin(2)) then
                      prtmin(2) = worksp(j1)
                      timin = t(jj)
                    endif
                  endif
                endif
  110           continue
              enddo
            endif
C           
C            IF THIS BUS IS IN THE BUS SUMMARY, THEN SAVE MAX, MIN
C            TIMES WHEN THEY OCCURRED
C           
            if (anlop(jbusdt(3, i))) then
              anbmax(2, ibusal) = prtmax(2)
              anbtax(2, ibusal) = timax
              anbmin(2, ibusal) = prtmin(2)
              anbtin(2, ibusal) = timin
            endif
            if (prntop(jbusdt(2, i))) then
              write (outbuf, 10040) prtmax(2), prtmin(2)
10040         format ('0', 5x, 'BUS VOLTAGE --MAG. AND ANGLE (DEGREES)'
     &         , 5x, ' MAX MAGNITUDE = ', f6.4, ' MIN MAGNITUDE = ',
     &         f6.4)
              call prtout(1)
              do jjj = 1, icount, 4
                kkk = min0(jjj+3, icount)
                write (outbuf, 10050) (t(jj), worksp(jj*2-1), worksp(2
     &           *jj), jj = jjj, kkk)
                call prtout(1)
              enddo
10050         format (4(f8.2, ' CYCLES ', f6.4, f10.3))
            endif
 
            if (auxop(jbusdt(2, i))) then
              if (auxfmt .eq. 'STD') then
                write (l11, '(a)') 'VOLTAGE'
C               WRITE (L11,5200)
C               5200        FORMAT('VOLTAGE')
              else
                write (l11, '(A)') '/CURVE'
                write (l11, '(A,A8,F5.1)') '  LEGEND  ', ewbus, ewbase
                write (l11, '(A)') '  ! VOLTAGE'
                write (l11, '(A)') '  DATA'
              endif
              do jj = 1, icount
 
C               New format has wider fields
 
                if (auxfmt .eq. 'STD') then
C               CALL VARFMT(WORKSP(2*JJ-1),ATEM,10)
C               WRITE(L11,5400) T(JJ),ATEM,WORKSP(2*JJ)
C               5400        FORMAT(F9.2,1X,A10,F10.4)
                  call varfmt(worksp(2*jj-1), atem, 10)
                  call varfmt(worksp(2*jj), atem2, 10)
                  write (l11, '(F9.2,1X,A10,1A,A10)') t(jj), atem,
     &             atem2
                else
                  call varfmt(worksp(2*jj-1), atem, 15)
                  call varfmt(worksp(2*jj), atem2, 15)
                  write (l11, '(F13.6,1X,A15,1X,A15)') t(jj), atem,
     &             atem2
                endif
              enddo
              write (l11, '(A)') crvend
C             WRITE(L11,3600)
            endif
 
            if (plotop(jbusdt(2, i))) then
              ktbus2 = ktbus2 + 1
              do jj = 1, icount
                worksp(jj) = worksp(2*jj-1)
              enddo
              worksp(icount+1) = ymax
              worksp(icount+2) = ymin
              call writmp (13, worksp(1), icount+2, 1)
              call ritecp(msub13(2), ksub13+ktbus2-1, 1)
            endif
C           
C            IF VOLTAGE MAG DATA IS NEEDED FOR A TWO OPTION OUTPUT
C            THEN STORE IT IN VSPCE
C           
            if (imvsw3 .ne. 0) then
              do n = 1, 2*mvkt
                if (mvgen(n) .eq. i) then
                  if (mvcde(n) .eq. 'B') then
                    if (mvopt(n) .eq. 1) then
                      mvadr(n) = madr1
                      do l = 1, icoun2
                        vspce(madr1+l) = worksp(l)
                      enddo
                      madr1 = madr1 + icoun2
                    endif
                  endif
                endif
              enddo
            endif
          endif
C         
C          START OF REAL BUS LOAD OUTPUT
C          WRITE TO OUTPUT FILE IF REQUESTED
C         
          if (jbusdt(4, i) .ge. 1) then
            mwkt = mwkt + 1
            if (mwkt .le. itotmw) then
              ymax = 0.0
              ymin = 0.0
              do ktrr = 1, icount
                if (busmw(mwkt, ktrr) .gt. ymax) ymax = busmw(mwkt,
     &           ktrr)
                if (busmw(mwkt, ktrr) .lt. ymin) ymin = busmw(mwkt,
     &           ktrr)
              enddo
              busmw(mwkt, icount+1) = ymax
              busmw(mwkt, icount+2) = ymin
              if (prntop(jbusdt(4, i))) then
                do itrr = 1, icount
                  if (t(itrr) .ge. wtim1 .and. t(itrr) .le. wtim2 .or.
     &             wtim2 .eq. -1.) then
                    if (busmw(mwkt, itrr) .lt. prtmin(3)) prtmin(3) =
     &               busmw(mwkt, itrr)
                    if (busmw(mwkt, itrr) .gt. prtmax(3)) prtmax(3) =
     &               busmw(mwkt, itrr)
                  endif
                enddo
                write (outbuf, 10060) prtmax(3), prtmin(3)
10060           format (6x, ' BUS LOAD IN MWS ', 5x, ' MAXIMUM = ',
     &           f8.2, ' MINIMUM = ', f8.2)
                call prtout(1)
                do jjj = 1, icount, 5
                  kkk = min0(jjj+4, icount)
                  write (outbuf, 10070) (t(jj), busmw(mwkt, jj), jj =
     &             jjj, kkk)
                  call prtout(1)
10070             format (5(f8.2, ' CYCLES ', f8.2))
                enddo
              endif
C             
C              WRITE BUS MW'S TO AUXILIARY FILE IF REQUESTED
C             
              if (auxop(jbusdt(4, i))) then
                if (auxfmt .eq. 'STD') then
                  write (l11, 10080)
10080             format ('LOAD-MWS')
                else
                  write (l11, '(A)') '/CURVE'
                  write (l11, '(A,A8,F5.1)') '  LEGEND  ', ewbus,
     &             ewbase
                  write (l11, '(A)') '  ! LOAD (MWs)'
                  write (l11, '(A)') '  DATA'
                endif
                do jj = 1, icount
C                 -  New format has wider fields
                  if (auxfmt .eq. 'STD') then
C                 CALL VARFMT(BUSMW(MWKT,JJ),ATEM,10)
C                 WRITE(L11,7800)T(JJ),ATEM
C                 7800        FORMAT(F9.2,1X,A10)
                    call varfmt(busmw(mwkt, jj), atem, 10)
                    write (l11, '(F9.2,1X,A10)') t(jj), atem
                  else
                    call varfmt(busmw(mwkt, jj), atem, 15)
                    write (l11, '(F13.6,1X,A15)') t(jj), atem
                  endif
                enddo
                write (l11, '(A)') crvend
C               WRITE (L11,3600)
              endif
            endif
          endif
C         
C          START OF REACTIVE BUS LOAD OUTPUT
C          WRITE TO OUTPUT FILE IF REQUESTED
C         
          if (jbusdt(5, i) .ge. 1) then
            mvarkt = mvarkt + 1
            if (mvarkt .le. itotvr) then
              ymax = 0.0
              ymin = 0.0
              do ktrr = 1, icount
                if (busvar(mvarkt, ktrr) .gt. ymax) ymax = busvar
     &           (mvarkt, ktrr)
                if (busvar(mvarkt, ktrr) .lt. ymin) ymin = busvar
     &           (mvarkt, ktrr)
              enddo
              busvar(mvarkt, icount+1) = ymax
              busvar(mvarkt, icount+2) = ymin
              if (prntop(jbusdt(5, i))) then
                do itrr = 1, icount
                  if (t(itrr) .ge. wtim1 .and. t(itrr) .le. wtim2 .or.
     &             wtim2 .eq. -1.) then
                    if (busvar(mvarkt, itrr) .lt. prtmin(4)) prtmin(4)
     &               = busvar(mvarkt, itrr)
                    if (busvar(mvarkt, itrr) .gt. prtmax(4)) prtmax(4)
     &               = busvar(mvarkt, itrr)
                  endif
                enddo
                write (outbuf, 10090) prtmax(4), prtmin(4)
10090           format (6x, ' BUS LOAD IN MVARS ', 5x, ' MAXIMUM = ',
     &           f8.2, ' MINIMUM = ', f8.2)
                call prtout(1)
                do jjj = 1, icount, 5
                  kkk = min0(jjj+4, icount)
                  write (outbuf, 10100) (t(jj), busvar(mvarkt, jj), jj
     &             = jjj, kkk)
                  call prtout(1)
10100             format (5(f8.2, ' CYCLES ', f8.2))
                enddo
              endif
C             
C              WRITE BUS MVAR'S TO AUXILIARY FILE IF REQUESTED
C             
              if (auxop(jbusdt(5, i))) then
                if (auxfmt .eq. 'STD') then
                  write (l11, 10110)
10110             format ('LOAD-MVARS')
                else
                  write (l11, '(A)') '/CURVE'
                  write (l11, '(A,A8,F5.1,I2)') '  LEGEND  ', ewbus,
     &             ewbase, nqty
                  write (l11, '(A)') '  ! LOAD (MVARs)'
                  write (l11, '(A)') '  DATA'
                endif
                do jj = 1, icount
C                 -  New format has wider fields
                  if (auxfmt .eq. 'STD') then
C                 CALL VARFMT(BUSVAR(MVARKT,JJ),ATEM,10)
C                 WRITE(L11,9400)T(JJ),ATEM
C                 9400        FORMAT(F9.2,1X,A10)
                    call varfmt(busvar(mvarkt, jj), atem, 10)
                    write (l11, '(F9.2,1X,A10)') t(jj), atem
                  else
                    call varfmt(busvar(mvarkt, jj), atem, 15)
                    write (l11, '(F13.6,1X,A15)') t(jj), atem
                  endif
                enddo
                write (l11, '(A)') crvend
C               WRITE (L11,3600)
              endif
            endif
          endif
        endif
  120   continue
      enddo
  130 kbusid = kwork
      kwork = kwork + nmx + 1
      ktbus = ktbus2
C     
C      CALL BUSSUM TO PRINT A SUMMARY OF MAXIMUMS AND MINIMUMS
C     
      if (ibusal .ne. 0) call bussum()
      return
      end
