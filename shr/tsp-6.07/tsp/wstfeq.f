C    %W% %G%
      subroutine wstfeq(to)
C     
C     THIS SUBROUTINE CALCULATES THE BUS FREQUENCY AT EACH
C     BUS IN THE STUDY AND FORMS A TABLE OF THE 20 BUSES
C     HAVING THE LOWEST FREQUENCIES. IT IS CALLED BY
C     NOUT2.  TO IS THE STUDY TIME IN CYCLES.
C     
      include 'tspinc/params.inc'
      include 'tspinc/comn56.inc'
      include 'tspinc/link56.inc'
      include 'tspinc/worst.inc'
      include 'tspinc/newtab.inc'
      include 'tspinc/wfeq.inc'
      include 'tspinc/fltim.inc'
      include 'tspinc/room.inc'
      include 'tspinc/wstequ.inc'

      data feqcon, pastim /9.5493, 0.0 /
C     
C     IF THIS IS THE FIRST TIME WSTFEQ HAS BEEN CALLED, INITIALIZE
C     THE PAST ANGLE TABLE AND SET THE BUS FREQUENCY TO A HIGH NUM
C     
      if (wfeqmg(1) .eq. 10000.0) then
        do j = 1, nmx
          busfeq(j) = 10000.0
          e1 = eyr(j)
          f1 = eyi(j)
          if (.not. (e1 .eq. 0.0 .and. f1 .eq. 0.0)) pastan(j) = atan2
     &     (f1, e1)
          pastim = 0.0
        enddo
        wfeqmg(1) = 9999.
      else
C       
C       CALCULATE BUS FREQUENCY FOR EACH BUS IN THE STUDY
C       
        do ind = 1, nmx
          delt = to - pastim
          e1 = eyr(ind)
          f1 = eyi(ind)
C         
C         IF THIS IS A DISCONTINUITY, THE BUS FREQUENCY REMAINS TH
C         BUT THE PAST ANGLE IS UPDATED.
C         
          if (delt .eq. 0.0) then
            if (.not. (e1 .eq. 0.0 .and. f1 .eq. 0.0)) pastan(ind) =
     &       atan2(f1, e1)
C           
C           IF THE BUS VOLTAGE IS ZERO THE BUS FREQUENCY REMAINS T
C           AS DOES THE PAST ANGLE
C           
          elseif (.not. ((e1 .eq. 0.) .and. (f1 .eq. 0.))) then
            presa = atan2(f1, e1)
            delang = presa - pastan(ind)
            if (delang .lt. -3.14159) delang = delang + 6.28318
            if (delang .gt. 3.14159) delang = delang - 6.28318
            busfeq(ind) = (delang/delt)*feqcon
            pastan(ind) = presa
          endif
        enddo
        pastim = to
C       
C       DO NOT CALCULATE FREQUENCIES IF A FAULT IS APPLIED AT THIS
C       AND NO TIME WINDOW HAS BEEN SPECIFIED(WTIM2=-1).
C       
        if (ifltkt .ne. 0 .and. wtim2 .eq. -1) then
          do l = 1, ifltkt
            if (to .eq. fstrt(l) .and. to .eq. tlast) goto 140
            if (to .eq. fstop(l) .and. to .ne. tlast) goto 140
            if ((to .gt. fstrt(l)) .and. (to .lt. fstop(l))) goto 140
          enddo
        endif
C       
C       IF A TIME WINDOW HAS BEEN ENTERED, CALCULATE BUS FREQUENCI
C       DURING THE WINDOW
C       
        if (.not. (to .lt. wtim1 .or. to .gt. wtim2 .and. wtim2 .ne.
     &   -1)) then
C         
C         SORT THROUGH ALL BUS FREQUENCIES AND FOR THIS TIME STEP
C         AND SAVE THOSE THAT ARE AMONG THE LOWEST TWENTY IN THE S
C         
          do i = 1, nmx
            bfq = busfeq(i)
            ibkv = inwtb(i)
            do j = 1, 20
              if (newtbc(i) .eq. wfeqnm(j) .and. basekv(ibkv) .eq.
     &         wfeqkv(j)) goto 100
            enddo
            goto 110
  100       if (bfq .ge. wfeqmg(j)) goto 130
            if (j .ne. 20) then
              do m = j, 19
                wfeqmg(m) = wfeqmg(m+1)
                wfeqnm(m) = wfeqnm(m+1)
                wfeqkv(m) = wfeqkv(m+1)
                wfeqtm(m) = wfeqtm(m+1)
              enddo
            endif
  110       do j = 1, 20
              if (bfq .lt. wfeqmg(j)) goto 120
            enddo
            goto 130
  120       l = 21
            do k = j + 1, 20
              l = l - 1
              wfeqmg(l) = wfeqmg(l-1)
              wfeqtm(l) = wfeqtm(l-1)
              wfeqnm(l) = wfeqnm(l-1)
              wfeqkv(l) = wfeqkv(l-1)
            enddo
            wfeqmg(j) = bfq
            wfeqtm(j) = to
            wfeqnm(j) = newtbc(i)
            wfeqkv(j) = basekv(ibkv)
  130       continue
          enddo
        endif
      endif
  140 return
      end
