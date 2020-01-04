C    @(#)chk_pqdt.f	20.2 1/4/99
      integer function chk_pqdt (ptr, field, count, out_buffer)
      integer field, ptr, count
      character out_buffer(10)*120

C     This subroutine checks PQCURV(*,PTR) extensively for data errors.

      include 'ipfinc/parametr.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/pqcurves.inc'
      include 'ipfinc/prt.inc'

      integer error
      logical finished

      chk_pqdt = 0
      mini = 0
      maxi = 0
      do i = 1, 15
        kfield = 25 + 6 * i
        if (i .eq. 1) then
          if ((field .eq. 0 .or. field .eq. kfield .or. 
     &       field .eq. kfield+1) .and. (pqpgen(1,ptr) .ne. 0.0)) then
            chk_pqdt = 1
            count = min0 (count + 1, 10)
            write (out_buffer(count), 10010) pqpgen(1,ptr)
10010       format (' P1 (', f8.2, ') must be zero ')
          else
            mini = i
            maxi = i
          endif
        else if (pqpgen(i,ptr) .gt. 0.0) then
          mini = i
          maxi = i
          if ((field .eq. 0 .or. field .eq. kfield .or. 
     &         field .eq. kfield+1) .and.
     &        (pqpgen(i,ptr) .le. pqpgen(i-1,ptr))) then
            chk_pqdt = 1
            count = min0 (count + 1, 10)
            write (out_buffer(count), 10020) i-1, pqpgen(i-1,ptr),
     &        i, pqpgen(i-1,ptr)
10020       format (' Pgen sort error: P', i2, ' (', f8.2, ') > P', 
     &        i2, ' (', f8.2, ')')
          endif
          if ((field .eq. 0 .or. field .eq. kfield .or. 
     &         field .eq. kfield+1) .and.
     &        (pqqmax(i,ptr) .le. pqqmax(i-1,ptr))) then
            chk_pqdt = 1
            count = min0 (count + 1, 10)
            write (out_buffer(count), 10030) i-1, pqqmax(i-1,ptr),
     &        i, pqqmax(i-1,ptr)
10030       format (' Qmax sort error: QX', i2, ' (', f8.2, ') > QX', 
     &        i2, ' (', f8.2, ')')
          endif
          if ((field .eq. 0 .or. field .eq. kfield .or. 
     &         field .eq. kfield+1) .and.
     &        (pqqmin(i,ptr) .ge. pqqmin(i-1,ptr))) then
            chk_pqdt = 1
            count = min0 (count + 1, 10)
            write (out_buffer(count), 10040) i-1, pqqmin(i-1,ptr),
     &        i, pqqmin(i-1,ptr)
10040       format (' Qmin sort error: QN', i2, ' (', f8.2, ') < QN', 
     &        i2, ' (', f8.2, ')')
          endif
        endif
        if (i .eq. mini) then
          if ((field .eq. 0 .or. field .eq. kfield .or. 
     &         field .eq. kfield+1) .and.
     &        (pqqmin(i,ptr) .ge. pqqmax(i,ptr))) then
            chk_pqdt = 1
            count = min0 (count + 1, 10)
            write (out_buffer(count), 10050) i, pqqmax(i,ptr),
     &        i, pqqmin(i,ptr)
10050       format (' Qlimit error: QX', i2, ' (', f8.2, ') < QN', 
     &        i2, ' (', f8.2, ')')
          endif
        endif
      enddo
c
c     Notes:  
c  
c      1. pqpgen(-1,*), pqqmax(-1,*), and pqqmin(-1,*) all pertain to 
c         the constant MVA model
c      2. pqpgen(0,*) pertains to Pmax. The corresponding values in
c         pqqmax(0,*), and pqqmin(0,*) are meaningless.
c
      if (pqpgen(-1,ptr) .gt. 0.0) then
        kfield = 21
        if ((field .eq. 0 .or. field .eq. kfield .or. 
     &       field .eq. kfield+1) .and.
     &      (pqqmax(-1,ptr) .lt. 0.0 .or. pqqmax(-1,ptr) .gt. 1.0)) then
          chk_pqdt = 1
          count = min0 (count + 1, 10)
          write (out_buffer(count), 10060) pqqmax(-1,ptr)
10060     format (' Illegal Qmax PF: QX PF = (', f8.2, 
     &      ') < 0.0 or > 1.0 ')
          pqqmax(-1,ptr) = amax1 (amin1 (pqqmax(-1,ptr), 1.0), 0.0)
        endif
        if ((field .eq. 0 .or. field .eq. kfield .or. 
     &       field .eq. kfield+1) .and.
     &      (pqqmin(-1,ptr) .lt. 0.0 .or. pqqmin(-1,ptr) .gt. 1.0)) then
          chk_pqdt = 1
          count = min0 (count + 1, 10)
          write (out_buffer(count), 10070) pqqmin(-1,ptr)
10070     format (' Illegal Qmin PF: QX PF = (', f8.2, 
     &      ') < 0.0 or > 1.0 ')
          pqqmin(-1,ptr) = amax1 (amin1 (pqqmin(-1,ptr), 1.0), 0.0)
        endif
        pgenpmax = pqqmax(-1,ptr) * pqqmax(-1,ptr)
        qgenqmax = pqqmax(-1,ptr) * sqrt (1.0 - pqqmax(-1,ptr) ** 2)
        pgenpmin = -pqqmax(-1,ptr) * pqqmin(-1,ptr)
        qgenqmin = pqqmax(-1,ptr) * sqrt (1.0 - pqqmin(-1,ptr) ** 2)
c
c       Test piecewise continuous Qmax vs constant MVA
c
        lastmax = maxi
        finished = .false.
        do while (.not. finished)
          if (pqpgen(lastmax,ptr) .gt. pgenpmax .and. lastmax .gt. 1) 
     &      then
            lastmax = lastmax - 1
          else
            finished = .true.
          endif
        enddo
        do i = lastmax, maxi
          kfield = 25 + 6 * i
          if ((field .eq. 0 .or. field .eq. kfield .or. 
     &        field .eq. kfield+1) .and. (i .eq. lastmax)) then
            chk_xdtdta = 1
            count = min0 (count + 1, 10)
            write (out_buffer(count), 10080) pgenpmax, qgenqmax
10080       format (' Constant MVA (Qmax) point (', f8.2, ',', f8.2,
     &        ') supersedes the following linear points')
          endif
          if ((field .eq. 0 .or. field .eq. kfield .or. 
     &        field .eq. kfield+1)) then
            count = min0 (count + 1, 10)
            write (out_buffer(count), 10090) i, pqpgen(i,ptr), 
     &        pqqmax(i,ptr)
10090       format (' QX  point ', i2, ' (', f8.2, ',', f8.2, ')')
          endif
        enddo
c
c       Test piecewise continuous Qmin vs constant MVA
c
        lastmin = mini
        finished = .false.
        do while (.not. finished)
          if (pqpgen(lastmin,ptr) .gt. pgenpmin .and. lastmin .gt. 1) 
     &      then
            lastmin = lastmin - 1
          else
            finished = .true.
          endif
        enddo
        do i = lastmin, mini
          kfield = 25 + 6 * i
          if ((field .eq. 0 .or. field .eq. kfield .or. 
     &         field .eq. kfield+1) .and. (i .eq. lastmin)) then
            chk_xdtdta = 1
            count = min0 (count + 1, 10)
            write (out_buffer(count), 10100) pgenpmin, qgenqmin
10100       format (' Constant MVA (Qmin) point (', f8.2, ',', f8.2,
     &        ') supersedes the following linear points')
          endif
          if ((field .eq. 0 .or. field .eq. kfield .or. 
     &        field .eq. kfield+1)) then
            count = min0 (count + 1, 10)
            write (out_buffer(count), 10110) i, pqpgen(i,ptr), 
     &        pqqmin(i,ptr)
10110       format (' QN  point ', i2, ' (', f8.2, ',', f8.2, ')')
          endif
        enddo
        kfield = 25 + 6 * (lastmax-1)
        if ((field .eq. 0 .or. field .eq. kfield .or. 
     &       field .eq. kfield+1) .and.
     &      (pqqmax(lastmax-1,ptr) .lt. qgenqmax)) then
          chk_xdtdta = 1
          count = min0 (count + 1, 10)
          write (out_buffer(count), 10120) lastmax, 
     &      pqpgen(lastmax-1,ptr), pqqmax(lastmax-1,ptr), pgenpmax, 
     &      qgenqmax
10120     format (' Improper Qmax slope between linear point ', i2, 
     &      ' (', f8.2, ',', f8.2, ') and Constant MVA point (', 
     &      f8.2, ',', f8.2, ')')
        endif
        kfield = 25 + 6 * (lastmin-1)
        if ((field .eq. 0 .or. field .eq. kfield .or. 
     &       field .eq. kfield+1) .and.
     &      (pqqmin(lastmin-1,ptr) .gt. qgenqmin)) then
          chk_xdtdta = 1
          count = min0 (count + 1, 10)
          write (out_buffer(count), 10130) lastmin, 
     &      pqpgen(lastmin-1,ptr), pqqmin(lastmin-1,ptr), pgenpmin, 
     &      qgenqmin
10130     format (' Improper Qmin slope between linear point ', i2, 
     &      ' (', f8.2, ',', f8.2, ') and Constant MVA point (', 
     &      f8.2, ',', f8.2, ')')
        endif
      endif
      if (pqpgen(0,ptr) .gt. 0.0) then
        kfield = 26
c
c       Test PMAX > 0 and PMAX < MMVA if MMVA > 0
c
        if ((field .eq. 0 .or. field .eq. kfield .or. 
     &       field .eq. kfield+1) .and.
     &      (pqpgen(0,ptr) .lt. 0.0 .or. 
     &       pqpgen(0,ptr) .gt. 0.0 .and. pqpgen(-1,ptr) .gt. 0.0 .and.
     &       pqpgen(0,ptr) .lt. pqpgen(-1,ptr))) then
          chk_pqdt = 1
          count = min0 (count + 1, 10)
          write (out_buffer(count), 10140) pqpgen(-1,ptr), pqpgen(0,ptr)
10140     format (' Illegal PMAX (', f8.2, ') < 0 or > MMVA (', f8.2, 
     &')')
        endif
      endif

  220 return
      end
