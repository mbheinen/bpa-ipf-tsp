C    %W% %G%
C****************************************************************
C
C     File: ld_geltc.f
C
C     Purpose: Routine to load GE LTC data from raw data file 
C
c     Return code:  n = 0 : Success
c                   n = 1 : Error
c
C     Author: Walt Powell  Date: 21 May 1996
C     Called by: load_ge.f
C
C****************************************************************
      integer function ld_geltc (xbuf, file, options, error)
      integer file, error, options(*)
      character *(*) xbuf

      include 'ipfinc/parametr.inc'

      include 'ipfinc/prt.inc'
      include 'ipfinc/pti_data.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/alt_case.inc'
      include 'ipfinc/alpha.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/branch.inc'
      include 'ipfinc/tx_misc.inc'

      common /old_branch/ old_k1, old_k2, old_sect, old_type, old_id
      integer ptr, old_k1, old_k2, old_sect, old_type
      character old_id * 1

      integer MAXWYEDELTA
      parameter (MAXWYEDELTA = 500)
      common /wye_delta/ num_wye_delta, lwye_delta(3,MAXWYEDELTA),
     &                   wye_delta(MAXWYEDELTA)

      character word(80)*10, id*1, getid*1
      logical phase_shift
      integer find_bus, fnd_ptin, ftn_atoi, status, bptr1, bptr2, 
     &        txterm(4,3), read_ge_file
      real zxterm(10,3), rate_ge(8)
      complex a(4), b(4)

      ld_geltc = 0
      error = 0

      last = lastch0 (xbuf)
      do while (xbuf(last:last) .eq. '/') 
        status = read_ge_file (file, xbuf(last:))
        if (status .eq. 0) go to 340
        last = lastch0 (xbuf)
      enddo
      call uscan (xbuf(1:last), word, nwrd, '~',  ' ')   

      do i = nwrd+1, 75
        word(i) = ' '
      enddo
c
c     Add defaults
c
      if (word(24) .eq. ' ') word(24) = '100.0'

      npti1 = ftn_atoi(word(1))              ! from bus
      npti2 = ftn_atoi(word(4))              ! to bus
      npti3 = ftn_atoi(word(12))             ! reg bus
      npti4 = ftn_atoi(word(16))             ! 3-winding pt bus
      npti5 = ftn_atoi(word(19))             ! tertiary bus
      id = getid (word(7))

      if (word(10) .eq. ' ') then
        status = 1
      else
        status = ftn_atoi(word(10))
      endif

      if (npti1 .eq. 0) then
        if (ichar (xbuf(1:1)) .ge. ichar ('a') .and.
     &      ichar (xbuf(1:1)) .le. ichar ('z')) ld_geltc = 1
      else if (npti2 .eq. 0 .or. status .eq. 0) then
      else
        kp1 = fnd_ptin (npti1)
        if (kp1 .le. 0) then
          write (errbuf(1), 10010) npti1, npti2
10010     format (' GE bus1 on Tx record (', i6, 1x, i6, 
     &      ') is not a valid number.')
          call prterx ('W',1)                
          error = 1                           
          go to 900
        endif
        kp2 = fnd_ptin (npti2)
        if (kp2 .le. 0) then
          write (errbuf(1), 10020) npti1, npti2
10020     format (' GE bus2 on Tx record (', i6, 1x, i6, 
     &      ') is not a valid number.')
          call prterx ('W',1)                
          error = 1                           
          go to 900
        endif
        k1 = find_bus (pti_name(kp1), pti_base(kp1))
        k2 = find_bus (pti_name(kp2), pti_base(kp2))
        if (k1 .le. 0) then
          write (errbuf(1), 10030) npti1, pti_name(kp1), 
     &       pti_base(kp1), npti2, pti_name(kp2), pti_base(kp2)
10030     format (' GE bus1 on Tx record (', i6, 1x, a8, f7.1, 
     &            ') - (', i6, 1x, a8, f7.1, ') is not in system.')
          call prterx ('W',1)                
          error = 1                           
          go to 900
        else if (k2 .le. 0) then
          write (errbuf(1), 10040) npti1, pti_name(kp1), 
     &       pti_base(kp1), npti2, pti_name(kp2), pti_base(kp2)
10040     format (' GE bus2 on Tx record (', i6, 1x, a8, f7.1, 
     &            ') - (', i6, 1x, a8, f7.1, ') is not in system.')
          call prterx ('W',1)                
          error = 1                           
          go to 900
        endif
        k3 = 0
        if (npti4 .le. 0) then
          kp4 = 0
          k4 = 0
        else
          kp4 = fnd_ptin (npti4)
          if (kp4 .le. 0) then
            write (errbuf(1), 10050) npti1, npti2, npti4
10050       format (' GE 3-winding pt bus on Tx record (', i5,
     &         1x, i5, ') is not a valid number (', i5, ').')
            call prterx ('W',1)                
            error = 1                           
            go to 900
          endif
          k4 = find_bus (pti_name(kp4), pti_base(kp4))
          if (k4 .le. 0) then
            write (errbuf(1), 10060) npti1, pti_name(kp1), 
     &         pti_base(kp1), npti2, pti_name(kp2), pti_base(kp2)
10060       format (' GE 3-winding pt on Tx record (', i6, 1x,
     &         a8, f7.1, ') - (', i6, 1x, a8, f7.1, ')')
            write (errbuf(1), 10070) npti4, pti_name(kp4), 
     &        pti_base(kp4)
10070       format ('                           (', i6, 1x, a8, f7.1, 
     &            ') is not in system.')
            call prterx ('W',2)                
            error = 1                           
            go to 900
          endif
        endif
        if (npti5 .le. 0) then
          kp5 = 0
          k5 = 0
        else
          kp5 = fnd_ptin (npti5)
          if (kp5 .lt. 0) then
            write (errbuf(1), 10080) npti1, npti2, npti5
10080       format (' GE tertiary bus on Tx record (', i5, 1x,
     &         i5, ') is not a valid number (', i5, ').')
            call prterx ('W',1)                
            error = 1                           
            go to 900
          endif
          k5 = find_bus (pti_name(kp5), pti_base(kp5))
          if (k5 .lt. 0) then
            write (errbuf(1), 10090) npti1, pti_name(kp1), 
     &         pti_base(kp1), npti2, pti_name(kp2), pti_base(kp2),
     &         npti5, pti_name(kp5), pti_base(kp5)
10090       format (' GE tertiary bus on Tx record (', i6, 1x,
     &         a8, f7.1, ') - (', i6, 1x, a8, f7.1, ')')
            write (errbuf(1), 10100) npti5, pti_name(kp5), 
     &        pti_base(kp5)
10100       format ('                           (', i6, 1x, a8, f7.1, 
     &            ') is not in system.')
            call prterx ('W',2)                
            error = 1                           
            go to 900
          endif
        endif

        ltc_type = ftn_atoi(word(11))
        angle = ftn_atof (word(34)) 

        phase_shift = ((ltc_type .eq. 4 .or. ltc_type .eq. 14) .or.
     &                 (angle .ne. 0.0 .and. abs(angle) .ne. 30.0 
     &                                 .and. base(k1) .eq. base(k2)))

        if ((ltc_type .eq. 1 .or. ltc_type .eq. 11 .or.
     &       ltc_type .eq. 2 .or. ltc_type .eq. 12 .or.
     &       ltc_type .eq. 3 .or. ltc_type .eq. 13) .and.
     &      (abs(angle) .eq. 30.0)) then
          num_wye_delta = num_wye_delta + 1
          lwye_delta(1,num_wye_delta) = k1
          lwye_delta(2,num_wye_delta) = k2
          lwye_delta(3,num_wye_delta) = ichar (id)
          wye_delta(num_wye_delta) = angle
        endif

        txbase = bmva / ftn_atof (word(24))

        if (k4 .eq. 0) then
          txterm(1,1) = k1
          txterm(2,1) = k2
          txterm(3,1) = npti1
          txterm(4,1) = npti2
          zxterm(1,1) = ftn_atof(word(25)) * txbase      ! R
          zxterm(2,1) = ftn_atof(word(26)) * txbase      ! X
          zxterm(3,1) = ftn_atof(word(35)) / txbase      ! G
          zxterm(4,1) = ftn_atof(word(36)) / txbase      ! B
          txbase1     = ftn_atof(word(31))               ! Tx base1
          txbase2     = ftn_atof(word(32))               ! Tx base2
          tapfp       = ftn_atof(word(48))
          tap1        = ftn_atof(word(47)) + tapfp - 1.0 
          zxterm(5,1) = tap1 * txbase1 / base(k1)        ! Tap1
          tap2        = ftn_atof(word(49))
          zxterm(6,1) = tap2 * txbase2 / base(k2)        ! Tap2
          zxterm(7,1) = txbase1                          ! Tx base1
          zxterm(8,1) = txbase2                          ! Tx base2
          numtx = 1
          if (abs (zxterm(5,1) - 1.0) .gt. 0.15) then
            write (errbuf(1), 10102) bus(k1), base(k1), bus(k2), 
     &        base(k2), id, zxterm(5,1)
10102       format (' Tap1 on Tx record (', a8, f7.1, 1x, a8, f7.1,
     &        1x, a1, ') is more than 15% of Tx base1 (', f7.4, ')')
            call prterx ('W',1)                
            error = error + 1
          endif
          if (abs (zxterm(6,1) - 1.0) .gt. 0.15) then
            write (errbuf(1), 10104) bus(k1), base(k1), bus(k2), 
     &        base(k2), id, zxterm(6,1)
10104       format (' Tap2 on Tx record (', a8, f7.1, 1x, a8, f7.1, 
     &        1x, a1, ') is more than 15% of Tx base2 (', f7.4, ')')
            call prterx ('W',1)                
            error = error + 1
          endif
        else
c
c         Define delta-impedances
c
          r = ftn_atof(word(25)) * txbase  ! primary-secondary
          x = ftn_atof(word(26)) * txbase
          b(1) = cmplx (r, x)
          r = ftn_atof(word(27)) * txbase  ! primary-tertiary
          x = ftn_atof(word(28)) * txbase
          b(2) = cmplx (r, x)
          r = ftn_atof(word(29)) * txbase  ! tertiary-secondary
          x = ftn_atof(word(30)) * txbase
          b(3) = cmplx (r, x)
c
c         Compute wye-impdances
c
          a(1) = cmplx (0.5, 0.0) * (b(1) + b(2) - b(3)) ! Zp
          a(2) = cmplx (0.5, 0.0) * (b(1) + b(3) - b(2)) ! Zs
          a(3) = cmplx (0.5, 0.0) * (b(2) + b(3) - b(1)) ! Zs

          txterm(1,1) = k1
          txterm(2,1) = k4
          txterm(3,1) = npti1
          txterm(4,1) = npti4
          zxterm(1,1) = real (a(1))                      ! R
          zxterm(2,1) = aimag (a(1))                     ! X
          zxterm(3,1) = 0.333 * ftn_atof(word(35)) / txbase  ! G
          zxterm(4,1) = 0.333 * ftn_atof(word(36)) / txbase  ! B
          txbase1     = ftn_atof(word(31))               ! Tx base1
          tapfp       = ftn_atof(word(48))
          tap1        = ftn_atof(word(47)) + tapfp - 1.0 
          zxterm(5,1) = tap1 * txbase1 / base(k1)        ! Tap1
          zxterm(6,1) = 1.0                              ! Tap2
          zxterm(7,1) = txbase1                          ! Tx base1
          zxterm(8,1) = base(k4)                         ! Tx base2
          if (abs (zxterm(5,1) - 1.0) .gt. 0.15) then
            write (errbuf(1), 10102) bus(k1), base(k1), bus(k4), 
     &        base(k4), id, zxterm(5,1)
            call prterx ('W',1)                
            error = error + 1
          endif
          txterm(1,2) = k2
          txterm(2,2) = k4
          txterm(3,2) = npti2
          txterm(4,2) = npti4
          zxterm(1,2) = real (a(2))                     ! R
          zxterm(2,2) = aimag (a(2))                    ! X
          zxterm(3,2) = 0.333 * ftn_atof(word(35)) / txbase  ! G
          zxterm(4,2) = 0.333 * ftn_atof(word(36)) / txbase  ! B
          txbase1     = ftn_atof(word(32))              ! Tx base1
          tap1        = ftn_atof(word(49))
          zxterm(5,2) = tap1 * txbase1 / base(k2)       ! Tap1
          zxterm(6,2) = 1.0                             ! Tap2
          zxterm(7,2) = txbase1                         ! Tx base1
          zxterm(8,2) = base(k4)                        ! Tx base2
          if (abs (zxterm(5,2) - 1.0) .gt. 0.15) then
            write (errbuf(1), 10104) bus(k2), base(k2), bus(k4), 
     &        base(k4), id, zxterm(5,2)
            call prterx ('W',1)                
            error = error + 1
          endif
          txterm(1,3) = k5
          txterm(2,3) = k4
          txterm(3,3) = npti5
          txterm(4,3) = npti4
          zxterm(1,3) = real (a(3))                     ! R
          zxterm(2,3) = aimag (a(3))                    ! X
          zxterm(3,3) = 0.333 * ftn_atof(word(35)) / txbase  ! G
          zxterm(4,3) = 0.333 * ftn_atof(word(36)) / txbase  ! B
          txbase1     = ftn_atof(word(33))              ! Tx base1
          tap1        = ftn_atof(word(50))
          zxterm(5,3) = tap1 * txbase1 / base(k5)       ! Tap1
          zxterm(6,3) = 1.0                             ! Tap2
          zxterm(7,3) = txbase1                         ! Tx base1
          zxterm(8,3) = base(k4)                        ! Tx base2
          if (abs (zxterm(5,3) - 1.0) .gt. 0.15) then
            write (errbuf(1), 10106) bus(k1), base(k1), bus(k2), 
     &        base(k2), bus(k5), base(k5), id, zxterm(5,3)
10106       format (' Base3 on Tx record (', a8, f7.1, 1x, a8, f7.1, 
     &        1x, a8, f7.1, 1x, a1, ') is more than 15% of Tx base3 (',
     &        f7.4, ')')
            call prterx ('W',1)                
            error = error + 1
          endif
          if (num_3term .ge. MAX_3TERM) then
            write (errbuf(1), 10108) MAX_3TERM, bus(k1), base(k1), 
     &        bus(k2), base(k2), bus(k5), base(k5), id
10108       format (' More than ', i4, ' 3-terminal Tx''s. Ignoring (', 
     &        a8, f7.1, 1x, a8, f7.1, 1x, a8, f7.1, 1x, a1, ')')
            call prterx ('W',1)                
            error = error + 1
          else
            num_3term = num_3term + 1
            tx_3term(1,num_3term) = k1          ! from bus
            tx_3term(2,num_3term) = k2          ! to bus
            tx_3term(3,num_3term) = k3          ! reg bus
            tx_3term(4,num_3term) = k4          ! 3-winding pt bus
            tx_3term(5,num_3term) = k5          ! tertiary bus
            tx_3term(6,num_3term) = ichar(id)   ! ID
          endif
          numtx = 3

        endif
                
        do ix = 1, numtx
          k1x = txterm(1,ix)
          k2x = txterm(2,ix)
          npti1x = txterm(3,ix)
          npti2x = txterm(4,ix)
          ptr = kbsdta(16,k1x)
c
c         Add T or TP record
c
          if (ltot+1 .ge. MAXBRN) then
            write (errbuf(1), 10110) MAXBRN
10110       format ('More than ',i5,
     &              ' branch records. Overflow occurred at branch:') 
            write (errbuf(2), 10120) xbuf(1:80)   
10120       format(11x, '(', a, ')')         
            call prterx ('E', 2)                
            ltot = 1                           
            error = 1                           
            go to 900
          endif
          ltot = ltot + 1
          do i = 1, 18
            kbrnch(i,ltot) = 0
          enddo
          do i = 1, 3
            rateln(i,ltot) = 0.0
          enddo
          if (phase_shift) then
            kbrnch(1,ltot) = 6
          else
            kbrnch(1,ltot) = 5
          endif

c         link up branch

          call lkbrdata (ltot, k1x, k2x, bptr1, bptr2, error)

          kx(bptr1) = k1x   
          ky(bptr1) = k2x
          brid(bptr1) = id
          brsect(bptr1) = 0
          brtype(bptr1) = kbrnch(1,ltot)

          kx(bptr2) = k2x   
          ky(bptr2) = k1x
          brid(bptr2) = id
          brsect(bptr2) = 0
          brtype(bptr2) = kbrnch(1,ltot)

          ptr = bptr1

          nbr = iabs (brnch_ptr(ptr))
          call putchr(3, word(59)(1:3), kbrnch(3,ltot))
          brnch(5,nbr) = zxterm(1,ix)         ! R
          brnch(6,nbr) = zxterm(2,ix)         ! X
          brnch(7,nbr) = zxterm(3,ix)         ! G
          brnch(8,nbr) = zxterm(4,ix)         ! B
          if (phase_shift) then
            if (brnch_ptr(ptr) .gt. 0) then
              brnch(9,nbr) = angle
              brnch(10,nbr) = 0.0
            else
              brnch(9,nbr) = -angle
              brnch(10,nbr) = 0.0
            endif
          else
            tx1 = zxterm(5,ix) * base(k1x)    ! Tap1
            tx2 = zxterm(6,ix) * base(k2x)    ! Tap2
            if (brnch_ptr(ptr) .gt. 0) then
              brnch(9,nbr) = tx1
              brnch(10,nbr) = tx2
            else
              brnch(9,nbr) = tx2
              brnch(10,nbr) = tx1
            endif
          endif
          brnch(16,nbr) = ftn_atoi (word(15)) ! Zdata
          brnch(18,nbr) = bmva / txbase
          aloss = ftn_atof(word(41))          ! Metering point as %
          if ((aloss .lt. 1.0 .and. brnch_ptr(ptr) .gt. 0) .or.
     &      (aloss .eq. 1.0 .and. brnch_ptr(ptr) .lt. 0)) then
            kbrnch(15,nbr) = 1
          else
            kbrnch(15,nbr) = 2
          endif
C                                                  
C         Add extended ratings.                 
C                                                  
          rate_ge(1) = ftn_atof(word(37))    ! Rate A
          rate_ge(2) = ftn_atof(word(38))    ! Rate B
          rate_ge(3) = ftn_atof(word(39))    ! Rate C
          rate_ge(4) = ftn_atof(word(40))    ! Rate D
          rate_ge(5) = ftn_atof(word(55))    ! Rate E
          rate_ge(6) = ftn_atof(word(56))    ! Rate F
          rate_ge(7) = ftn_atof(word(57))    ! Rate G
          rate_ge(8) = ftn_atof(word(58))    ! Rate H
          do i = 1, 8
            rate_ge(i) = amin1 (rate_ge(i), 9999.0)
          enddo
          do i = 5, 8
            if (rate_ge(i) .eq. 0.0) rate_ge(i) = rate_ge(i-4)
          enddo

          i = options(1)
          if (i .eq. 0) i = 1
          brnch(4,ltot) = rate_ge(i)

          i = options(2)
          if (i .eq. 0) i = 1
          rateln(1,ltot) = rate_ge(i)

          i = options(3)
          if (i .eq. 0) i = 2
          rateln(2,ltot) = rate_ge(i)

          i = options(4)
          if (i .eq. 0) i = 1
          rateln(3,ltot) = rate_ge(i)

        enddo

        if (ltc_type .eq. 1 .or. ltc_type .eq. 11 .or. 
     &      ix .gt. 2 .or. npti3 .le. 0) go to 900

        if (ltot+1 .ge. MAXBRN) then
          write (errbuf(1), 10110) MAXBRN
          write (errbuf(2), 10120) xbuf(1:80)   
          call prterx ('E', 2)                
          ltot = 1                           
          error = 1                           
          go to 900
        endif

        k1x = txterm(1,1) 
        k2x = txterm(2,1) 
        npti1x = txterm(3,1)
        npti2x = txterm(4,1)
        ltot = ltot + 1
        do i = 1, 18
          brnch(i,ltot) = 0
        enddo
        kbrnch(1,ltot) = 4
        call putchr(3, ' ', kbrnch(3,ltot))
        tmax = ftn_atof(word(42))
        tmin = ftn_atof(word(43))
        taps = abs(ftn_atof(word(46)))
        if (taps .gt. 0.0) then
          numptaps = (tmax + tapfp - 2.0) / taps + 0.5
          numptaps = max0 (numptaps, 0)
          numntaps = (2.0 - tmin - tapfp) / taps + 0.5
          numntaps = max0 (numntaps, 0)
          tmaxnew = 1.0 + taps * float(numptaps)
          tminnew = 1.0 - taps * float(numntaps)
          numtaps = numptaps + numntaps + 1
        else
          tmaxnew = tmax
          tminnew = tmin
          numtaps = 0
        endif
        if (phase_shift) then
          brnch(6,ltot) = tmaxnew               ! Tmax (kV)
          brnch(7,ltot) = tminnew               ! Tmin (kV)
          if (taps .gt. 0.0) then
            if (numtaps .gt. 50) numtaps = 0
            brnch(8,ltot) = numtaps
          else
            brnch(8,ltot) = 0
          endif
        else
          brnch(6,ltot) = tmaxnew * base(k1x)   ! Tmax (kV)
          brnch(7,ltot) = tminnew * base(k1x)   ! Tmin (kV)
          if (taps .gt. 0.0) then
            if (numtaps .gt. 50) numtaps = 0
            brnch(8,ltot) = numtaps
          else
            brnch(8,ltot) = 0.0
          endif
        endif
        if (npti3 .eq. -1) then
          npti3 = npti1x
          kp3 = kp1
          k3 = k1x
        else if (npti3 .eq. -2) then
          npti3 = npti2x
          kp3 = kp2
          k3 = k2x
        else if (npti3 .gt. 0) then
          kp3 = fnd_ptin (npti3)
          if (kp3 .le. 0) then
            write (errbuf(1), 10140) npti3, npti1x, bus(k1x), 
     &         base(k1x), npti2x, bus(k2x), base(k2x)
10140       format (' LTC controlled bus (', i6, 
     &         ') on branch record (', i6, 1x, a8, f7.1, ' - ', 
     &         1x, i6, 1x, a8, f7.1, ') is not in system.')
            call prterx ('W',1)                
            error = 1                           
            go to 900
          endif
          k3 = find_bus (pti_name(kp3), pti_base(kp3))
          if (k3 .le. 0) then
            write (errbuf(1), 10150) npti3, pti_name(kp3), 
     &         pti_base(kp3), npti1x, bus(k1x), base(k1x), 
     &         npti2x, bus(k2x), base(k2x)
10150       format (' LTC controlled bus (', i6, 1x, a8, f7.1, 
     &         ') on branch record (', i6, 1x, a8, f7.1, ' - ', i6,
     &         1x, a8, f7.1, ') is not in system.')
            call prterx ('W',1)                
            error = 1                           
            go to 900
          endif
          if (k3 .eq. k1x) then
            kbrnch(4,ltot) = k1x
          else if (k3 .eq. k2x) then
            kbrnch(4,ltot) = k2x
          else if (k3 .eq. -1) then
            kbrnch(4,ltot) = -1
          else
            kbrnch(4,ltot) = -2
          endif
        endif
        if (ltc_type .eq. 2 .or. ltc_type .eq. 12) then
          vmax = ftn_atof(word(44))          ! Vmax (p.u.)
          vmin = ftn_atof(word(45))          ! Vmin (p.u.)
          if (options(8) .eq. 1) then
            if (kbsdta(1,k3) .eq. 1) kbsdta(1,k3) = 10
            busdta(11,k3) = 0.5 * (vmax + vmin)
            busdta(12,k3) = busdta(11,k3)
          else if (options(8) .eq. 2) then
            if (kbsdta(1,k3) .eq. 1) kbsdta(1,k3) = 10
            busdta(11,k3) = vmax
            busdta(12,k3) = busdta(11,k3)
          else if (options(8) .eq. 3) then
            if (kbsdta(1,k3) .eq. 1) then
              busdta(11,k3) = vmax
              busdta(12,k3) = vmin
            else if (kbsdta(1,k3) .eq. 7) then
              busdta(11,k3) = 0.5 * (vmax + vmin)
            else
              busdta(11,k3) = vmax
              busdta(12,k3) = vmin
            endif
          else if (kbsdta(1,k3) .eq. 1) then
            busdta(11,k3) = vmax
            busdta(12,k3) = vmin
          else 
            busdta(11,k3) = 0.5 * (vmax + vmin)
            busdta(12,k3) = busdta(11,k3)
          endif
          if (kbsdta(1,k3) .eq. 10) busdta(12,k3) = 0.0
        else if (ltc_type .eq. 3 .or. ltc_type .eq. 13) then
          qmax = ftn_atof(word(44))          ! Qmax (p.u.)
          qmin = ftn_atof(word(45))          ! Qmin (p.u.)
          if (k3 .eq. k2) then
            qtemp = qmax
            qmax = -qmin
            qmin = -qtemp
          endif
          kbrnch(4,ltot) = 0
          call putchr(1, 'N', kbrnch(3,ltot))
          brnch(9,ltot) = qmax               ! Qmax (MVAR)
          brnch(10,ltot) = qmin              ! Qmin (MVAR)
        else if (phase_shift) then
          pmax = ftn_atof(word(44))          ! Pmax (p.u.)
          pmin = ftn_atof(word(45))          ! Pmin (p.u.)
          kbrnch(4,ltot) = 0
          call putchr(1, 'M', kbrnch(3,ltot))
          brnch(9,ltot) = pmax               ! Pmax (MVAR)
          brnch(10,ltot) = pmin              ! Pmin (MVAR)
        endif

c       link up branch

        call lkbrdata (ltot, k1x, k2x, bptr1, bptr2, error)

        kx(bptr1) = k1x                                                 
        ky(bptr1) = k2x
        brid(bptr1) = id
        brsect(bptr1) = 0
        brtype(bptr1) = kbrnch(1,ltot)

        kx(bptr2) = k2x                                                 
        ky(bptr2) = k1x
        brid(bptr2) = id
        brsect(bptr2) = 0
        brtype(bptr2) = kbrnch(1,ltot)
      endif
      go to 900

  340 continue
      write (errbuf(1), 10160) 
10160 format ('Premature E-O-F encountered reading Transformer Data segm
     &ent')
      write (errbuf(2), 10120) xbuf(1:80)   
      call prterx ('W',2)                
      ld_geltc = 1                           
      error = 1

  900 continue
      return
      end
