C    %W% %G%
C****************************************************************
C
C     File: ld_gebrn.f
C
C     Purpose: Routine to load GE branch data from raw data file 
C
c     Return code:  n = 0 : Success
c                   n = 1 : Error
c
C     Author: Walt Powell  Date: 21 May 1996
C     Called by: load_ge.f
C
C****************************************************************
      integer function ld_gebrn (xbuf, file, options, error)
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

      common /old_branch/ old_k1, old_k2, old_sect, old_type, old_id
      integer old_k1, old_k2, old_sect, old_type
      character old_id * 1

      character word(80)*10, id*1, getid*1
      integer find_bus, fnd_ptin, ftn_atoi, status, bptr1, bptr2, 
     &        find_obr, findoldbus, oldk1, oldk2, oldptr1, oldptr2,
     &        sect, read_ge_file
      real rate_ge(8)

      data oldptr1, oldptr2 / 0, 0/

      save oldptr1, oldptr2

      ld_gebrn = 0
      error = 0

      last = lastch0 (xbuf)
      do while (xbuf(last:last) .eq. '/') 
        status = read_ge_file (file, xbuf(last:))
        last = lastch0 (xbuf)
        if (status .eq. 0) go to 340
      enddo
      call uscan (xbuf(1:last), word, nwrd, '~',  ' ')   

      do i = nwrd+1, 53
        word(i) = ' '
      enddo

      npti1 = ftn_atoi(word(1))
      npti2 = ftn_atoi(word(4))
      id = getid (word(7))
      sect = ftn_atoi(word(8))
      if (word(11) .eq. ' ') then
        status = 1
      else
        status = ftn_atoi(word(11))
      endif
      if (npti1 .eq. 0) then
        if (ichar (xbuf(1:1)) .ge. ichar ('a') .and.
     &      ichar (xbuf(1:1)) .le. ichar ('z')) ld_gebrn = 1
      else if (npti2 .eq. 0) then
      else if (status .eq. 0) then
        if (sect .gt. 1) then
           write (errbuf(1), 9900) npti1, npti2, id, sect, status
 9900      format (' GE branch section (', i6, 1x, i6, 1x, a, 1x, i2, 
     &       ') has status = ', i1, '.')
           call prterx ('W',1)                
        endif
      else if (status .eq. 2) then
        if (sect .gt. 1) then
           write (errbuf(1), 9900) npti1, npti2, id, sect, status
           call prterx ('W',1)                
        endif
      else
        if (ltot+1 .ge. MAXBRN) then
           write (errbuf(1), 10000) MAXBRN
10000      format ('More than ',i5,
     &             ' branch records. Overflow occurred at branch:') 
           write (errbuf(2), 10010) xbuf(1:80)   
10010      format(11x, '(', a, ')')         
           call prterx ('E',2)                
           ltot = 1                           
           error = 1                           
           go to 900
        endif
        kp1 = fnd_ptin (npti1)
        if (kp1 .le. 0) then
           write (errbuf(1), 10020) npti1, npti2
10020      format (' PTI bus1 on branch record (', i6, 1x, i6, 
     &       ') is not in system.')
           call prterx ('W',1)                
           error = 1                           
           go to 900
        endif
        kp2 = fnd_ptin (npti2)
        if (kp2 .le. 0) then
           write (errbuf(1), 10030) npti1, npti2
10030      format (' PTI bus2 on branch record (', i6, 1x, i6, 
     &       ') is not in system.')
           call prterx ('W',1)                
           error = 1                           
           go to 900
        endif
        k1 = find_bus (pti_name(kp1), pti_base(kp1))
        k2 = find_bus (pti_name(kp2), pti_base(kp2))
        if (k1 .le. 0) then
           write (errbuf(1), 10040) npti1, pti_name(kp1), 
     &        pti_base(kp1)
10040      format (' PTI bus1 (', i6, 1x, a8, f7.1, ') is not in system.
     &')
           call prterx ('W',1)                
           error = 1                           
           go to 900
        else if (k2 .le. 0) then
           write (errbuf(1), 10050) npti2, pti_name(kp2), 
     &        pti_base(kp2)
10050      format (' PTI bus2 (', i6, 1x, a8, f7.1, ') is not in system.
     &')
           call prterx ('W',1)                
           error = 1                           
           go to 900
        endif
        ltot = ltot + 1
        line_type = ftn_atoi (word(30))
        kbrnch(1,ltot) = 3
        do i = 2, 18
          kbrnch(i,ltot) = 0
        enddo
        nptiown = ftn_atoi(word(35))
        if (nptiown .ne. 0) then
          call putchr (3, word(35), kbrnch(3,ltot))
        else
          oldk1 = findoldbus(bus(k1), base(k1))
          oldk2 = findoldbus(bus(k2), base(k2))
          if (oldk1 .gt. 0 .and. oldk2 .gt. 0) then
            ltot2x = find_obr (oldk1, oldk2, id, 0)
            if (ltot2x .eq. 0 .and. id .eq. '1') then
              ltot2x = find_obr (oldk1, oldk2, ' ', 0)
            endif
            if (ltot2x .gt. 0) then
              nbr = iabs(obrnch_ptr(ltot2x))
              kbrnch(3,ltot) = okbrnch(3,nbr)
            else
              call putchr (3, '   ', kbrnch(3,ltot))
            endif
          else
            call putchr (3, '   ', kbrnch(3,ltot))
          endif
        endif

        brnch(5,ltot) = ftn_atof(word(12))   ! R
        brnch(6,ltot) = ftn_atof(word(13))   ! X
        if (brnch(6,ltot) .eq. 0.0) then
           write (errbuf(1), 10052) bus(k1), base(k1), bus(k2), 
     &        base(k2), id, sect, 0.0003
10052      format (' Branch ', a8, f7.1, 1x, a8, f7.1, 1x, a1, i2,
     &        ') has zero X changed to ', f7.5)
           call prterx ('W',1)                
           brnch(6,ltot) = 0.0003
           error = 1                           
        endif
        b = ftn_atof(word(14))               ! B
        brnch(8,ltot) = 0.5 * b
        aloss = ftn_atof(word(19))            ! Metering point as %
        if (aloss .lt. 1.0) then
          kbrnch(15,ltot) = 1
        else
          kbrnch(15,ltot) = 2
        endif
        if (kbrnch(1,ltot) .eq. 3) then
          alength = ftn_atof(word(20))        ! Line length
          brnch(9,ltot) = alength
          const = 1000.0 / (sqrt (3.0) * base(k1))
        else
          brnch(9,ltot) = ftn_atof (word(24)) * base(k1)
          brnch(10,ltot) = ftn_atof (word(25)) * base(k2)
          const = 1.0
        endif
C                                                  
C       Add nominal and extended ratings.                 
C                                                  
        rate_ge(1) = const * ftn_atof(word(15))    ! Rate A
        rate_ge(2) = const * ftn_atof(word(16))    ! Rate B
        rate_ge(3) = const * ftn_atof(word(17))    ! Rate C
        rate_ge(4) = const * ftn_atof(word(18))    ! Rate D
        rate_ge(5) = const * ftn_atof(word(31))    ! Rate E
        rate_ge(6) = const * ftn_atof(word(32))    ! Rate F
        rate_ge(7) = const * ftn_atof(word(33))    ! Rate G
        rate_ge(8) = const * ftn_atof(word(34))    ! Rate H
        do i = 1, 8
          rate_ge(i) = amin1 (rate_ge(i), 9999.0)
        enddo
        do i = 5, 8
          if (rate_ge(i) .eq. 0.0) rate_ge(i) = rate_ge(i-4)
        enddo

        if (kbrnch(1,ltot) .eq. 3) then

          i = options(5)
          if (i .eq. 0) i = 1
          brnch(4,ltot) = rate_ge(i)

          i = options(6)
          if (i .eq. 0) i = 1
          rateln(1,ltot) = rate_ge(i)

          i = options(7)
          if (i .eq. 0) i = 2
          rateln(2,ltot) = rate_ge(i)

          rateln(3,ltot) = 0.0

        else

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

        endif

        do i = 1, 3
          rateln(i,ltot) = amin1 (rateln(i,ltot), 9999.0)
        enddo

c       link up branch

        call lkbrdata (ltot, k1, k2, bptr1, bptr2, error)

        kx(bptr1) = k1                                                 
        ky(bptr1) = k2
        brid(bptr1) = id
        brsect(bptr1) = sect
        brtype(bptr1) = kbrnch(1,ltot)

        kx(bptr2) = k2                                                 
        ky(bptr2) = k1
        brid(bptr2) = id
        brsect(bptr2) = sect
        brtype(bptr2) = kbrnch(1,ltot)

        old_k1 = k1
        old_k2 = k2
        old_id = id
        old_sect = sect

      endif

      go to 900

  340 continue
      write (errbuf(1), 10060) 
10060 format (' Premature E-O-F encountered reading Branch Data segment'
     &)
      write (errbuf(2), 10010) xbuf(1:80)   
      call prterx ('W',2)                
      ld_gebrn = 1                           
      error = 1

  900 continue
      return
      end
