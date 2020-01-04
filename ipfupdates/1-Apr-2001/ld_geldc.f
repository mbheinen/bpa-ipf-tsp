C    %W% %G%
C****************************************************************
C
C     File: ld_geldc.f
C
C     Purpose: Routine to load GE mulit-terminal d-c branch data from 
C              raw data file 
C
c     Return code:  n = 0 : Success
c                   n = 1 : Error
c
C     Author: Walt Powell  Date: 21 May 1996
C     Called by: load_ge.f
C
C****************************************************************
      integer function ld_geldc (xbuf, file, options, error)
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

      character word(100)*10, id*1, getid*1
      integer find_bus, fnd_ptin, ftn_atoi, status, bptr1, bptr2, 
     &        oldptr1, oldptr2, gtnwdcno, read_ge_file

      data oldptr1, oldptr2 / 0, 0/

      save oldptr1, oldptr2

      ld_geldc = 0
      error = 0

      last = lastch0 (xbuf)
      if (xbuf(last:last) .eq. '/') then
        status = read_ge_file (file, xbuf(last:))
        if (status .eq. 0) go to 340
        last = lastch0 (xbuf)
      endif
      call uscan (xbuf(1:last), word, nwrd, '~',  ' ')   

      do i = nwrd+1, 45
        word(i) = ' '
      enddo

      num1 = ftn_atoi(word(1))
      num2 = ftn_atoi(word(4))

      id = getid (word(9))
      if (word(11) .eq. ' ') then
        status = 1
      else
        status = ftn_atoi(word(10))
      endif
      if (num1 .eq. 0) then
        if (ichar (xbuf(1:1)) .ge. ichar ('a') .and.
     &      ichar (xbuf(1:1)) .le. ichar ('z')) ld_geldc = 1
      else if (num2 .eq. 0 .or. status .eq. 0) then
      else
        if (ltot+1 .ge. MAXBRN) then
          write (errbuf(1), 10000) MAXBRN
10000     format ('More than ',i5,
     &            ' branch records. Overflow occurred at branch:') 
          write (errbuf(2), 10010) xbuf(1:80)   
10010     format(11x, '(', a, ')')         
          call prterx ('E',2)                
          ltot = 1                           
          error = 1                           
          go to 900
        endif
        npti1 = gtnwdcno (num1)
        npti2 = gtnwdcno (num2)
        kp1 = fnd_ptin (npti1)
        if (kp1 .le. 0) then
          write (errbuf(1), 10020) num1, num2
10020     format (' Bus1 on d-c branch record (', i6, 1x, i6, 
     &      ') is not a valid bus number.')
          call prterx ('W',1)                
          error = 1                           
          go to 900
        endif
        kp2 = fnd_ptin (npti2)
        if (kp2 .le. 0) then
          write (errbuf(1), 10030) num1, num2
10030     format (' Bus2 on d-c branch record (', i6, 1x, i6, 
     &      ') is not a valid bus number.')
          call prterx ('W',1)                
          error = 1                           
          go to 900
        endif
        k1 = find_bus (pti_name(kp1), pti_base(kp1))
        k2 = find_bus (pti_name(kp2), pti_base(kp2))
        if (k1 .le. 0) then
          write (errbuf(1), 10040) npti1, pti_name(kp1), 
     &      pti_base(kp1)
10040     format (' D-C bus1 (', i6, 1x, a8, f7.1, ') is not in system.
     &')
          call prterx ('W',1)                
          error = 1                           
          go to 900
        else if (k2 .le. 0) then
          write (errbuf(1), 10050) npti2, pti_name(kp2), 
     &       pti_base(kp2)
10050     format (' D-C bus2 (', i6, 1x, a8, f7.1, ') is not in system.
     &')
          call prterx ('W',1)                
          error = 1                           
          go to 900
        endif
        ltot = ltot + 1
        kbrnch(1,ltot) = 2
        do i = 2, 18
          kbrnch(i,ltot) = 0
        enddo
        call putchr (3, word(30), kbrnch(3,ltot))
        brnch(5,ltot) = ftn_atof(word(13))   ! Rdc
        brnch(6,ltot) = ftn_atof(word(14))   ! Ldc
        brnch(7,ltot) = ftn_atof(word(15))   ! Cdc

        const = 1000.0 * base(k1)
        ratea = const * ftn_atof(word(16)) ! Rate A
        rateb = const * ftn_atof(word(17)) ! Rate B
        ratec = const * ftn_atof(word(18)) ! Rate C
        rated = const * ftn_atof(word(19)) ! Rate D
        ratea = amin1 (ratea, 9999.0)
        rateb = amin1 (rateb, 9999.0)
        rateb = amin1 (rateb, 9999.0)
        if (options(5) .eq. 3) then
          brnch(4,ltot) = ratec            ! Rate
        else
          brnch(4,ltot) = ratea            ! Rate
        endif
C                                                  
C       Add extended ratings.                 
C                                                  
        if (options(6) .eq. 3) then
          rateln(1,ltot) = ratec            ! Rate C
        else
          rateln(1,ltot) = ratea            ! Rate A
        endif
        if (options(7) .eq. 1) then
          rateln(2,ltot) = ratea            ! Rate A
        else
          rateln(2,ltot) = ratec            ! Rate C
        endif

c       link up branch

        call lkbrdata (ltot, k1, k2, bptr1, bptr2, error)
        kx(bptr1) = k1                                                 

        ky(bptr1) = k2
        brid(bptr1) = id
        brsect(bptr1) = 0
        brtype(bptr1) = kbrnch(1,ltot)

        kx(bptr2) = k2                                                 
        ky(bptr2) = k1
        brid(bptr2) = id
        brsect(bptr2) = 0
        brtype(bptr2) = kbrnch(1,ltot)

        old_k1 = k1
        old_k2 = k2
        old_id = id
        old_sect = 0

      endif

      go to 900

  340 continue
      write (errbuf(1), 10060) 
10060 format (' Premature E-O-F encountered reading Branch Data segment'
     &)
      write (errbuf(2), 10010) xbuf(1:80)   
      call prterx ('W',2)                
      ld_geldc = 1                           
      error = 1

  900 continue
      return
      end
