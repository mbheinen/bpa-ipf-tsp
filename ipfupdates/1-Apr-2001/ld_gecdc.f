c    %W% %G%
C****************************************************************
C
C     File: ld_gecdc.f
C
C     Purpose: Routine to load GE multi-terminal d-c data from raw 
C              data file 
C
c     Return code:  n = 0 : Success
c                   n = 1 : Error
c
C     Author: Walt Powell  Date: 21 May 1996
C     Called by: load_ge.f
C
C****************************************************************
      integer function ld_gecdc (xbuf, file, options, error)
      integer file, options(*), error
      character *(*) xbuf

      include 'ipfinc/parametr.inc'

      include 'ipfinc/prt.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/alt_case.inc'
      include 'ipfinc/area.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/branch.inc'
      include 'ipfinc/pti_data.inc'
      include 'ipfinc/lfiles.inc'

      character word(100)*10, dccode*1, id*1, old_id*1, getid*1
      integer fnd_ptin, ftn_atoi, findoldbus, find_obr, gtnwdcno,
     &        find_bus, conv_type, conv_mode, conv_ltc, bptr1, bptr2, 
     &        oldk1, oldk2, status, read_ge_file

      ld_gecdc = 0
      error = 0

      last = lastch0 (xbuf)
      do while (xbuf(last:last) .eq. '/') 
        status = read_ge_file (file, xbuf(last:))
        if (status .eq. 0) go to 340
        last = lastch0 (xbuf)
      enddo
      call uscan (xbuf(1:last), word, nwrd, '~',  ' ')   

      do i = nwrd+1, 76
        word(i) = ' '
      enddo

      npti1 = ftn_atoi(word(1))
      id = getid (word(7))
      status = ftn_atoi(word(10))
      if (npti1 .eq. 0) then
        if (ichar (xbuf(1:1)) .ge. ichar ('a') .and.
     &      ichar (xbuf(1:1)) .le. ichar ('z')) ld_gecdc = 1
      else if (status .eq. 0) then
      else
c
c       Complete converter bus d-c data
c
        num2 = ftn_atoi(word(4))
        npti2 = gtnwdcno (num2)
        kp1 = fnd_ptin (npti1)
        if (kp1 .le. 0) then
          write (errbuf(1), 10020) npti1, num2
10020     format (' A-C commuator bus on d-c converter record (', i6, 
     &      1x, i6, ') does not have a valid bus number.')
          call prterx ('W',1)                
          error = 1                           
          go to 900
        endif
        kp2 = fnd_ptin (npti2)
        if (kp2 .le. 0) then
          write (errbuf(1), 10030) npti1, num2
10030     format (' D-C converter bus on converter record (', i6, 1x, 
     &      i6, ') does not have a valid bus number.')
          call prterx ('W',1)                
          error = 1                           
          go to 900
        endif
        k1 = find_bus (pti_name(kp1), pti_base(kp1))
        k2 = find_bus (pti_name(kp2), pti_base(kp2))
        if (k1 .le. 0) then
          write (errbuf(1), 10040) npti1, pti_name(kp1), 
     &      pti_base(kp1)
10040     format (' A-C commuatator bus (', i6, 1x, a8, f7.1, 
     &      ') is not in system.')
          call prterx ('W',1)                
          error = 1                           
          go to 900
        else if (k2 .le. 0) then
          write (errbuf(1), 10050) npti2, pti_name(kp2), 
     &       pti_base(kp2)
10050     format (' D-C converter bus (', i6, 1x, a8, f7.1, 
     &      ') is not in system.')
          call prterx ('W',1)                
          error = 1                           
          go to 900
        endif
        if (kbsdta(1,k2) .ne. 12) then
          write (errbuf(1), 10060) npti2, pti_name(kp2), 
     &       pti_base(kp2)
10060     format (' D-C converter bus (', i6, 1x, a8, f7.1, 
     &      ') is changed to type BM.')
          call prterx ('W',1)                
          kbsdta(1,k2) = 12
          error = 1                           
        endif
        conv_type = ftn_atoi (word(11))
        bridges = ftn_atof (word(13))          !Number of Bridges
        busdta(3,k2) = bridges
        busdta(4,k2) = ftn_atof (word(57))     !Smoothing Reactor
        if (conv_type .eq. 1) then
          busdta(5,k2) = ftn_atof (word(36))   !Alpha min
          busdta(6,k2) = ftn_atof (word(37))   !Alpha max
          busdta(10,k2) = ftn_atof (word(35))  !Alpha nominal
          busdta(11,k2) = ftn_atof (word(36))  !Gamma min
        else
          busdta(5,k2) = ftn_atof (word(39))   !Gamma min
          busdta(6,k2) = ftn_atof (word(40))   !Gamma max
          busdta(10,k2) = ftn_atof (word(38))  !Alpha nominal
          busdta(11,k2) = ftn_atof (word(39))  !Gamma min
        endif
        busdta(8,k2) = 0.0                     !Current rating
        kbsdta(9,k2) = k1                      !Converter bus
        if (conv_type .eq. 1) then
          dccode = 'R'
        else
          dccode = 'I'
        endif
        call putchr(1, dccode, kbsdta(12,k2))
        conv_mode = ftn_atoi (word(12))
        pdc = ftn_atof (word(26))
        adc = ftn_atof (word(27))
        if (conv_type .eq. 2) adc = -abs (adc)
        vdc = ftn_atof (word(28))
        if ((conv_type .eq. 1 .and. 
     &      (conv_mode .eq. 2. or. conv_mode .eq. 3) .and.
     &       adc .lt. 0.0) .or.
     &      (conv_type .eq. 1 .and.
     &      (conv_mode .eq. 3. or. conv_mode .eq. 5) .and.
     &       pdc .lt. 0.0)) then
          write (errbuf(1), 10132) bus(k1), base(k1), bus(k2), base(k2),
     &      dccode, pdc, adc
10132     format (' D-C converter (', a8, f7.1, ' - ', a8, f7.1,
     &      ' Mode ', a, ' has negative Idc or Pdc changed ', 2f8.1)
          call prterx ('W',1)                
          adc = abs (adc)
          pdc = abs (pdc)
        else if ((conv_type .eq. 2 .and. 
     &           (conv_mode .eq. 2. or. conv_mode .eq. 3) .and.
     &            adc .gt. 0.0) .or.
     &           (conv_type .eq. 2 .and.
     &           (conv_mode .eq. 3. or. conv_mode .eq. 5) .and.
     &            pdc .gt. 0.0)) then
          write (errbuf(1), 10132) bus(k1), base(k1), bus(k2), base(k2),
     &      dccode, pdc, adc
          call prterx ('W',1)                
          adc = -abs (adc)
          pdc = -abs (pdc)
        endif
        num3 = ftn_atoi(word(15))
        if (num3 .gt. 0) then
          npti3 = gtnwdcno (num3)
          kp3 = fnd_ptin (npti3)
          if (kp3 .le. 0) then
            write (errbuf(1), 10140) num3, npti1, bus(k1), 
     &         base(k1), npti2, bus(k2), base(k2)
10140       format (' LTC controlled bus (', i6, 
     &         ') on branch record (', i6, 1x, a8, f7.1, ' - ', 
     &         1x, i6, 1x, a8, f7.1, ') is not in system.')
            call prterx ('W',1)                
            error = 1                           
            go to 900
          endif
          k3 = find_bus (pti_name(kp3), pti_base(kp3))
          if (k3 .le. 0) then
            write (errbuf(1), 10150) num3, pti_name(kp3), 
     &         pti_base(kp3), npti1, bus(k1), base(k1), 
     &         npti2, bus(k2), base(k2)
10150       format (' LTC controlled bus (', i6, 1x, a8, f7.1, 
     &         ') on branch record (', i6, 1x, a8, f7.1, ' - ', i6,
     &         1x, a8, f7.1, ') is not in system.')
            call prterx ('W',1)                
            error = 1                           
            go to 900
          endif
        else
          k3 = 0
        endif
        if (vdc .eq. 0.0) vdc = ftn_atof (word(23))       
        if (conv_mode .eq. 1) then
          busdta(13,k2) = 0.0                 !Pdc        
          busdta(14,k2) = 0.0                 !Vdc        
        else if (conv_mode .eq. 2) then
          busdta(13,k2) = 0.001 * adc * vdc   !Pdc = vdc * adc       
          busdta(14,k2) = 0.0                 !Vdc        
        else if (conv_mode .eq. 3) then
          busdta(13,k2) = pdc                 !Pdc 
          busdta(14,k2) = 0.0                 !Vdc        
        else if (conv_mode .eq. 4) then
          busdta(13,k2) = 0.001 * adc * vdc   !Pdc
          busdta(14,k2) = vdc                 !Vdc = vdc * adc        
          if (k2 .ne. k3 .and. k3 .gt. 0) then
            write (errbuf(1), 10152) bus(k1), base(k1), bus(k2), 
     &         base(k2), conv_mode, vdc, bus(k3), base(k3)
10152       format (' D-C converter (', a8, f7.1, 1x, a8, f7.1, 
     &         ') mode (', i1, ') vdc (', f10.3, 
     &         ') at converter (', a8, f7.1, ')')
            call prterx ('W',1)                
          endif
        else if (conv_mode .eq. 5) then
          busdta(13,k2) = pdc                 !Pdc
          busdta(14,k2) = vdc                 !Vdc        
          if (k2 .ne. k3 .and. k3 .gt. 0) then
            write (errbuf(1), 10152) bus(k1), base(k1), bus(k2), 
     &         base(k2), conv_mode, vdc, bus(k3), base(k3)
            call prterx ('W',1)                
          endif
        else
          busdta(13,k2) = 0.0                 !Pdc
          busdta(14,k2) = 0.0                 !Vdc        
        endif
c
c       Add commutating TX branch
c
        if (ltot+1 .ge. MAXBRN) then
           write (errbuf(1), 10070) MAXBRN
10070      format ('More than ',i5,
     &        ' branch records. Overflow occurred at d-c converter') 
           write (errbuf(2), 10080) xbuf(1:80)   
10080      format(11x, '(', a, ')')         
           call prterx ('E',2)                
           ltot = 1                           
           error = 1                           
           go to 900
        endif

        ltot = ltot + 1
        kbrnch(1,ltot) = 5
        nptiown = ftn_atoi(word(60))
        if (nptiown .ne. 0) then
          call putchr (3, word(60), kbrnch(3,ltot))
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
        if (bridges .eq. 0.0) bridges = 1.0
        brnch(4,ltot) = 0.0
        brnch(5,ltot) = ftn_atof(word(43)) / bridges   ! R 
        brnch(6,ltot) = ftn_atof(word(44)) / bridges   ! X
        brnch(7,ltot) = 0.0
        brnch(8,ltot) = 0.0
        txbase1 = ftn_atof(word(33))
        txbase2 = ftn_atof(word(34))
        brnch(9,ltot) = ftn_atof(word(46)) * txbase1
        brnch(10,ltot) = ftn_atof(word(47)) * txbase2
        aloss = ftn_atof(word(55))            ! Metering point as %
        if (aloss .lt. 1.0) then
          kbrnch(15,ltot) = 1
        else
          kbrnch(15,ltot) = 2
        endif
        rateln(1,ltot) = 0.0                  ! Rate A
        rateln(2,ltot) = 0.0                  ! Rate B
        rateln(3,ltot) = 0.0                  ! Rate C

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
c
c       Add LTC TX branch
c
        tmax = ftn_atof(word(51))
        tmin = ftn_atof(word(50))
        if (amax1 (tmax, tmin) .gt. 0.0 .and.
     &      amin1 (tmax, tmin) .gt. 0.0) then
          if (ltot+1 .ge. MAXBRN) then
            write (errbuf(1), 10070) MAXBRN
            write (errbuf(2), 10080) xbuf(1:80)   
            call prterx ('E',2)                
            ltot = 1                           
            error = 1                           
            go to 900
          endif

          ltot = ltot + 1
          kbrnch(1,ltot) = 4
          do i = 2, 18
            kbrnch(i,ltot) = 0
          enddo
          call putchr (3, ' ', kbrnch(3,ltot))

          kbrnch(4,ltot) = k2

          brnch(6,ltot) = ftn_atof(word(51)) * txbase1  ! Tmax
          brnch(7,ltot) = ftn_atof(word(50)) * txbase1  ! Tmin
          stepsize = ftn_atof(word(52))   ! Steps
          if (stepsize .gt. 1.0) then
            brnch(8,ltot) = (brnch(6,ltot) - brnch(7,ltot))
     &                    / stepsize + 1.0
          endif

c         link up branch

          call lkbrdata (ltot, k1, k2, bptr1, bptr2, error)

          kx(bptr1) = k1                                         
          ky(bptr1) = k2
          brid(bptr1) = ' '
          brsect(bptr1) = 0
          brtype(bptr1) = kbrnch(1,ltot)

          kx(bptr2) = k2                                               
          ky(bptr2) = k1
          brid(bptr2) = ' '
          brsect(bptr2) = 0
          brtype(bptr2) = kbrnch(1,ltot)

          old_k1 = k1
          old_k2 = k2
          old_id = id
          old_sect = 0
        endif
      endif
      go to 900

  340 write (errbuf(1), 10160)
10160 format ('E-O-F encountered processing dc bus data segment in raw d
     &ata file ')
      call prterx ('W', 1)
      ld_gecdc = 1
      error = 1                           

  900 continue
      return
      end
