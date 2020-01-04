C    %W% %G%
C****************************************************************
C
C     File: ld_geshn.f
C
C     Purpose: Routine to load GE shunt data from raw data file 
C
c     Return code:  n = 0 : Success
c                   n = 1 : Error
c
C     Author: Walt Powell  Date: 21 May 1996
C     Called by: load_ge.f
C
C****************************************************************
      integer function ld_geshn (xbuf, file, options, error)
      integer file, error, options(*)
      character *(*) xbuf

      include 'ipfinc/parametr.inc'

      include 'ipfinc/prt.inc'
      include 'ipfinc/pti_data.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/alt_case.inc'
      include 'ipfinc/alpha.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/cbus.inc'
      include 'ipfinc/branch.inc'
      include 'ipfinc/xdata.inc'

      character word(60)*10, id*1, type*2, sh_own*3, cbown*3, cbtyp*1, 
     &          cbkyr*2, lntype(9)*2, capital*2, getid*1
      integer find_bus, fnd_ptin, ftn_atoi, status, sect, ptr,
     &        add_cbs, read_ge_file
      logical found

      data lntype / 'L*', 'LM', 'L ', 'R ', 'T ', 'TP', 'E ', 'LM',
     &              'RZ' /

      ld_geshn = 0
      error = 0

      last = lastch0 (xbuf)
      if (xbuf(last:last) .eq. '/') then
        status = read_ge_file (file, xbuf(last:))
        if (status .eq. 0) go to 340
        last = lastch0 (xbuf)
      endif
      call uscan (xbuf(1:last), word, nwrd, '~',  ' ')   

      do i = nwrd+1, 28
        word(i) = ' '
      enddo

      npti1 = ftn_atoi(word(1))
      npti2 = ftn_atoi(word(5))
      type = capital(word(4)(1:2))
      id = getid (word(8))
      sect = ftn_atoi(word(9))
      sh_own = word(21)
      if (word(12) .eq. ' ') then
        status = 1
      else
        status = ftn_atoi(word(12))
      endif
      if (npti1 .eq. 0) then
        if (ichar (xbuf(1:1)) .ge. ichar ('a') .and.
     &      ichar (xbuf(1:1)) .le. ichar ('z')) ld_geshn = 1
      else if (status .eq. 0) then
      else
        kp1 = fnd_ptin (npti1)
        if (kp1 .le. 0) then
          write (errbuf(1), 10020) npti1, npti2
10020     format (' Bus1 on shunt record (', i6, 1x, i6, 
     &      ') is not a valid number.')
          call prterx ('W',1)                
          error = 1                           
          go to 900
        endif
        k1 = find_bus (pti_name(kp1), pti_base(kp1))
        if (k1 .le. 0) then
          write (errbuf(1), 10030) npti1, pti_name(kp1), 
     &       pti_base(kp1)
10030     format (' Bus1 (', i6, 1x, a8, f7.1, ') is not in system.
     &')
          call prterx ('W',1)                
          error = 1                           
          go to 900
        endif
        if (npti2 .le. 0) then
          kp2 = -1
        else
          kp2 = fnd_ptin (npti2)
          if (kp2 .le. 0 .and. type(1:1) .ne. 'B') then
            write (errbuf(1), 10040) npti1, npti2
10040       format (' Bus2 on shunt record (', i6, 1x, i6, 
     &        ') is not a valid number.')
            call prterx ('W',1)                
            error = 1                           
            go to 900
          endif
        endif
        if (kp2 .le. 0 .and. type(1:1) .ne. 'T' .and. 
     &      type(1:1) .ne. 'F') then
c
c         Process bus shunt
c
          ptr = add_cbs (k1, 'A', sh_own, id // ' ')
          if (ptr .eq. 0) go to 900
          bctbl(4,ptr) = bctbl(4,ptr) + ftn_atof(word(15)) * bmva ! G
          bctbl(5,ptr) = bctbl(5,ptr) + ftn_atof(word(16)) * bmva ! B
        else if ((kp2 .le. 0 .and. 
     &           (type(1:1) .eq. 'T' .or. type(1:1) .eq. 'F')) .or.
     &           (kp2 .gt. 0 .and. 
     &           (type(1:1) .ne. 'T' .and. type(1:1) .ne. 'F'))) then
          write (errbuf(1), 10060) npti1, npti2, type
10060     format (' Illegal Bus2 on shunt record (', i6, 1x, i6, 
     &        ') for shunt type (', a, ')')
          call prterx ('W',1)                
          error = 1                           
          go to 900
        else
c
c         Process branch shunt
c
          k2 = find_bus (pti_name(kp2), pti_base(kp2))
          if (k2 .le. 0) then
            write (errbuf(1), 10070) npti2, pti_name(kp2), 
     &         pti_base(kp2)
10070       format (' Bus2 (', i6, 1x, a8, f7.1, ') is not in system.')
            call prterx ('W',1)                
            error = 1                           
            go to 900
          endif
        
          ptr = numbrn (k1, k2, id, sect)
          if (ptr .le. 0) then
            write (errbuf(1), 10080) bus(k1), base(k1), bus(k2),
     &         base(k2), id, sect
10080       format (' Shunt branch (', a8, f7.1, 1x, a8, f7.1, 1x, a1,
     &         i2, ' is not in system.')
            call prterx ('W',1)                
            error = 1                           
            go to 900
          endif
          nbr = iabs (brnch_ptr(ptr))
          if (brtype(ptr) .eq. 3) then
c
c           Convert branch type from L to E
c
            brnch(9,nbr) = brnch(7,nbr)          
            brnch(10,nbr) = brnch(8,nbr)          
            brnch(7,nbr) = brnch(7,nbr)          
            brnch(8,nbr) = brnch(8,nbr)          
            kbrnch(1,nbr) = 8
            brtype(ptr) = 8
          else if (brtype(ptr) .ne. 8) then
            write (errbuf(1), 10090) lntype(brtype(ptr)), bus(k1), 
     &         base(k1), bus(k2), base(k2), id, sect
10090       format (' Shunt data on branch ', a2, 1x, a8, f7.1, 
     &         1x, a8, f7.1, 1x, a1, i2,
     &         ') is applied to an illegal branch type.')
            call prterx ('W',1)                
            error = 1                           
            go to 900
          endif
          if ((type(1:1) .eq. 'F' .and. brnch_ptr(ptr) .gt. 0) .or.
     &        (type(1:1) .eq. 'T' .and. brnch_ptr(ptr) .lt. 0)) then
            brnch(7,nbr) = brnch(7,nbr) + ftn_atof(word(15))       ! G
            brnch(8,nbr) = brnch(8,nbr) + ftn_atof(word(16))       ! B
          else
            brnch(9,nbr) = brnch(9,nbr) + ftn_atof(word(15))       ! G
            brnch(10,nbr) = brnch(10,nbr) + ftn_atof(word(16))     ! B
          endif
          ptr = numbrn (k2, k1, id, sect)
          if (ptr .gt. 0) brtype(ptr) = 8
        endif
      endif
      go to 900

  340 continue
      write (errbuf(1), 10100) 
10100 format (' Premature E-O-F encountered reading Shunt Data segment'
     &)
      write (errbuf(2), 10110) xbuf(1:80)   
10110 format(11x, '(', a, ')')         
      call prterx ('W',2)                
      ld_geshn = 1                           
      error = 1

  900 continue
      return
      end
