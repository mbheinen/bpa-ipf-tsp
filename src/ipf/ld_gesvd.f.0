C    %W% %G%
C****************************************************************
C
C     File: ld_gesvd.f
C
C     Purpose: Routine to load GE SVD data from raw data file 
C
c     Return code:  n = 0 : Success
c                   n = 1 : Error
c
C     Author: Walt Powell  Date: 21 May 1996
C     Called by: load_ge.f
C
C****************************************************************
      integer function ld_gesvd (xbuf, file, options, error)
      integer file, error, options(*)
      character *(*) xbuf

      include 'ipfinc/parametr.inc'

      include 'ipfinc/prt.inc'
      include 'ipfinc/pti_data.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/alpha.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/cbus.inc'
      include 'ipfinc/xdata.inc'

      character word(60)*10, id*1, getid*1
      integer find_bus, fnd_ptin, ftn_atoi, status, ptr, add_cbs,
     &        read_ge_file
      logical found
      real*8 tempx(22)

      ld_gesvd = 0
      error = 0

      last = lastch0 (xbuf)
      if (xbuf(last:last) .eq. '/') then
        status = read_ge_file (file, xbuf(last:))
        if (status .eq. 0) go to 340
        last = lastch0 (xbuf)
      endif
      call uscan (xbuf(1:last), word, nwrd, '~',  ' ')   

      do i = nwrd+1, 49
        word(i) = ' '
      enddo

      npti1 = ftn_atoi(word(1))
      id = getid (word(4))
      npti2 = ftn_atoi(word(5))
      status = ftn_atoi(word(7))
      mode = ftn_atoi(word(8))
      vband = ftn_atof(word(18))
      if (npti1 .eq. 0) then
        if (ichar (xbuf(1:1)) .ge. ichar ('a') .and.
     &      ichar (xbuf(1:1)) .le. ichar ('z')) ld_gesvd = 1
      else if (status .eq. 0) then
      else
        kp1 = fnd_ptin (npti1)
        if (kp1 .le. 0) then
          write (errbuf(1), 10020) npti1, npti2
10020     format (' Bus1 on SVD record (', i6, 1x, i6, 
     &      ') is not a valid number.')
          call prterx ('W',1)                
          error = 1                           
          go to 900
        endif
        k1 = find_bus (pti_name(kp1), pti_base(kp1))
        if (k1 .le. 0) then
          write (errbuf(1), 10030) npti1, pti_name(kp1), 
     &       pti_base(kp1)
10030     format (' Bus1 on SVD record (', i6, 1x, a8, f7.1, 
     &            ') is not in system.')
          call prterx ('W',1)                
          error = 1                           
          go to 900
        endif
        if (npti2 .le. 0) then
          kp2 = kp1
        else
          kp2 = fnd_ptin (npti2)
          if (kp2 .le. 0) then
            write (errbuf(1), 10040) npti1, npti2
10040       format (' Bus2 on SVD record (', i6, 1x, i6, 
     &        ') is not a valid number.')
            call prterx ('W',1)                
            error = 1                           
            go to 900
          endif
        endif
        if (kp2 .eq. kp1) then
          k2 = k1
        else
          k2 = find_bus (pti_name(kp2), pti_base(kp2))
          if (k2 .le. 0) then
            write (errbuf(1), 10050) npti2, pti_name(kp2), 
     &       pti_base(kp2)
10050       format (' Bus2 on SVD record (', i6, 1x, a8, f7.1, 
     &              ') is not in system.')
            call prterx ('W',1)                
            error = 1                           
            go to 900
          endif
        endif

        if (mode .gt. 0) then
          kxndx = 1
          found = .false.
          do while (kxndx .le. kxtot .and. .not. found)
            if (xdata(1,kxndx) .eq. k1) then
              found = .true.
              write (errbuf(1), 10052) bus(k1), base(k1)
10052         format (' Duplicate SVD records (', 
     &          a8, f6.1, '). First record ignored ')
              call prterx ('W',1)                
            else
              kxndx = kxndx + 1
            endif
          enddo

          if (.not. found) then
            if (kxtot .ge. MAXXDT) then
              write (errbuf(1), 10060) MAXXDT
10060         format(' More than ', i4, ' switched reactance entities.')
              errbuf(2) = ' (' // xbuf(1:60) // ')'
              call prterx ('W',2)
              error = 1
              kxtot = 1
              go to 900
            endif
            kxtot = kxtot+1
            kxndx = kxtot
          endif
        endif

        totcap = 0.0
        totrek = 0.0
        tempx(1) = k1
        tempx(2) = k2
        do i = 1, 8
          j = 2*i + 31
          k = 2*i + 5
          tempx(k) = ftn_atof(word(j))
          tempx(k+1) = ftn_atof(word(j+1)) * bmva
          if (tempx(k+1) .gt. 0.0) then
            totcap = totcap + tempx(k) * tempx(k+1)
          else if (tempx(k+1) .lt. 0.0) then
            totrek = totrek + tempx(k) * tempx(k+1)
          endif
        enddo

        tempx(3) = totrek
        tempx(4) = totcap
        tempx(5) = totrek
        tempx(6) = totcap
c
c       Test if more than 10 switchable steps
c
        iflag = 0
        ilast = 0
        do k = 7, 21, 2
          if (tempx(k) .ge. 10.0) iflag = k
          if (tempx(k) .gt. 0.0) ilast = k
        enddo
c
c       Edit any overflowed entity (10 or more steps) 
c       to contain 9 steps and create an overflow entity 
c       to receive steps 10, ....
c
        do while (iflag .ne. 0 .and. ilast .lt. 21)
           do k = 19, iflag, -2
              tempx(k+2) = tempx(k)
              tempx(k+3) = tempx(k+1)
           enddo
           tempx(iflag+2) = tempx(iflag) - 9.0
           tempx(iflag) = 9.0
           iflag = 0
           ilast = 0
           do k = 7, 21, 2
              if (tempx(k) .ge. 10.0) iflag = j
              if (tempx(k) .gt. 0.0) ilast = j
           enddo
        enddo

        do i = 9, 10
          j = 2*i + 31
          xnum = ftn_atof(word(j))
          xstep = ftn_atof(word(j+1)) * bmva
          if (xnum * xstep .ne. 0.0) then
            write (errbuf(1), 10061) bus(k1), base(k1), i, xnum,
     &        xstep
10061       format (' More than 8 discrete steps on SVD record (', 
     &        a8, f6.1, ') : step ', i2, 2f10.3)
            call prterx ('W',1)                
          endif
        enddo

        gused = ftn_atof(word(14))
        bused = ftn_atof(word(15))
        bmin_c = ftn_atof(word(16)) 
        bmax_c = ftn_atof(word(17))
        bmin_tot = ftn_atof(word(19)) 
        bmax_tot = ftn_atof(word(20)) 

        if (bmin_tot .gt. bmax_tot) then
          write (errbuf(1), 10062) bus(k1), base(k1), bmin_tot, 
     &      bmax_tot
10062     format (' Improper Total B limits on SVD record (', a8, f6.1,
     &      ') : ', 2f10.3)
          call prterx ('W',1)                
          temp1 = bmin_tot
          bmax_tot = bmin_tot
          bmin_tot = temp1
        endif
        if (bmin_c .gt. bmax_c) then
          write (errbuf(1), 10064) bus(k1), base(k1), bmin_c, 
     &      bmax_c
10064     format (' Improper Continuous B limits on SVD record (',
     &       a8, f6.1, ') : ', 2f10.3)
          call prterx ('W',1)                
          temp1 = bmin_c
          bmax_c = bmin_c
          bmin_c = temp1
        endif
        if (bmin_c .lt. bmin_tot .or. 
     &      bmax_c .gt. bmax_tot) then
          write (errbuf(1), 10066) bus(k1), base(k1), bmin_c, 
     &      bmax_c, bmin_tot, bmax_tot
10066     format (' Improper B limits on SVD record ('
     &, a8, f6.1, ') : ', 4f10.3)
          call prterx ('W',1)                
          bmax_tot = amax1 (bmax_c, bmax_tot)
          bmin_tot = amin1 (bmin_c, bmin_tot)
        endif
c
c       Mode = 0:  bused -> +A year "X1" if mode == 0
c       Mode = 2 or 4 & bmin_c < 0 && bmax_c > 0:  
c          bmin_c -> +A year "X2" 
c          bmax_c -> +X year "X2" 
c
        if (mode .eq. 0) then
c
c         gused + j bused = fixed
c
          if (abs (gused) .gt. 0.01 .or. abs (bused) .gt. 0.01) then
            ptr = add_cbs (k1, 'A', '   ', 'X1')
            if (ptr .gt. 0) then
              bctbl(4,ptr) = bctbl(4,ptr) + gused * bmva ! G
              bctbl(5,ptr) = bctbl(5,ptr) + bused * bmva ! B
            endif            
          endif
        else if (mode .eq. 1) then
          if (kbsdta(1,k1) .ne. 3) then
            kbsdta(1,k1) = 11
            kbsdta(13,k1) = k2
          endif
          busdta(5,k1) = busdta(5,k1) + gused * bmva
          busdta(6,k1) = busdta(6,k1) + bused * bmva
c
c         bmin_c, bmax_c define continuous shunt added as generation
c
          if (bmax_c - bmin_c .gt. 0.01) then
            ptr = add_cbs (k1, 'X', '   ', 'X2')
            bctbl(11,ptr) = bctbl(11,ptr) 
     &                    + bmax_c * bmva * vstart(k1) ** 2
            bctbl(12,ptr) = bctbl(12,ptr) 
     &                    + bmin_c * bmva / vstart(k1) ** 2
          endif
        else if (mode .eq. 2) then
c
c         bmin_c, bmax_c define continuous shunt
c
          if (bmax_c - bmin_c .gt. 0.1 .or.
     &        totrek .lt. -1.0 .or. totcap .gt. 1.0) then
            if (bmin_c .ne. 0.0 .and. bmax_c .ne. 0.0 ) then
              ptr = add_cbs (k1, 'A', '   ', 'X2')
              if (ptr .gt. 0) then
                bctbl(5,ptr) = bctbl(5,ptr) 
     &                       + bmin_c * bmva ! B
              endif            
              ptr = add_cbs (k1, 'X', '   ', 'X2')
              if (ptr .gt. 0) then
                bctbl(5,ptr) = bctbl(5,ptr) 
     &                       + (bmax_c - bmin_c) * bmva 
     &                       + (totrek + totcap)
              endif            
            else 
              ptr = add_cbs (k1, 'X', '   ', 'X2')
              if (ptr .gt. 0) then
                bctbl(5,ptr) = bctbl(5,ptr) 
     &                       + (bmax_c + bmin_c) * bmva 
     &                       + (totrek + totcap)
              endif            
            endif
          endif
          if (kbsdta(1,k1) .ne. 3) then
            kbsdta(1,k1) = 7
            if (busdta(11,k1) .eq. 0.0 .and. busdta(12,k1) .eq. 0.0) 
     &        busdta(11,k1) = vstart(k1) 
          endif
        else if (mode .eq. 3) then
c
c         totrek + totcap are all or none
c
          if (kbsdta(1,k1) .ne. 3) then
            kbsdta(1,k1) = 11
            kbsdta(13,k1) = k2
          endif
          busdta(5,k1) = busdta(5,k1) + gused * bmva
          busdta(6,k1) = busdta(6,k1) + bused * bmva
          do i = 2, 8
            k = 2*i + 5
            tempx(k) = 0.0
            tempx(k+1) = 0.0
          enddo
          tempx(7) = 1.0
          tempx(8) = totrek + totcap
        else if (mode .eq. 4) then
          if (kbsdta(1,k1) .ne. 3) then
            kbsdta(1,k1) = 11
            kbsdta(13,k1) = k2
          endif
          busdta(5,k1) = busdta(5,k1) + gused * bmva
          busdta(6,k1) = busdta(6,k1) + bused * bmva
          if (busdta(11,k1) .gt. 0.0) then
            busdta(11,k1) = busdta(11,k1) + 0.5 * vband
          endif
          if (busdta(12,k1) .gt. 0.0) then
            busdta(12,k1) = busdta(12,k1) - 0.5 * vband
          endif
        else
          write (errbuf(1), 10088) bus(k1), base(k1), mode
10088     format (' Illegal mode on SVD record ('
     &, a8, f6.1, ') : ', i2)
          call prterx ('W',1)                
        endif
      endif
      if (status .gt. 0 .and. mode .gt. 0) then
        do i = 1, 22
          xdata(i,kxndx) = tempx(i)
        enddo
        if (mode .eq. 1 .or. mode .eq. 3 .or. mode .eq. 4) then
c   
c         Check "discreteness" of initial bus shunt value USED.
c         Function XDSCRT computes new discrete value of USED.
c           
          disc = xdscrt (kxndx, k1, bused*bmva, b1, b2)     
        endif
      endif
      go to 900

  340 continue
      write (errbuf(1), 10090) 
10090 format (' Premature E-O-F encountered reading SVD Data segment'
     &)
      write (errbuf(2), 10100) xbuf(1:80)   
10100 format(11x, '(', a, ')')         
      call prterx ('W',2)                
      ld_gesvd = 1                           
      error = 1

  900 continue
      return
      end
