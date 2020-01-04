C    %W% %G%
C****************************************************************
C
C     File: ld_geare.f
C
C     Purpose: Routine to load GE Area Interchange data from 
C              raw data file 
C
c     Return code:  n = 0 : Success
c                   n = 1 : Error
c
C     Author: Walt Powell  Date: 21 May 1996
C     Called by: load_pti.f
C
C****************************************************************
      integer function ld_geare (xbuf, file, options, error)
      integer file, error, options(*)
      character *(*) xbuf

      include 'ipfinc/parametr.inc'

      include 'ipfinc/prt.inc'
      include 'ipfinc/pti_data.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/arcntl.inc'

      character word(10)*40
      integer find_bus, fnd_ptin, ftn_atoi, ptihasha, status, 
     &        read_ge_file

      ld_geare = 0
      error = 0

      last = lastch0 (xbuf)
      if (xbuf(last:last) .eq. '/') then
        status = read_ge_file (file, xbuf(last:))
        if (status .eq. 0) go to 340
        last = lastch0 (xbuf)
      endif
      call uscan (xbuf(1:last), word, nwrd, '~',  ' ')   

      do i = nwrd+1, 7
        word(i) = ' '
      enddo

      npti1 = ftn_atoi(word(1))
      if (npti1 .eq. 0) then
        if (ichar (xbuf(1:1)) .ge. ichar ('a') .and.
     &      ichar (xbuf(1:1)) .le. ichar ('z')) ld_geare = 1
      else
        num = ptihasha (npti1, word(2))
        if (num .le. 0) then
          write (errbuf(1), 10000) npti1, word(2)(1:10)
10000     format (' Area on GE Area Interchange record (', 
     &       i6, 1x, a, ') could not be hashed.')
          call prterx ('W',1)                
          error = 1                           
          go to 900
        endif
        npti2 = ftn_atoi(word(3))
        kp2 = fnd_ptin (npti2)
        if (kp2 .le. 0) then
          write (errbuf(1), 10010) npti1, npti2
10010     format (' Area slack bus on GE Area Interchange record (', 
     &       i6, 1x, i6, ') is not in system.')
          call prterx ('W',1)                
          error = 1                           
          go to 900
        endif
        k2 = find_bus (pti_name(kp2), pti_base(kp2))
        if (k2 .le. 0) then
          write (errbuf(1), 10020) npti2, pti_name(kp2), pti_base(kp2),
     &       npti1
10020     format (' Slack bus bus (', i6, 1x, a8, f7.1, 
     &       ') on Area Interchange record (', i6, 
     &       ') is not in system.')
          call prterx ('W',1)                
          error = 1                           
          go to 900
        endif
        if (ntotc .ge. MAXCAR) then
          write (errbuf(1), 10030) MAXCAR
10030     format ('More than ', i5, ' Area Interchange records. Overflow
     & occurred at record:')
          write (errbuf(2), 10040) xbuf(1:80)   
10040     format(11x, '(', a, ')')         
          call prterx ('W',2)                
          ntotc = 1                           
          error = 1                           
          go to 900
        endif

        ntotc = ntotc + 1

        arcnam(ntotc) = pti_anam(num)
        arcnet(ntotc) = ftn_atof (word(4)) / bmva
        arcbus(ntotc) = pti_name(kp2)
        arcbas(ntotc) = pti_base(kp2)
        area_number(ntotc) = npti1
        do j = 1, MAXCAZ
          arczns(j,ntotc) = ' '
        enddo
      endif
      go to 900

  340 continue
      write (errbuf(1), 10090) 
10090 format (' Premature E-O-F encountered reading Area Data segment')
      write (errbuf(2), 10100) xbuf(1:80)   
10100 format(11x, '(', a, ')')         
      call prterx ('W',2)                
      ld_geare = 1                           
      error = 1

  900 continue
      return
      end
