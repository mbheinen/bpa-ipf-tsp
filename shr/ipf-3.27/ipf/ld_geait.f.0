C    %W% %G%
C****************************************************************
C
C     File: ld_geait.f
C
C     Purpose: Routine to load PTI area transaction data from raw 
C              data file 
C
c     Return code:  n = 0 : Success
c                   n = 1 : Error
c
C     Author: Walt Powell  Date: 21 May 1996
C     Called by: load_pti.f
C
C****************************************************************
      integer function ld_geait (xbuf, file, options, error)
      integer file, error, options(*)
      character *(*) xbuf

      include 'ipfinc/parametr.inc'

      include 'ipfinc/prt.inc'
      include 'ipfinc/pti_data.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/alt_case.inc'
      include 'ipfinc/alpha.inc'
      include 'ipfinc/area.inc'
      include 'ipfinc/arcntl.inc'

      character word(20)*40
      integer find_ara, fnd_ptic, ftn_atoi, status, read_ge_file

      ld_geait = 0
      error = 0

      last = lastch0 (xbuf)
      do while (xbuf(last:last) .eq. '/') 
        status = read_ge_file (file, xbuf(last:))
        if (status .eq. 0) go to 340
        last = lastch0 (xbuf)
      enddo
      call uscan (xbuf(1:last), word, nwrd, '~',  ' ')   

      do i = nwrd+1, 14
        word(i) = ' '
      enddo

      npti1 = ftn_atoi(word(1))
      npti2 = ftn_atoi(word(4))
      status = ftn_atoi(word(12))
      if (npti1 .eq. 0) then
        if (ichar (xbuf(1:1)) .ge. ichar ('a') .and.
     &      ichar (xbuf(1:1)) .le. ichar ('z')) ld_geait = 1
      else if (status .eq. 0 .or. word(3) .ne. 'a' .or. 
     &         word(6) .ne. 'a') then
      else
        if (ntotic .ge. 5*MAXCAR-1) then
          write (errbuf(1), 10000) 5*MAXCAR
10000     format ('More than ',i5,
     &      ' Area Transaction records. Overflow occurred at record:')
          write (errbuf(2), 10010) xbuf(1:80)   
10010     format(11x, '(', a, ')')         
          call prterx ('E',2)                
          ntotic = 1                           
          error = 1                           
          go to 900
        endif
        kp1 = fnd_ptic (npti1)
        kp2 = fnd_ptic (npti2)
        if (kp1 .le. 0) then
          write (errbuf(1), 10020) npti1, npti2
10020     format (' Area1 on transaction record (', i6, 1x, 
     &       i6, ') is not in system.')
          call prterx ('W',1)                
          error = 1                           
          go to 900
        endif
        if (kp2 .le. 0) then
          write (errbuf(1), 10030) npti1, npti2
10030     format (' Area2 on transaction record (', i6, 1x, 
     &      i6, ') is not in system.')
          call prterx ('W',1)                
          error = 1                           
          go to 900
        endif
        k1 = find_ara (pti_anam(kp1))
        k2 = find_ara (pti_anam(kp2))
        if (k1 .le. 0) then
          write (errbuf(1), 10040) npti1, pti_area(kp1)
10040     format (' Area1 (', i6, 1x, a10, 
     &            ') in transaction data is not in system.')
          call prterx ('W',1)                
          error = 1                           
          go to 900
        else if (k2 .le. 0) then
          write (errbuf(1), 10050) npti2, pti_area(kp2)
10050     format (' Area2 (', i6, 1x, a10, 
     &            ') in transaction data is not in system.')
          call prterx ('W',1)                
          error = 1                           
          go to 900
        endif
        ntotic = ntotic + 1
        arcint(1,ntotic) = arcnam(k1)
        arcint(2,ntotic) = arcnam(k2)
        arcinp(ntotic) = ftn_atof (word(9))
        ntotic = ntotic+1
        arcint(1,ntotic) = arcnam(k2)
        arcint(2,ntotic) = arcnam(k1)
        arcinp(ntotic) = -ftn_atof (word(9))

      endif
      go to 900

  340 write (errbuf(1), 10160)
10160 format ('E-O-F encountered processing transaction data segment in 
     &raw data file ')
      call prterx ('W', 1)
      ld_geait = 1
      error = 1                           

  900 continue
      return
      end
