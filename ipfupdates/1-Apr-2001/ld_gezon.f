C    %W% %G%
C****************************************************************
C
C     File: ld_gezon.f
C
C     Purpose: Routine to load GE Zone data from raw data file 
C
c     Return code:  n = 0 : Success
c                   n = 1 : Error
c
C     Author: Walt Powell  Date: 21 May 1996
C     Called by: load_ge.f
C
C****************************************************************
      integer function ld_gezon (xbuf, file, options, error)
      integer error, file, options(*)
      character *(*) xbuf

      include 'ipfinc/parametr.inc'

      include 'ipfinc/prt.inc'
      include 'ipfinc/pti_data.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/arcntl.inc'

      common /ge_zones/ num_gezones, htable_geznum(MAXCZN),
     &                  nextptr_geznum(MAXCZN),
     &                  htable_geznam(MAXCZN),
     &                  nextptr_geznam(MAXCZN),
     &                  ge_znum(MAXCZN),
     &                  ge_znam(MAXCZN)
      integer num_gezones, htable_geznum, nextptr_geznum,
     &        htable_geznam, nextptr_geznam, ge_znum
      character ge_znam*2

      character word(20)*40, capital*40, tempc1*40, tempc2*40, 
     &          comprs*40
      integer ftn_atoi, add_ptiz, fnd_ptiy, status, fd_gezones,
     &        read_ge_file

      ld_gezon = 0
      error = 0

      last = lastch0(xbuf)
      do while (xbuf(last:last) .eq. '/')
        status = read_ge_file (file, xbuf(last:))
        if (status .eq. 0) go to 340
        last = lastch0 (xbuf)
      enddo
      call uscan (xbuf(1:last), word, nwrd, '~',  ' ')   

      do i = nwrd+1, 4
        word(i) = ' '
      enddo

      npti1 = ftn_atoi(word(1))
      if (npti1 .eq. 0 .and. word(1) .ne. '0') then
        if (ichar (xbuf(1:1)) .ge. ichar ('a') .and.
     &      ichar (xbuf(1:1)) .le. ichar ('z')) ld_gezon = 1
      else
        numgez = 0
        if (options(10) .eq. 1) then
          numgez = fd_gezones (npti1, '  ')
          if (numgez .gt. 0) then
            tempc1 = ge_znam(numgez)
          endif
        endif
        if (numgez .eq. 0) then
          tempc1 = capital(word(2))
          tempc2 = comprs(tempc1)
          i = index (tempc2, 'ZONE')
          if (i .eq. 1) then
            tempc1 = tempc2(5:)
          else if (i .gt. 1) then
            tempc1 = tempc2(1:i-1) // tempc2(i+4:)
          else
            tempc1 = tempc2
          endif
        endif

        num = add_ptiz (npti1, tempc1(1:2))
        zone_name(num) = tempc1
        if (pti_znam(num) .ne. tempc1) then
          last = lastch (word(2))
          write (errbuf(1), 10020) npti1, word(2)(1:last), 
     &      pti_znam(num)
10020     format (' Zone on GE Zone record (', 
     &      i6, 1x, a, ') renamed to (', a, ')')
          call prterx ('I',1)                
        endif
      endif
      go to 900

  340 continue
      write (errbuf(1), 10090) 
10090 format (' Premature E-O-F encountered reading Zone Data segment')
      write (errbuf(2), 10100) xbuf(1:80)   
10100 format(11x, '(', a, ')')         
      call prterx ('W',2)                
      ld_gezon = 1                           
      error = 1

  900 continue
      return
      end
