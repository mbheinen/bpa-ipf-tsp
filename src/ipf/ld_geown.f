C    %W% %G%
C****************************************************************
C
C     File: ld_geown.f
C
C     Purpose: Routine to load GE Owner data from raw data file 
C
c     Return code:  n = 0 : Success
c                   n = 1 : Error
c
C     Author: Walt Powell  Date: 21 May 1996
C     Called by: load_ge.f
C
C****************************************************************
      integer function ld_geown (xbuf, file, options, error)
      integer error, file, options(*)
      character *(*) xbuf

      include 'ipfinc/parametr.inc'

      include 'ipfinc/prt.inc'
      include 'ipfinc/pti_data.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/owner_cm.inc'

      character word(20)*40, capital*40, tempc*40
      integer ftn_atoi, add_ptio, status, read_ge_file

      ld_geown = 0
      error = 0

      last = lastch0 (xbuf)
      if (xbuf(last:last) .eq. '/') then
        status = read_ge_file (file, xbuf(last:))
        if (status .eq. 0) go to 340
        last = lastch0 (xbuf)
      endif
      call uscan (xbuf(1:last), word, nwrd, '~',  ' ')   

      do i = nwrd+1, 9
        word(i) = ' '
      enddo
c
c     Allow ownership number "0"
c
      npti1 = ftn_atoi(word(1))
      if (npti1 .eq. 0 .and. word(1) .ne. '0') then
        if (ichar (xbuf(1:1)) .ge. ichar ('a') .and.
     &      ichar (xbuf(1:1)) .le. ichar ('z')) ld_geown = 1
      else
        tempc = capital(word(3))
        num = add_ptio (npti1, tempc(1:3))
c
c       If num < 0, the owner exists
c
        if (num .lt. 0) then
          last = lastch (word(2))
          write (errbuf(1), 10000) npti1, word(3)(1:3), 
     &       word(2)(1:last)
10000     format (' Owner GE record (', 
     &        i6, 1x, a, 1x, a, ') could not be hashed.')
          call prterx ('W',1)                
          error = 1                           
        else
          owner_name(num) = word(2)
        endif
      endif
      go to 900

  340 continue
      write (errbuf(1), 10090) 
10090 format (' Premature E-O-F encountered reading Owner Data segment')
      write (errbuf(2), 10100) xbuf(1:80)   
10100 format(11x, '(', a, ')')         
      call prterx ('W',2)                
      ld_geown = 1                           
      error = 1

  900 continue
      return
      end
