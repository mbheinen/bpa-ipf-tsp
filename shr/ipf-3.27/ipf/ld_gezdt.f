C    %W% %G%
C****************************************************************
C
C     File: ld_gezdt.f
C
C     Purpose: Routine to load GE Z data from raw data file 
C
c     Return code:  n = 0 : Success
c                   n = 1 : Error
c
C     Author: Walt Powell  Date: 21 May 1996
C     Called by: load_ge.f
C
C****************************************************************
      integer function ld_gezdt (xbuf, file, options, error)
      integer error, file, options(*)
      character *(*) xbuf

      include 'ipfinc/parametr.inc'

      include 'ipfinc/prt.inc'
      include 'ipfinc/pti_data.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/tx_misc.inc'

      character word(30)*10
      integer ftn_atoi, status, read_ge_file

      ld_gezdt = 0
      error = 0

      last = lastch0 (xbuf)
      if (xbuf(last:last) .eq. '/') then
        status = read_ge_file (file, xbuf(last:))
        if (status .eq. 0) go to 340
        last = lastch0 (xbuf)
      endif
      call uscan (xbuf(1:last), word, nwrd, '~',  ' ')   

      do i = nwrd+1, 22
        word(i) = ' '
      enddo

      npti1 = ftn_atoi(word(1))
      if (npti1 .eq. 0) then
        if (ichar (xbuf(1:1)) .ge. ichar ('a') .and.
     &      ichar (xbuf(1:1)) .le. ichar ('z')) ld_gezdt = 1
      else
        if (num_zdata .ge. MAX_ZDATA) then
          write (errbuf(1), 10000) npti1
10000     format (' more than ', i4, ' Z Data records encountered')
          call prterx ('W',1)                
          error = 1                           
        else
          num_zdata = num_zdata + 1
          tx_zdata(1,num_zdata) = npti1
          tx_zdata(2,num_zdata) = ftn_atoi(word(2))
          do i = 3, 26
            tx_zdata(i,num_zdata) = ftn_atof(word(i))
          enddo
        endif
      endif
      go to 900

  340 continue
      write (errbuf(1), 10090) 
10090 format (' Premature E-O-F encountered reading Z Data segment')
      write (errbuf(2), 10100) xbuf(1:80)   
10100 format(11x, '(', a, ')')         
      call prterx ('W',2)                
      ld_gezdt = 1                           
      error = 1

  900 continue
      return
      end
