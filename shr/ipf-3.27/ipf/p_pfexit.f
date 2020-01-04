C    @(#)p_pfexit.f	20.5 8/20/98
C****************************************************************
C
C   File: p_pfexit.f
C   Purpose: IPF shell program to process /EXIT or /QUIT commands.
C
C   Author: Walt Powell  Date: 20 February 1992
C                        Modified: 20 February 1992
C   Called by:
C
C****************************************************************
C
      integer function p_pfexit (in_buffer, out_buffer) 

        character in_buffer*(*)
        character out_buffer*(*)

      include 'ipfinc/parametr.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/filnam.inc'
      include 'ipfinc/jobctl.inc'
      include 'ipfinc/dtaiop.inc'
      include 'ipfinc/errorsw.inc'
      include 'ipfinc/pfstates.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/errmsg.inc'
      include 'ipfinc/errorx.inc'

      character null * 1, linefeed * 1, text * 80
      integer apdoutbuf, o2

      p_pfexit = 0     ! default return SUCCESS state
      numerr = 0       ! reinitialize error count
      null = char(0)
      linefeed = char(10)
      out_buffer(1:1) = null

      if (ostates .eq. 5) then
         p_pfexit = 1
         write (errbuf(1), 150)
  150    format(' Quit without saving solved base data in residence')
         call prterx ('W', 1)
      endif
      call pflend
      call prtime('PFLEND')
      inrcd = buf
c
c       Append error messages to buffer
c
      j = 1 
      length = 1
      o2 = index (out_buffer,null)
      do while (j .le. numerr .and. length .gt. 0)
         length = apdoutbuf(o2, errm(j), out_buffer(o2:))
         o2 = o2 + length
         j = j + 1
      enddo
c
c       Append summary
c
      write (text, 340) 'p_pfexit.f', p_pfexit, ostates
  340 format ('/', a, ' return status: ', i2, ' IPF state: ', i2)
      length = apdoutbuf(o2, text, out_buffer(o2:))
      o2 = o2 + length
c
c       Reset error flag
c
      call setercnt (0, ' ')
      call exit
      end
