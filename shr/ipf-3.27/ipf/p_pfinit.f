C    @(#)p_pfinit.f	20.5 1/7/99
C****************************************************************
C
C   File: p_pfinit.f
C   Purpose: IPF shell program to process /INITIALIZE commands
C
C   Author: Walt Powell  Date: 20 February 1992
C                        Modified: 20 February 1992
C   Called by:
C
C****************************************************************
C
	integer function p_pfinit (in_buffer, out_buffer) 

        character in_buffer*(*)
        character out_buffer*(*)

        include 'ipfinc/parametr.inc'

        include 'ipfinc/pfstates.inc'
        include 'ipfinc/errmsg.inc'

        character null * 1, linefeed * 1, text * 80
        integer status, apdoutbuf, o1, o2, findstr, olderr, chkerr,
     &          bufsize
 
        bufsize = len (out_buffer)
        p_pfinit = 0     ! default return SUCCESS state
        null = char(0)
        linefeed = char(10)
        out_buffer(1:1) = null
c
c       Initialize program variables
c
        call baseinit

        o2 = index (out_buffer,null)
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
        if ( o2 .gt. bufsize - 50 ) then
           o2 = bufsize - 50
           do while ( o2 .gt. 1  .and.
     &                out_buffer(o2:o2) .ne. linefeed )
              o2 = o2 - 1
           enddo
           out_buffer(o2:o2) = null
        endif
        write (text, 340) 'p_pfinit.f', p_pfinit, ostates
  340   format ('/', a, ' return status: ', i2, ' IPF state: ', i2)
        length = apdoutbuf(o2, text, out_buffer(o2:))
        o2 = o2 + length
c
c       Reset error flag
c
        call setercnt (0, ' ')
        return
	end
