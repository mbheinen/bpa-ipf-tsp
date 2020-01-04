C    @(#)p_solton.f	20.4 8/20/98
C****************************************************************
C
C   File: p_solton.f
C   Purpose: IPF shell program to process /SOLUTION commands
C
C   Author: Walt Powell  Date: 20 February 1992 
C                        Modified: 20 February 1992
C   Called by: 
C
C****************************************************************
C
	integer function p_solton( in_buffer, out_buffer ) 

        character in_buffer*(*)
        character out_buffer*(*)

        include 'ipfinc/parametr.inc'
        include 'ipfinc/errmsg.inc'
        include 'ipfinc/pfstates.inc'

        character * 1  null, linefeed
        integer   apdoutbuf, o2
        character * 50  stext

        max_buf = len( out_buffer ) - len( stext ) - 10
        if (max_buf .le. 0) max_buf = len( out_buffer )
        numerr = 0       ! reinitialize error count
        null = char(0)
        linefeed = char(10)
        out_buffer(1:1) = null
        o2 = 1
        
        p_solton = isolton()
c
c       Append error messages to buffer
c
        j = 1 
        length = 1
        do while ( j .le. numerr  .and.  length .gt. 0 )
           length = apdoutbuf( o2, errm(j), out_buffer(o2:) )
           o2 = o2 + length
           j = j + 1
        enddo
c
c 	Append summary
c
        if ( o2 .gt. max_buf ) then
           o2 = max_buf
           do while ( o2 .gt. 1  .and.
     &                out_buffer(o2:o2) .ne. linefeed )
              o2 = o2 - 1
           enddo
           out_buffer(o2:o2) = null
        endif
        write (stext, 911) 'p_solton.f', p_solton, ostates
  911   format ('/', a, ' return status: ', i2, ' IPF state: ', i2)
        length = apdoutbuf( o2, stext, out_buffer(o2:) )
        o2 = o2 + length
c
c       Reset error flag
c
        call setercnt( 0, ' ' )
        return
	end
