C    @(#)apnd_stat.f	20.4 1/7/99
C****************************************************************
C   File: apnd_stat.f
C   Purpose: Append any error messages and the return status to the
C            IPC out_buffer.
C   Author:  Jay Coleman   20 APR 94
C****************************************************************
C
  	subroutine apnd_stat( out_buffer, status, rname )

c***    hard code string sizes to insure "len" function works when this
c***    routine is called by "C"
c***    calling routine must size "rname" at least "* 15"

        character rname * 15
        integer status

        include 'ipfinc/parametr.inc'
        character out_buffer * (MAXBUFFER)                     

        include 'ipfinc/pfstates.inc'
        include 'ipfinc/prt.inc'
        include 'ipfinc/errmsg.inc'
        include 'ipfinc/errorx.inc'

        character * 1    null, linefeed
        character * 120  text
        integer  o2

        integer  apdoutbuf

        null = char(0)
        linefeed = char(10)
        max_buf = len( out_buffer )

        lc = index( rname, null )
        if ( lc .eq. 1 ) then
           rname = 'cmdprs.f'
           lc = lastch( rname )
        else if (lc .eq. 0 ) then
           lc = lastch( rname )
        else
           lc = lc - 1
        endif

        if ( status .gt. 99  .or.  status .lt. -9 ) then
           write( errbuf(1), 111 ) rname(1:lc), status
  111      format(1x, a, ': out of range return status =', i15 )
           call prterx ('E', 1)
           status = 1
        endif

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
c 	Append summary
c
        if ( o2 .gt. max_buf - 50 ) then
           o2 = max_buf - 50
           do while ( o2 .gt. 1  .and.
     &                out_buffer(o2:o2) .ne. linefeed )
              o2 = o2 - 1
           enddo
           out_buffer(o2:o2) = null
        endif

        write (text, 211) rname(1:lc), status, ostates
  211   format ('/', a, ' return status: ', i2, ' IPF state: ', i2)
        length = apdoutbuf(o2, text, out_buffer(o2:))
        o2 = o2 + length
c
c       Reset error flag
c
        call setercnt (0, ' ')
        numerr = 0          ! reinitialize error count
        return
	end
