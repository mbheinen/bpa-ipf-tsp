C    @(#)gtoutage.f	20.3 2/13/96
C****************************************************************
C
C   File: gtoutage.f
C   Purpose: Routine to obtain BCD image of outaged data 
C
C   Author: Walt Powell  Date: 20 February 1992
C                        Modified: 20 February 1992
C   Called by:
C
C****************************************************************
C
        subroutine gtoutage (in_buffer, out_buffer)
        character in_buffer *(*), out_buffer *(*)

        include 'ipfinc/parametr.inc'
      	include 'ipfinc/blank.inc'
      	include 'ipfinc/changr.inc'
      	include 'ipfinc/lfiles.inc'
      	include 'ipfinc/delete.inc'

	character null * 1, linefeed * 1
        integer o2, apdoutbuf

        maxbuf_out = len( out_buffer ) - 400
        null = char(0)
        linefeed = char(10)

        out_buffer(1:1) = null
        o2 = index (out_buffer,null)

        ic = 1
        do while (ic .le. ndelete .and. o2 .le. maxbuf_out)
           if (delete(ic)(3:3) .ne. '*') then
              length = apdoutbuf(o2, delete(ic)(1:92), out_buffer(o2:))
              o2 = o2 + length
           endif
           ic = ic + 1
        enddo

c*** remember maxbuf_out is really 400 less than the real buffer size
        if (o2 .gt. maxbuf_out) then
           write (out_buffer(o2:o2+8), 820) linefeed, null
  820      format (a, '*[MORE]', a)
           o2 = o2 + 9
        endif

  900   continue
        return
        end
