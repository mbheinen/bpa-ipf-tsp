C    @(#)apdoutbuf.f	20.3 2/13/96
C****************************************************************
C
C   File: apdoutbuf
C   Purpose: This routine transfers a line from in_buf to out_buf,
C            separating the records with LINEFEED and maintaining 
C            NULL termination.
C
C   Author: Walt Powell  Date: 18 May 1992
C   Called by:
C
C****************************************************************
C
        integer function apdoutbuf(first, in_buf, out_buf)

        integer first
        character in_buf *(*), out_buf *(*)
        character linefeed * 1, null * 1

        linefeed = char(10)
        null = char(0)

        len1 = len(in_buf)
        len2 = len(out_buf) - 1
        ifirst = 1

        if (len1 .lt. len2) then
           if (first .gt. 1) then     ! Replace NULL with LINEFEED
              out_buf(ifirst:ifirst) = linefeed
              ifirst = ifirst + 1    
              apdoutbuf = len1 + 1
           else
              apdoutbuf = len1 
           endif
           out_buf(ifirst:ifirst+len1) = in_buf(1:len1) // null
        else
           apdoutbuf = 0
        endif
        return
        end
