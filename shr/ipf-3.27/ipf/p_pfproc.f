C    @(#)p_pfproc.f	20.5 1/7/99
C****************************************************************
C
C   File: p_pfproc.f
C   Purpose: IPF shell program to process anY IPF command
C
C   Author: Walt Powell  Date: 20 February 1992
C                        Modified: 20 February 1992
C   Called by:
C
C****************************************************************
C
	integer function p_pfproc (in_buffer, out_buffer) 

        include 'ipfinc/parametr.inc'

        character in_buffer * (MAXBUFFER)
        character out_buffer * (MAXBUFFER)

        include 'ipfinc/blank.inc'
        include 'ipfinc/jobctl.inc'
        include 'ipfinc/lfiles.inc'
        include 'ipfinc/filnam.inc'
        include 'ipfinc/errorsw.inc'
        include 'ipfinc/pfstates.inc'
        include 'ipfinc/prt.inc'
        include 'ipfinc/errmsg.inc'
        include 'ipfinc/errorx.inc'

        character null * 1, linefeed * 1, text * 80
        character word(50) * 60
        integer apdoutbuf, o2

        p_pfproc = 0     ! default return SUCCESS state
        numerr = 0       ! reinitialize error count
        null = char(0)
        linefeed = char(10)
        out_buffer(1:1) = null

        i1 = 1

c	Parse entire in_buffer
c
        i1 = 1
        i2 = index (in_buffer, null)
        call uscan (in_buffer(i1:i2), word, nwrd, '=', ', ' // linefeed)
        word(nwrd+1) = null

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
c 	Append summary
c
        write (text, 340) 'p_pfproc.f', p_pfproc, ostates
  340   format ('/', a, ' return status: ', i2, ' IPF state: ', i2)
        length = apdoutbuf(o2, text, out_buffer(o2:))
        o2 = o2 + length
c
c       Reset error flag
c
        call setercnt (0, ' ')
        return
	end

