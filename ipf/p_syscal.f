C    @(#)p_syscal.f	20.5 8/20/98
C****************************************************************
C
C   File: p_syscal.f
C   Purpose: IPF shell program to process system calls
C
C   Author: Dave Stefonek Date: 20 February 1992
C   Called by:
C
C****************************************************************
C
	integer function p_syscal (in_buffer, out_buffer)

        character in_buffer*(*)
        character out_buffer*(*)

        include 'ipfinc/parametr.inc'
        include 'ipfinc/blank.inc'
        include 'ipfinc/jobctl.inc'
        include 'ipfinc/lfiles.inc'
        include 'ipfinc/filnam.inc'
        include 'ipfinc/errorsw.inc'
        include 'ipfinc/pfstates.inc'
        include 'ipfinc/prt.inc'
        include 'ipfinc/errmsg.inc'
        include 'ipfinc/errorx.inc'

        character null * 1, linefeed * 1, text * 80, word(10)*256
        integer apdoutbuf, o2

        p_syscal = 0     ! default return SUCCESS state
        numerr = 0       ! reinitialize error count
        null = char(0)
        linefeed = char(10)
        out_buffer(1:1) = null

        i1 = index(in_buffer, linefeed) + 1
        i2 = nxt_term (in_buffer(i1+1:)) + i1 
        word(1) = in_buffer(i1:i2)
        word(1)(i2-i1+1:i2-i1+1) = null
********************** debug stuff ***********************
        print 9931, i1, i2, in_buffer
 9931   format(' in p_syscal i1, i2, in_buffer(1:60) = '
     1,2i3, a60)
********************** debug stuff ***********************
c********* call system with second line of buffer **********
c old way dc        numerr = calsys (word(1))
        call calsys (word(1))

******************** debug stuff **************************
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
        write (text, 340) 'p_syscal.f', p_syscal, ostates
  340   format ('/', a, ' return status: ', i2, ' IPF state: ', i2)
        length = apdoutbuf(o2, text, out_buffer(o2:))
        o2 = o2 + length
c
c       Reset error flag
c
        call setercnt (0, ' ')
        return
	end
