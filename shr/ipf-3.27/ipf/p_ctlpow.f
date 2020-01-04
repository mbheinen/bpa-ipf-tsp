C    @(#)p_ctlpow.f	20.5 8/20/98
C****************************************************************
C
C   File: p_ctlpow.f
C   Purpose: IPF shell program to process miscellaneous commands
C
C   Author: Walt Powell  Date: 29 November 1992 
C                        Modified: 
C   Called by: pf_proc, p_commnd
C
C****************************************************************
C
	integer function p_ctlpow (in_buffer, out_buffer) 

        character in_buffer*(*)
        character out_buffer*(*)

        include 'ipfinc/parametr.inc'
        include 'ipfinc/blank.inc'
        include 'ipfinc/lfiles.inc'
        include 'ipfinc/filnam.inc'
        include 'ipfinc/jobctl.inc'
        include 'ipfinc/dtaiop.inc'
        include 'ipfinc/changr.inc'
        include 'ipfinc/delete.inc'
        include 'ipfinc/epridc.inc'
        include 'ipfinc/errorsw.inc'
        include 'ipfinc/pfstates.inc'
        include 'ipfinc/prt.inc'
        include 'ipfinc/errmsg.inc'
        include 'ipfinc/errorx.inc'


        character null * 1, linefeed * 1, text * 80
        integer o2, apdoutbuf

        null = char(0)
        linefeed = char(10)
        p_ctlpow = 0     ! default return SUCCESS state
        numerr = 0       ! reinitialize error count
        out_buffer(1:1) = null

        buf = inrcd
        call ctlpow
        buf = inrcd
        call loadarcv
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
        write (text, 340) 'p_ctlpow.f', p_ctlpow, ostates
  340   format ('/', a, ' return status: ', i2, ' IPF state: ', i2)
        length = apdoutbuf(o2, text, out_buffer(o2:))
        o2 = o2 + length
c
c       Reset error flag
c
        call setercnt (0, ' ')
        return
	end
