C    @(#)p_newbse.f	20.5 8/20/98
C****************************************************************
C
C   File: p_newbse.f
C   Purpose: IPF shell program to process /NEW_BASE commands
C
C   Author: Walt Powell  Date: 20 February 1992 
C                        Modified: 20 February 1992
C   Called by: 
C
C****************************************************************
C
	integer function p_newbse (in_buffer, out_buffer) 

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
        integer apdoutbuf, o1, o2, findstr, olderr, chkerr

        p_newbse = 0     ! default return SUCCESS state
        numerr = 0       ! reinitialize error count
        null = char(0)
        linefeed = char(10)
        out_buffer(1:1) = null

        if (ostates .lt. 5) then
           p_newbse = 1
           write (errbuf(1), 148)
  148      format(' Unsolved base data in residence')
           call prterx ('W', 1)
        else
           call ctlpow
           buf = inrcd
           if ( kspare(14) .eq. 1 ) then
c
c             DATAO file opened
c
              call wrdtao
              call prtime('WRITE_NEWBASE')
              if ( ostates .le. 6 ) then
                 ostates = 6
              else
                 ostates = 8
              endif
           else                         ! DATAO file could not be opened
              p_newbse = 1
           endif
           call close_file (datao)
           call loadarcv
        endif
        last = index (out_buffer,null)
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

        write (text, 340) 'p_newbse.f', p_newbse, ostates
  340   format ('/', a, ' return status: ', i2, ' IPF state: ', i2)
        length = apdoutbuf(o2, text, out_buffer(o2:))
        o2 = o2 + length
c
c       Reset error flag
c
        call setercnt (0, ' ')
        return
	end
