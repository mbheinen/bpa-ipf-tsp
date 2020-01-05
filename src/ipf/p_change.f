C    @(#)p_change.f	20.6 1/7/99
C****************************************************************
C
C   File: p_change.f
C   Purpose: IPF shell program to process /CHANGES commands
C
C   Author: Walt Powell  Date: 20 February 1992 
C                        Modified: 20 February 1992
C   Called by: 
C
C****************************************************************
C
	integer function p_change (in_buffer, out_buffer) 

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
        include 'ipfinc/errorsw.inc'
        include 'ipfinc/pfstates.inc'
        include 'ipfinc/prt.inc'
        include 'ipfinc/errmsg.inc'

        common /bldtbl/ bldtbl, fltstr
        logical bldtbl
        integer fltstr

        character null * 1, linefeed * 1, text * 80
        integer o1, o2, apdoutbuf, chkerr, bufsize

        null = char(0)
        linefeed = char(10)
        bufsize = len(out_buffer)
        out_buffer(1:1) = null
        p_change = 0     ! default return SUCCESS state
        numerr = 0       ! reinitialize error count

        if (ostates .lt. 2) then
c
c          No base data in residence - check if first change is a
c          bus addition.
c
           o1 = 1
           do while (o1 .lt. bufsize .and. 
     &               in_buffer(o1:o1) .ne. null)
              o2 = nxt_term(in_buffer(o1+1:)) + o1
              if (index ('AIB+XQLERT', in_buffer(o1:o1)) .gt. 0) then
                 if (in_buffer(o1:o1) .eq. 'B' .and. 
     &               in_buffer(o1+2:o1+2) .eq. ' ') ostates = 2
                 o1 = bufsize + 1
              else
                 o1 = o2 + 1
              endif
           enddo
        endif
        if (ostates .lt. 2) then
           p_change = 1
           write (errbuf(1), 138)
  138      format(' No base data in residence')
           call prterx ('W', 1)
        endif
        if (ostates .gt. 1) then
c
c          Reset error flag
c
           call setercnt (0, ' ')
           call ctlpow
           buf = inrcd
           call change
           call prtime('CHANGES')
           call loadarcv
           inrcd = buf
           bldtbl = .true.
           ostates = 2
           ierror = chkerr('E') + chkerr('F') + chkerr('A') 
           if ( ierror .ne. 0 ) p_change = 1
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
        if ( o2 .gt. bufsize - 50 ) then
           o2 = bufsize - 50
           do while ( o2 .gt. 1  .and.
     &                out_buffer(o2:o2) .ne. linefeed )
              o2 = o2 - 1
           enddo
           out_buffer(o2:o2) = null
        endif
        write (text, 340) 'p_change.f', p_change, ostates
  340   format ('/', a, ' return status: ', i2, ' IPF state: ', i2)
        length = apdoutbuf(o2, text, out_buffer(o2:))
        o2 = o2 + length
c
c       Reset error flag
c
        call setercnt (0, ' ')
        return
	end
