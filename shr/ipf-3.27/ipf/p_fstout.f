C    @(#)p_fstout.f	20.4 7/18/96
C****************************************************************
C
C   File: p_fstout.f
C   Purpose: IPF shell program to process /OUTAGE_SIMULATION commands
C
C   Author: Walt Powell  Date: 15 May 1996
C                        Modified: 15 May 1996
C
C   Called by: srv_fcmdprs.f
C
C****************************************************************
C
	integer function p_fstout( in_buffer, out_buffer ) 

        character in_buffer * (*)
        character out_buffer * (*)

        include 'ipfinc/parametr.inc'

        include 'ipfinc/errmsg.inc'
        include 'ipfinc/pfstates.inc'
        include 'ipfinc/blank.inc'
        include 'ipfinc/lfiles.inc'
        include 'ipfinc/filnam.inc'
        include 'ipfinc/jobctl.inc'
        include 'ipfinc/dtaiop.inc'
        include 'ipfinc/changr.inc'
        include 'ipfinc/ecvar.inc'
        include 'ipfinc/epridc.inc'
        include 'ipfinc/prt.inc'

        common /bldtbl/ bldtbl, fltstr
        logical bldtbl
        integer fltstr, reordr, status

        common /is_batch / is_batch       ! 0 = interactive, 1 = batch

        integer chkerr

        character * 1  null, linefeed
        integer   apdoutbuf, o2
        character * 50  stext

        max_buf = len( out_buffer ) - len( stext ) - 10
        numerr = 0       ! reinitialize error count
        null = char(0)
        linefeed = char(10)
        out_buffer(1:1) = null
        o2 = 1
        
        p_fstout = 0

        buf = inrcd
        if (ostates .gt. 1 .and. ostates .lt. 5) then
           write (errbuf(1), 150)
  150      format(' Base data in residence has not been solved')
           call prterx ('W', 1)
           p_fstout = 1
           goto 900
        else if (ostates .eq. 5 .and. numchg .gt. 0) then
           write (errbuf(1), 160)
  160      format(' Base data in residence will be permanently altered.'
     &)
           write (errbuf(2), 170)
  170      format(' Save base data in residence onto history data file b
     &efore proceeding')
           call prterx ('W', 2)
           p_fstout = 1
           goto 900
        else if (ostates .eq. 7 .or. ostates .eq. 8) then
           write (errbuf(1), 180)
  180      format(' Base data in residence has diverged solution')
           call prterx ('W', 1)
           p_fstout = 1
           goto 900
        else
           call fstout ()
           ostates = 2          ! Set flag to rebuild data in residence
        endif

  900   if ( is_batch .eq. 0 ) then
          if (chkerr('E') .eq. 0) p_fstout = 0
        else
          if (chkerr('F') .eq. 0) p_fstout = 0
        endif
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
        write (stext, 920) 'p_fstout.f', p_fstout, ostates
  920   format ('/', a, ' return status: ', i2, ' IPF state: ', i2)
        length = apdoutbuf( o2, stext, out_buffer(o2:) )
        o2 = o2 + length
c
c       Reset error flag
c
        call setercnt( 0, ' ' )
        return
	end
