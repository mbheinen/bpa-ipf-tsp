C    %W% %G%
C****************************************************************
C
C   File: p_gtnetdat.f
C   Purpose: IPF shell program to process /NETWORK_DATA commands
C
C   Author: Walt Powell  Date: 27 February 1992 
C                        Modified: 27 February 1992
C   Called by: 
C
C****************************************************************
C
	integer function p_gtnetdat (in_buffer, out_buffer) 

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
        include 'ipfinc/mrgtxt.inc'

        character null * 1, linefeed * 1, text * 80
	integer apdoutbuf, o1, o2, findstr, olderr, chkerr, error,
     &          bufsize

        bufsize = len (out_buffer)

        call baseinit

        call ctlpow

        null = char(0)
        linefeed = char(10)
        p_gtnetdat = 0  ! default return SUCCESS state
        numerr = 0       ! reinitialize error count
        out_buffer(1:1) = null

        nbsmrg = 0
        nbrmrg = 0
        nifmrg = 0
        kspare(1) = 0    ! flag for building case from scratch
        call close_file (logmbs)
        call close_file (logmbr)
        open (logmbs,status='scratch')
        open (logmbr,status='scratch')
        endfile logmbs
        endfile logmbr
        rewind logmbs
        rewind logmbr

c
c*** Reset error flag
c
        call setercnt (0, ' ')
        buf = inrcd
        card = buf(1:1)
        call bsread
        olderr = chkerr('E') + chkerr('F')
        if (olderr .eq. 0) then
           call cmpltbus(error)
           call prtime('bus_read')
           call sortbus
           call sortcbus
           call prtime('bus_sort')
           rewind logmbr
           call brread
           call prtime('branch_read_and_sort')
           olderr = chkerr('E') + chkerr('F')
           if (olderr .eq. 0) then
              call btable (1, .false.)
              call prtime('build_table')
              olderr = chkerr('E') + chkerr('F')
              if (olderr .eq. 0 .or. 
     &           (kbsknt .gt. 0 .and. kbrknt .gt. 0)) then
c
c***          All base data ready (maybe with some errors!)
c
                 kspare(1) = 2
                 jobreq(3) = ' '
                 ostates = 2
              else
                 p_gtnetdat = 1
              endif
           else
              p_gtnetdat = 1
           endif
        else
           p_gtnetdat = 1
        endif
        inrcd = buf
        call ctlpow
        buf = inrcd
        call close_file (logmbs)
        call close_file (logmbr)
        call loadarcv

C*************************************************************
C***    else if (ostates .eq. 5) then
C***       p_gtnetdat = 1
C***       write (errbuf(1), 136)
C*136      format(' Loading new base case clobbered [unsaved, ',
C*** &            ' solved] base data in residence')
C***       call prterx ('W', 1)
C*************************************************************

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
c
c       Append summary
c
        if ( o2 .gt. bufsize - 50 ) then
           o2 = bufsize - 50
           do while ( o2 .gt. 1  .and.
     &                out_buffer(o2:o2) .ne. linefeed )
              o2 = o2 - 1
           enddo
           out_buffer(o2:o2) = null
        endif
        write (text, 340) 'p_gtnetdat.f', p_gtnetdat, ostates
  340   format ('/', a, ' return status: ', i2, ' IPF state: ', i2)
        length = apdoutbuf(o2, text, out_buffer(o2:))
        o2 = o2 + length
c
c       Reset error flag
c
        call setercnt (0, ' ')
        return
	end
