C    %W% %G%
C****************************************************************
C
C   File: p_chgbty.f
C   Purpose: IPF shell program to process /CHANGE_BUS_TYPE commands
C
C   Author: Walt Powell  Date: 29 November 1992 
C                        Modified: 
C   Called by: pf_proc, p_commnd
C
C****************************************************************
C
	integer function p_chgbty (in_buffer, out_buffer) 

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

        common /bldtbl/ bldtbl, fltstr
        logical bldtbl
        integer fltstr

        common /is_batch / is_batch       ! 0 = interactive, 1 = batch

        character null * 1, linefeed * 1, text * 80, zonex(100)*2
        integer o2, apdoutbuf, olderr, chkerr

        null = char(0)
        linefeed = char(10)
        p_chgbty = 0     ! default return SUCCESS state
        numerr = 0       ! reinitialize error count
        out_buffer(1:1) = null

        if (ostates .le. 1) then
           p_chgbty = 1
           write (errbuf(1), 138)
  138      format(' No base data in residence')
           call prterx ('W', 1)
        else
c
c          Determine if 'F' errors possibly corrected
c
           if ( is_batch .eq. 0 ) then
              olderr = chkerr('E')
           else
              olderr = chkerr('F')
           endif

           if (bldtbl) then
C
C             BTABLE must be called prior to HOTCHG since
C             system changes have been made and all array must
C             be updated.
C
              fltstr = 0
              call btable (fltstr, .false.)
              call prtime('build_table')
C
C             Process any EPRI_1964 data.
C
              if (nepbus + nepctl .gt. 0) then
                 call lodepr(zonex, numznx)
                 call prtime('build_epri_1964_tables')
              else
                 numznx = 0
              endif
C
C             Zones declared in d-c are trasnferred to BLDTIE.
C
              call bldtie(zonex, numznx)
              call prtime('build_area_zone_tables')
              bldtbl = .false.
              ostates = 3

           endif
           call hotchg
           call prtime('change_bus_type')
           call loadarcv
C
C          Set "REBUILD" switch on to force BTABLE.
C          Set "FLTSTR" switch off if unassigned. FLTSTR = 0
C          preserves voltages for hot start.
C
           bldtbl = .true.
           if (fltstr .eq. -1) fltstr = 0
           kspare(1) = 1
           inrcd = buf
           if (index ('/HCS',inrcd(1:1)) .ne. 0) call ctlpow
           buf = inrcd
           if ( is_batch .eq. 0 ) then
              newerr = chkerr('E')
           else
              newerr = chkerr('F')
           endif
           if (olderr .ge. newerr .and. olderr .gt. 0) then
              if ( is_batch .eq. 0 ) then
                 errcnt(3) = 0
              else
                 errcnt(4) = 0
              endif
              write (errbuf(1), 139)
  139         format(' Changes have reset fatal error flag')
              call prterx ('W', 1)
           endif
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
        write (text, 340) 'p_chgbty.f', p_chgbty, ostates
  340   format ('/', a, ' return status: ', i2, ' IPF state: ', i2)
        length = apdoutbuf(o2, text, out_buffer(o2:))
        o2 = o2 + length
c
c       Reset error flag
c
        call setercnt (0, ' ')
        return
	end
