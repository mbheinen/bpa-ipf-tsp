C    %W% %G%
C****************************************************************
C
C   File: p_cnvtld.f
C   Purpose: IPF shell program to process /%LOAD_DISTRIBUTION commands
C
C   Author: Walt Powell  Date: 14 December 1992 
C   Called by: p_commnd
C
C****************************************************************
C
	integer function p_cnvtld (in_buffer, out_buffer) 

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
        include 'ipfinc/epridc.inc'
        include 'ipfinc/errmsg.inc'
        include 'ipfinc/errorx.inc'
        include 'ipfinc/pfstates.inc'
        include 'ipfinc/prt.inc'

        common /bldtbl/ bldtbl, fltstr
        logical bldtbl
        integer fltstr

        common /is_batch / is_batch       ! 0 = interactive, 1 = batch

        character null * 1, linefeed * 1, text * 80, zonex(100)*2
        integer o2, apdoutbuf, chkerr

        null = char(0)
        linefeed = char(10)
        p_cnvtld = 0     ! default return SUCCESS state
        numerr = 0       ! reinitialize error count
        out_buffer(1:1) = null

        if (ostates .le. 1) then
           p_cnvtld = 1
           write (errbuf(1), 138)
  138      format(' No base data in residence')
           call prterx ('W', 1)
           go to 330
        endif

        if (ostates .eq. 2) then
C
C          BTABLE must be called prior to %LOAD_DISTRIBUTION since
C          system changes have been made and all array must
C          be updated.
C
           if (numchg .gt. 0 .or. kspare(1) .eq. 1) then
              if (fltstr .eq. -1) then
                 if (nadd .gt. 0 .or. ndel .gt. 0) then
                    fltstr = 1
                 else
                    fltstr = 0
                 endif
              endif
           else
              fltstr = 0
           endif
           call btable(fltstr, .false.)
C
C          Process any EPRI_1964 data.
C
           if (nepbus + nepctl .gt. 0) then
              call lodepr(zonex, numznx)
              call prtime('BUILD_EPRI_1964_TABLES')
           else
              numznx = 0
           endif
C
C          Zones declared in d-c are trasnferred to BLDTIE.
C
           call bldtie(zonex, numznx)
           call prtime('BUILD_AREA_ZONE_TABLES')
           inrcd = buf
           if ( is_batch .eq. 0 ) then
              if (chkerr('E') .eq. 0) ostates = 3
           else
              if (chkerr('F') .eq. 0) ostates = 3
           endif
        endif

        call cnvtld (ierror)
        call prtime('%LOAD_DISTRIBUTION')
        call loadarcv

        if (ierror .eq. 0) then
C
C          Set "REBUILD" switch on to force BTABLE.
C
           kspare(1) = 1
           ostates = 2
           inrcd = buf
        else
           p_cnvtld = 1
        endif
c
c       Append error messages to buffer
c
  330   j = 1 
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
        write (text, 340) 'p_cnvtld.f', p_cnvtld, ostates
  340   format ('/', a, ' return status: ', i2, ' IPF state: ', i2)
        length = apdoutbuf(o2, text, out_buffer(o2:))
        o2 = o2 + length
c
c       Reset error flag
c
        call setercnt (0, ' ')
        return
	end
