C    %W% %G%
C****************************************************************
C
C   File: p_reduct.f
C   Purpose: IPF shell program to process /REDUCTION commands
C
C   Author: Walt Powell  Date: 20 February 1992 
C                        Modified: 20 February 1992
C   Called by: 
C
C****************************************************************
C
	integer function p_reduct (in_buffer, out_buffer) 

        character in_buffer*(*)
        character out_buffer*(*)

        include 'ipfinc/parametr.inc'

        include 'ipfinc/blank.inc'
        include 'ipfinc/lfiles.inc'
        include 'ipfinc/filnam.inc'
        include 'ipfinc/jobctl.inc'
        include 'ipfinc/dtaiop.inc'
        include 'ipfinc/changr.inc'
        include 'ipfinc/ecvar.inc'
        include 'ipfinc/epridc.inc'
        include 'ipfinc/errorsw.inc'
        include 'ipfinc/pfstates.inc'
        include 'ipfinc/prt.inc'
        include 'ipfinc/errmsg.inc'
        include 'ipfinc/errorx.inc'

        common /bldtbl/ bldtbl, fltstr
        logical bldtbl
        integer fltstr, status, error

        character null * 1, linefeed * 1 
        integer chkerr, apdoutbuf, o2
        character text * 120, zonex(100)*2

        p_reduct = 0     ! default return SUCCESS state
        numerr = 0       ! reinitialize error count
        null = char(0)
        linefeed = char(10)
        out_buffer(1:1) = null
        
        if (ostates .le. 1) then
           p_reduct = 1
           write (errbuf(1), 140)
  140      format(' No base data in residence')
           call prterx ('W', 1)
        else
           istate = 0
           do while ( chkerr('E') + chkerr('F') .eq. 0 .and.
     &                istate .le. 4 )
              if (istate .eq. 0) then
                 call reduct
                 call prtime( 'REDUCTION' )
                 inrcd = buf
                 if (index ('/HCS',inrcd(1:1)) .ne. 0) call ctlpow
                 buf = inrcd
                 kspare(1) = 1
                 istate = 1
              else if (istate .eq. 1) then
                 call bsread
                 call cmpltbus(error)
                 call prtime('BUS_READ')
                 call sortbus
                 call sortcbus
                 call prtime('BUS_SORT')
                 istate = 2
              else if (istate .eq. 2) then
                 call brread
                 call prtime('BRANCH_READ_SORT')
                 istate = 3
              else if (istate .eq. 3) then
                 fltstr = 0
                 call btable (fltstr, .false.)
                 call prtime('BUILD_TABLE')
                 istate = 4
                 kspare(1) = 2
              else if (istate .eq. 4) then
C
C                Process any EPRI_1964 data.
C
                 if (nepbus + nepctl .gt. 0) then
                    call lodepr(zonex, numznx)
                    call prtime('BUILD_EPRI_1964_TABLES')
                 else
                    numznx = 0
                 endif
C
C                Zones declared in d-c are trasnferred to BLDTIE.
C
                 call bldtie(zonex, numznx)
                 call prtime('BUILD_AREA_ZONE_TABLES')
                 inrcd = buf
                 istate = 5
              endif
           enddo
           if ( chkerr('E') + chkerr('F') .eq. 0 ) then
              ostates = 3
           else
              p_reduct = 1
           endif
           call loadarcv

        endif

        o2 = index (out_buffer,null)
c
c       Append error messages to buffer
c
        j = 1 
        length = 1
        do while (j .le. numerr .and. length .gt. 0)
           length = apdoutbuf(o2, errm(j), out_buffer(o2:))
           o2 = o2 + length
           j = j + 1
        enddo
c
c 	Append summary
c
        write (text, 340) 'p_reduct.f', p_reduct, ostates
  340   format ('/', a, ' return status: ', i2, ' IPF state: ', i2)
        length = apdoutbuf(o2, text, out_buffer(o2:))
        o2 = o2 + length
c
c       Reset error flag
c
  900   call setercnt (0, ' ')
        return
	end
