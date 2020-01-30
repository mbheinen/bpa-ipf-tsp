C    @(#)mrgbse.f	20.6 11/12/98
        subroutine mrgbse (subsys)
 
        include 'ipfinc/parametr.inc'

        include 'ipfinc/blank.inc'
        include 'ipfinc/bus.inc'
        include 'ipfinc/data.inc'
        include 'ipfinc/filnam.inc'
        include 'ipfinc/jobctl.inc'
        include 'ipfinc/lfiles.inc'
        include 'ipfinc/merge.inc'
        include 'ipfinc/mrgtxt.inc'
        include 'ipfinc/xdata.inc'
        include 'ipfinc/pqcurves.inc'
        include 'ipfinc/prt.inc'
C
        integer subsys
        character case*10
        logical baseok
C
        save
C
C       CONTENTS OF "COMVAR"
C
C       COMVAR(1) - NUMBER OF SUBSYSTEMS
C             (2) - NAME OF SUBSYSTEM 1
C             (3) - NAME OF SUBSYSTEM 2
C             (4) - NUMBER OF BUSES IN SUBSYSTEM 1
C             (5) - NUMBER OF BUSES IN SUBSYSTEM 2
C             (6) - COUNT OF ENCODED BUS TEXT
C             (7) - COUNT OF ENCODED BRANCH TEXT
C             (8) - COUNT OF ENCODED INTERFACE TEXT
C             (9) - INPUT SWITCH TO SAVE AREA INTERCHANGE SYSTEM
C            (10) - RESERVED FOR DEBUG
C
        call dbgprt (1)
C
C       INITIALIZE PARAMETERS
C
        jobreq(1) = ' '
        if (subsys .eq. 1) then
           kexit = 0
           kerrsw = 0
           do i = 1,10
              comcha(i)= ' '
              comvar(i) = 0
           enddo
           call prntxt ()
        endif
        comvar(1) = subsys
C
C       Save subsys number
C
        if (subsys.gt.2) then
           write (errbuf(1),122)
  122      format ('0 FATAL ERROR - MORE THAN TWO MERGE BASE',
     1            ' CASES PROCESSED.')
           call prterx ('F',1)
           kexit = 1
           subsys = 2
           comvar(1) = 2
        endif
        comcha(subsys+1) = cspare(38)
C
C       Turn Area Int switch off
C
        comvar(9) = 0
C
C       Load base data if requested
C
        if (jobreq(2) .eq. 'OLD_BASE') then
           case = crun1(3)
           baseok = .false.
           call gtbase (obasnm, case, baseok)
C
           if ( .not.baseok ) then
               write (errbuf(1),136) case,obasnm
  136          format (' CASE ',a10,' IS NOT IN OLD_BASE FILE ',
     1                              a30 )
               call prterx ('F',1)
               kexit = 1
               go to 900
           endif
           jobreq(2) = ' '
           xdt_flag = .false.
           pq_flag = .false.
C
        endif
C
C       PROCESS TEXT COMMANDS
C
        call mergtx ()
        if (kerrsw .gt. 0) then
           kexit = 1
           do while (index('(/',card) .eq. 0 )
              call readtx ()
              inptls = 1
           enddo
           goto 900
        endif
C
C       EXECUTE REMAINING MERGE COMMANDS
C
        call proctx ()
        if (kerrsw.gt.0) then
           kexit = 1
           do while (index('(',card) .eq. 0 ) 
              call readtx ()
              inptls = 1
           enddo
           goto 900
        endif
C
C       ENCODE SUBSYSTEM DATA
C
        call writxt ()
        comvar(subsys+3) = ntot
C
C       END SUBSUSTEM LOOP
C
  900   if (kexit .ne. 0) then
C
C          ERROR EXIT
C
           write (errbuf(1),920)
  920      format('   MERGE RUN ABORTED BY ERRORS ')
           call prterx ('F',1)
           jobreq(1)='QUIT'
           buf = '( STOP  - ABORT BY MERGE_BASE )'
        endif
        call dbgprt (0)
        return
        end
