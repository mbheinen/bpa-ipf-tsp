C    @(#)erexit.f	20.3 2/13/96
      subroutine erexit    !  Error exit job processing

      include 'ipfinc/parametr.inc'

      include 'ipfinc/blank.inc'
      include 'ipfinc/jobctl.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/prt.inc'
 
      character job*30
      save
C
      if (batch) then
         call proend
         job = 'FINIS PWRFLW ' // prgvsn
         job(21:30) = chase1(1)
         call prtime('POWERFLOW')
C
         if ( kspare(16) .eq. -1 ) then
              close ( unit = mfich, status = 'DELETE' )
         else
              close ( unit = mfich, status = 'KEEP' )
         endif
 
         if ( kspare(17) .eq. 0 ) then
              close ( unit = dbug, status = 'DELETE' )
         else
              close ( unit = dbug, status = 'KEEP' )
         endif
C
         write(errbuf(1),100)
  100      format ('0 Program terminated by error conditions. ',
     1          'If "F" errors have been encountered, ',
     2          'correct and rerun.')
         write (errbuf(2),110)
  110    format ('  Otherwise report to programming staff.')
         call prterx ('F',2)
c
c        call "C" routine to provide an error status on exit
c
         call c_err_exit
      endif
C
C     This entry is similar to EREXIT except it is called from
C     PRTERX and cannot recursively recall PRTERX as is done in
C     EREXIT
C
      entry erquit

      if (batch) then
         call proend
         job = 'FINIS PWRFLW '//prgvsn
         job(21:30) = chase1(1)
         call prtime('POWERFLOW')
C
         if ( kspare(16) .eq. -1 ) then
            close ( unit = mfich, status = 'DELETE' )
         else
            close ( unit = mfich, status = 'KEEP' )
         endif
 
         if ( kspare(17) .eq. 0 ) then
            close ( unit = dbug, status = 'DELETE' )
         else
            close ( unit = dbug, status = 'KEEP' )
         endif
C
c        force and error exit/trace back if possible
c
         y = abs(1.0)
         x = abs(0.0)
         x = y / x
         stop 'errexit'
      endif
C
      end
