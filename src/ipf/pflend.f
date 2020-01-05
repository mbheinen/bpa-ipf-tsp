C    @(#)pflend.f	20.3 2/13/96
      subroutine pflend
C
C       Powerflow_end job processing
C
C       Added case ID to PRGMON call - AHS 4/22/82
 
      include 'ipfinc/blank.inc'
      include 'ipfinc/jobctl.inc'
      include 'ipfinc/lfiles.inc'
      include 'ipfinc/usranl.inc'
 
      character job*30
 
      job = 'FINIS PWRFLW '//prgvsn
 
C     Add the case ID to the job label
 
      job(21:30) = chase1(1)
      call prtime('POWERFLOW')
C
C
      if ( kspare(16) .eq. -1 ) then
           close ( unit = mfich, status = 'DELETE' )
      else
           close ( unit = mfich, status = 'KEEP' )
      endif
 
      if ( kspare(17) .eq. 0 .and. batch) then
           close ( unit = dbug, status = 'DELETE' )
      else
           close ( unit = dbug, status = 'KEEP' )
      endif
 
      if (numusr .gt. 0 .and. lunusr .gt. 0) then
          close (unit = lunusr, status = 'DELETE')
      endif
C
      return
      end
