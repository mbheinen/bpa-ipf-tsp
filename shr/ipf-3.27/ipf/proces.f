C    @(#)proces.f	20.3 2/13/96
      subroutine proces
C
C     Process base data
C
      include 'ipfinc/jobctl.inc'
 
      integer chkerr
C
C     Define the initial base system.
C
      call dfnbse
      call prtime('DEFINE-BASE')
      iercnt = chkerr('F') + chkerr('A')
      if ( iercnt .gt. 0 ) then
C
C        If errors, abort.
C
         endjob = .true.
      else
C
C        Else process the system data
C
         call prodat
      endif
      return
      end
