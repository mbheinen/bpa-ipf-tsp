C    @(#)procse.f	20.3 2/13/96
        subroutine procse
C
C       PROCESS-A-CASE
C              (1,C)
C
        include 'ipfinc/jobctl.inc'
C
        character chtemp*8
        integer chkerr
C
        call probeg
        call prtime('PROC-BEGIN')
C
        iercnt  = chkerr('F') + chkerr('A')
        if ( iercnt  .gt. 0 ) then
           endjob = .true.
        else
           call proces
        endif
C
        call proend
        chtemp = 'PROC_END'
        call prtime(chtemp)
C
        return
        end
