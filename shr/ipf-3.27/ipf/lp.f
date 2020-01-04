C    @(#)lp.f	20.3 2/13/96
        subroutine lp
C
C       This LP routine is copied from the book by A. Land and S. Powell
C       "Fortran Codes for Mathematical Programming", 1978, John Wiley
C       and Sons, N.Y.
C
 
      include 'ipfinc/parametr.inc'
 
      include 'ipfinc/smallp.inc'


        itr = 0
        ir = 0
        if (istate .eq. 10) inrev = 1
   10   call doanlp
        if (istate .gt. 3) go to 20
        call chacc
        if (istate.ne.7) go to 20
        if (ir.ge.irmax) go to 20
        call revert
        istate = 11
        go to 10

   20   return
        end
