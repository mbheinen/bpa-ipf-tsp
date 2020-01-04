C    @(#)fxrxrt.f	20.3 2/13/96
        subroutine fxrxrt (y,v,tol)
        complex * 16 y(2,2),v(2)
        real tol
C
C       This subroutine changes the R/X ratio (or more precisely, the
C       G/B ratio) to accomodate the restrictions on the decoupled
C       solution routine.
c
c	Changed to accept double precision arguments.
C
C       "Acceptable" G/B values are:
C
C       1. CDABS (Y) .GT. TOL, and
C       2. Compensation occurs for G/B < -0.50 or G/B > 0.25.
C
C       Y (new) is computed such that V and A are preserved.  Fictitious
C       shunts are employed. If the branch admittance is less than TOL,
C       the branch is replaced entirely with shunt.
C
        complex * 16 a(2)
        double precision yb, yg, ratio
        integer i, j
C
C       Check G/B ratio of equivalent branches
C
        do i = 1,2
           a(i) = 0.0d0
           do j = 1,2
              a(i) = a(i) + y(i,j)*v(j)
           enddo
        enddo
        yb = dimag (y(1,2))
        yg = dreal (y(1,2))
        if (cdabs (y(1,2)) .le. tol) then
           y(1,2) = 0.0d0
           y(2,1) = 0.0d0
        else if (yb .ne. 0.0d0) then
           ratio = yg / yb
           if (ratio .le. -0.50d0 .or. ratio .ge. 0.25d0) then
              y(1,2) = dcmplx (0.0d0,yb)
              y(2,1) = y(1,2)
           endif
        else
           y(1,2) = dcmplx (0.0d0,yg)
           y(2,1) = y(1,2)
        endif
        y(1,1) = (a(1) - y(1,2)*v(2)) / v(1)
        y(2,2) = (a(2) - y(2,1)*v(1)) / v(2)
  120   continue
        return
        end
