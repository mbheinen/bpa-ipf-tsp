C    @(#)swpymx.f	20.3 2/13/96
        subroutine swpymx (m,n)
C
        include 'ipfinc/parametr.inc'
        include 'ipfinc/red7.inc'

        double precision xtemp
C
        do i = 1,3
           xtemp = ymtrx(i,m)
           ymtrx(i,m) = ymtrx(i,n)
           ymtrx(i,n) = xtemp
        enddo
        return
        end
