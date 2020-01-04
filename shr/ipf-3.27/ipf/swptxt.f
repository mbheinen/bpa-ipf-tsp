C    @(#)swptxt.f	20.3 2/13/96
        subroutine swptxt(m,n)
C
        include 'ipfinc/parametr.inc'
        include 'ipfinc/optim1.inc'

c	Global variables used:
c		txtie(r*8)
C
        double precision xtemp
c
        do i=1,10
           xtemp = txtie(i,m)
           txtie(i,m) = txtie(i,n)
           txtie(i,n) = xtemp
        enddo
        return
        end
