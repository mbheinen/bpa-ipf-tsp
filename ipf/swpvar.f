C    @(#)swpvar.f	20.6 7/18/96
        subroutine swpvar (m,n)
C
        include 'ipfinc/parametr.inc'

        include 'ipfinc/bus.inc'
        include 'ipfinc/pctvr2.inc'
C
        do i=1,3
           itemp = kpctvr(i,m)
           kpctvr(i,m) = kpctvr(i,n)
           kpctvr(i,n) = itemp
        enddo
 
        return
        end
