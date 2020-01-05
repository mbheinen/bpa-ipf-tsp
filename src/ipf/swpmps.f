C    @(#)swpmps.f	20.3 2/13/96
        subroutine swpmps (i,j)

        include 'ipfinc/cut.inc'

        do l = 1,2
           ktemp = mpsort(l,i)
           mpsort(l,i) = mpsort(l,j)
           mpsort(l,j) = ktemp
        enddo
        return
        end
