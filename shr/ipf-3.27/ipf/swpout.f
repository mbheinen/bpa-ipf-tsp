C    @(#)swpout.f	20.3 2/13/96
        subroutine swpout (m,n)

C       Exchange the indices in the branch outage index array.

        include 'ipfinc/parametr.inc'
        include 'ipfinc/apcom.inc'
 
        i = isort(m)
        isort(m) = isort(n)
        isort(n) = i
 
        return
        end
