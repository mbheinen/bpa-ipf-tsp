C    @(#)swpovl.f	20.3 2/13/96
        subroutine swpovl (m,n)

C       Swap the indices in the branch overload index array.

        include 'ipfinc/parametr.inc'
        include 'ipfinc/apcom.inc'
 
        i = isort(m)
        isort(m) = isort(n)
        isort(n) = i
        return
        end
