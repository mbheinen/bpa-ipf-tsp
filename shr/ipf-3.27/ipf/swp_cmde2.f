C    @(#)swp_cmde2.f	20.1 8/20/98
        subroutine swp_cmde2 (m,n)

C       Swap the indices in the branch overload index array.

        include 'ipfinc/parametr.inc'
        include 'ipfinc/cmde_com.inc'
 
        i = isort(m)
        isort(m) = isort(n)
        isort(n) = i
        return
        end
