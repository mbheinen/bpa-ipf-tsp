C    @(#)sp_outcmd2.f	20.1 8/20/98
        subroutine sp_outcmd2 (m,n)

C       Exchange the indices in the branch outage index array.

        include 'ipfinc/parametr.inc'
        include 'ipfinc/cmde_com.inc'
 
        i = isort(m)
        isort(m) = isort(n)
        isort(n) = i
 
        return
        end
