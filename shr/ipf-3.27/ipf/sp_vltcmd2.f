C    @(#)sp_vltcmd2.f	20.1 8/20/98
        subroutine sp_vltcmd2 (m,n)

C       Swap the indices in the over/undervoltage index array.

        include 'ipfinc/parametr.inc'
        include 'ipfinc/cmde_com.inc'
 
        itemp = isort(m)
        isort(m) = isort(n)
        isort(n) = itemp
 
        return
        end
