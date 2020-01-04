C    @(#)swpvlt.f	20.3 2/13/96
        subroutine swpvlt (m,n)

C       Swap the indices in the over/undervoltage index array.

        include 'ipfinc/parametr.inc'
        include 'ipfinc/apcom.inc'
 
        itemp = isort(m)
        isort(m) = isort(n)
        isort(n) = itemp
 
        return
        end
