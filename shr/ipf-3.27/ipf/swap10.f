C    @(#)swap10.f	20.3 2/13/96
        subroutine swap10 (i,j)
C
C       This function sorts NORDER()
C
        include 'ipfinc/parametr.inc'
        include 'ipfinc/norder.inc'
C
        itemp = norder(i)
        norder(i) = norder(j)
        norder(j) = itemp
        return
        end
