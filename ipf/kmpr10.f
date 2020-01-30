C    @(#)kmpr10.f	20.3 2/13/96
        function kmpr10 (i,j)
C
C       This function sorts NORDER() using the Y-matrix row pointers.
c       NORDER() sorted can compress the Y-matrix.
C
        include 'ipfinc/parametr.inc'

        include 'ipfinc/alpha.inc'
        include 'ipfinc/norder.inc'

        integer start1, start2

        i1 = norder(i)
        j1 = norder(j)
        if (km(i1) .gt. 0 .and. kmlen(i1) .gt. 0) then
          start1 = km(i1)
        else 
          start1 = 1000000 + i1
        endif
        if (km(j1) .gt. 0 .and. kmlen(j1) .gt. 0) then
          start2 = km(j1)
        else 
          start2 = 1000000 + j1
        endif
        kmpr10  = start1 - start2
        return
        end
