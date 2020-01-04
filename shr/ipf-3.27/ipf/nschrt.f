C    @(#)nschrt.f	20.3 2/13/96
        function nschrt (a,b)
C
C       This function finds via binary search the default ratings
C       in array ZRAT.
C
        include 'ipfinc/dflrat.inc'
C
        nschrt = 0
        i1 = 1
        i2 = nrat
        do while (i1 .le. i2 .and. nschrt .eq. 0)
           i = (i1 + i2)/2
           komp = zrat(1,i) - a
           if (komp .eq. 0) komp = zrat(2,i) - b
           if (komp .lt. 0) then
              i1 = i + 1
           else if (komp .gt. 0) then
              i2 = i - 1
           else
              nschrt = i
           endif
        enddo
        return
        end
