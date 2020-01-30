C    @(#)intchcor.f	20.3 2/13/96
        integer function intchcor(busx)
        character busx*10

        include 'ipfinc/bscords.inc'
C
C       Search for Area Name for Area Interchange (bubble) plot
C       Binary search of BUS to find location of BUSX
C       The Area name is actually stored in the bus and busx variables
C
        integer hit,hi
        logical found
C
        lo=kntbus + 1
        hi=kntbus + kntarea
*************** debug stuff *****************
*     print 9901, kntbus, kntarea, BUSX
*9901 format('in intchcor-kntbus, BUSX', 2i5, a10)
*************** debug stuff *****************
C
        found = .false.
        do while (lo .le. hi .and. .not. found)
           loc = (lo + hi)/2
           hit = kompr(busx,bus(loc),hit) 
           if (hit .lt. 0) then
              hi = loc - 1
           else if (hit .gt. 0) then
              lo = loc + 1
           else
              found = .true.
           endif
        enddo
        if (found) then
           intchcor = loc
        else
           intchcor = -max0(lo,hi)
        endif
        return
        end
