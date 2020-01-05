C    @(#)ksrchcor.f	20.3 2/13/96
        integer function ksrchcor(busx,basex)
        character busx*8

        include 'ipfinc/bscords.inc'
C
C       Binary search of BUS,BASE to find location of BUSX,BASEX
C
        integer hit,hi
        logical found
C
        lo=1
        hi=kntbus
*************** debug stuff *****************
*     print 9901, kntbus, BUSX, BASEX
*9901 format('in ksrchcor-kntbus, BUSX, BASEX', i5, a8,1x,f6.1)
*************** debug stuff *****************
C
        found = .false.
        do while (lo .le. hi .and. .not. found)
           loc = (lo + hi)/2
           if (kompr(busx,bus(loc),hit) .eq. 0) then
              hit = 100.0*(basex-base(loc))
           endif
           if (hit .lt. 0) then
              hi = loc - 1
           else if (hit .gt. 0) then
              lo = loc + 1
           else
              found = .true.
           endif
        enddo
        if (found) then
           ksrchcor = loc
        else
           ksrchcor = -max0(lo,hi)
        endif
        return
        end
