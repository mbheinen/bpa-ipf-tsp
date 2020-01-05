C    @(#)oldbsh.f	20.3 2/13/96
      integer function oldbsh(busx,basex)
C
C     Binary search of OLDBUS, OLDBAS to find location of BUSX, BASEX
C
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/oldbus.inc'
C
        character busx*8
        integer hit,hi
C
        lo=1
        hi=numobs
C
100     if(lo.le.hi)then
           loc=(lo + hi)/2
           if (kompr(busx, oldbus(loc), hit).eq.0) then
              hit=100.0*(basex-oldbas(loc))
           endif
           if(hit.ne.0) then
              if(hit.lt.0)then
                 hi=loc-1
              else
                 lo=loc+1
              endif
              go to 100
           endif
        endif
        if(hit.eq.0)then
           oldbsh=loc
        else
           oldbsh= -max0(lo,hi)
        endif
        return
        end
