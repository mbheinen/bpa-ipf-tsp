C    @(#)komozs.f	20.3 2/13/96
      function komozs(m,n)
C
C     COMPARE OUTPUT SORT ORDER BY ZONES OR OWNERS
C
      include 'ipfinc/parametr.inc'

      include 'ipfinc/asort.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'

      character zn1*2, zn2*2, own1*3, own2*3
C
        i=sorto(m)
        j=sorto(n)
        if ( kspare(11) .eq. 1 ) then
           if (bus(i) .eq. '~~~~~~~~') then
              zn1 = '~~'
           else
              zn1 = zone(i)
           endif
           if (bus(j) .eq. '~~~~~~~~') then
              zn2 = '~~'
           else
              zn2 = zone(j)
           endif
           if (kompr(zn1, zn2, komozs) .eq. 0) then
              komozs = inp2alf(i) - inp2alf(j)
           endif
        else if ( kspare(11) .eq. 3 ) then
           if (bus(i) .eq. '~~~~~~~~') then
              own1 = '~~~'
           else
              own1 = zone(i)
           endif
           if (bus(j) .eq. '~~~~~~~~') then
              own2 = '~~~'
           else
              own2 = zone(j)
           endif
           if (kompr(own1, own2, komozs) .eq. 0) then
              komozs = inp2alf(i) - inp2alf(j)
           endif
        else
c           komozs = inp2alf(i) - inp2alf(j)
           komozs = 0
        endif
        return
        end
