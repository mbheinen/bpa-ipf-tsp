C    @(#)kompef.f	20.3 2/13/96
        integer function kompef(i,j)
 
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/anlys.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/busanl.inc'
      include 'ipfinc/qksrt.inc'
 
        character id1*1, id2*1, own1, own2
C
C       Sort KEY: 0 - Sort by bus names.
C                 1 - sort by voltage-bus names.
C                 2 - Sort by owner-bus names.
C                 3 - Sort by zone-bus names.
C
        kompef = 0
        if (key .eq. 1) then
           x = 100.0 * (base(leff(2,i)) - base(leff(2,j)))
           kompef = int (x)
        else if (key .eq. 2) then
           write (own1, 100) leff(1,i)
           write (own2, 100) leff(1,j)
  100      format (a3)
           kompef = kompr(own1, own2, kompef)
        else if (key .eq. 3) then
           kompef = kompr(zone(leff(2,i)), zone(leff(2,j)), kompef)
        endif
        if (kompef .eq. 0) then
           kompef = leff(2,i) - leff(2,j)
        endif
        if (kompef .eq. 0) then
           kompef = leff(3,i) - leff(3,j)
           if (kompef .eq. 0) then
              write (id1,110) leff(4,i)
              write (id2,110) leff(4,j)
  110         format (a1)
              kompef = kompr (id1,id2,junk)
              if (kompef .eq. 0) then
                 kompef = leff(5,i) - leff(5,j)
              endif
           endif
        endif
        return
        end
