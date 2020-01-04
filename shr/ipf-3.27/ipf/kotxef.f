C    @(#)kotxef.f	20.3 2/13/96
        function kotxef(i,j)
 
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/anlys.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/busanl.inc'
 
        character id1*1, id2*1
 
        kotxef = ltxeff(2,i) - ltxeff(2,j)
        if (kotxef.eq.0) then
           kotxef = ltxeff(3,i) - ltxeff(3,j)
           if (kotxef .eq. 0) then
              write (id1,110) ltxeff(4,i)
              write (id2,110) ltxeff(4,j)
  110         format (a1)
              kotxef = kompr (id1,id2,junk)
              if (kotxef .eq. 0) then
                 kotxef = ltxeff(5,i) - ltxeff(5,j)
              endif
           endif
        endif
        return
        end
