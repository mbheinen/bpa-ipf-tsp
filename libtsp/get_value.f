C    %W% %G%
	function get_value (ib, ia)
	integer ib, ia

        include 'tspinc/params.inc'
        include 'tspinc/wstequ.inc'
        include 'tspinc/room.inc'
        include 'tspinc/wfeq.inc'
	include 'tspinc/vfhistory.inc'
        include 'tspinc/filter.inc'

        if (ia .eq. 1) then
          volt = sqrt (eyr(ib) ** 2 + eyi(ib) ** 2)
          dv = amin1 (volt - rbuss(1,ib), 0.0)
          get_value = dv / rbuss(1,ib)
        else if (ia .eq. 2) then
          get_value = abs (frqbse + busfeq(ib)) 
        else if (ia .eq. 3) then
          volt = sqrt (eyr(ib) ** 2 + eyi(ib) ** 2)
          get_value = volt
        else
          volt = sqrt (eyr(ib) ** 2 + eyi(ib) ** 2)
          dv = amax1 (volt - rbuss(1,ib), 0.0)
          get_value = dv / rbuss(1,ib)
        endif
        return
        end
