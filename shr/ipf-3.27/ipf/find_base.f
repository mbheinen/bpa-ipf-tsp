C    @(#)find_base.f	20.3 2/13/96
      integer function find_base (basekv)   

      include 'ipfinc/parametr.inc' 

      include 'ipfinc/blank.inc' 
      include 'ipfinc/bsekvhsh.inc'

      integer        first, compare
      logical        found 

      first = 1 
      last = numbases
      found = .false. 

      do while (first .le. last .and. .not.found) 
         nb = (first + last) / 2 
         compare = 100.0 * (basekvs(nb) - basekv)
         if (compare .eq. 0) then 
            found = .true. 
         else if (compare .lt. 0) then 
            first = nb + 1 
         else 
            last = nb - 1 
         endif 
      end do 

      if (found) then 
         find_base = nb 
      else 
         find_base = -max0 (first, last) 
      endif 

      return 
      end 
