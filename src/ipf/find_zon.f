C    @(#)find_zon.f	20.3 2/13/96
      integer function find_zon (zonename)   
      character zonename *(*)   

      include 'ipfinc/parametr.inc' 

      include 'ipfinc/blank.inc' 
      include 'ipfinc/arcntl.inc' 

      integer        first, last, p, compare 
      logical        found 

      first = 1 
      last = nztot 
      found = .false. 

      do while (first .le. last .and. .not.found) 
         p = (first + last) / 2 
         compare = kompr (acznam(p), zonename, compare) 
         if (compare .eq. 0) then 
            found = .true. 
         else if (compare .lt. 0) then 
            first = p + 1 
         else 
            last = p - 1 
         endif 
      end do 

      if (found) then 
         find_zon = p 
      else 
         find_zon = -max0 (first, last) 
      endif 

      return 
      end 
