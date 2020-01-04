C    @(#)bus_hash.f	20.3 2/13/96
      integer function bus_hash (busname, basekv) 

      character*(*)  busname 
      real           basekv 
 
c     compute hash value of busname 
 
      include 'ipfinc/parametr.inc' 

      include 'ipfinc/bushasht.inc' 

      integer        k, i
 
      k = 0 
      do i = 1, 8 
         k = k + k + ichar (busname(i:i)) 
      end do 
      k = k + int (basekv) 
      bus_hash = mod (k, BUS_HASHSIZE) + 1 
  
      return 
      end 
