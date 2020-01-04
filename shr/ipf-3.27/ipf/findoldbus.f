C    @(#)findoldbus.f	20.3 2/13/96
      integer function findoldbus (busname, basekv)

      character busname *(*)
      real      basekv


      include 'ipfinc/parametr.inc'

      include 'ipfinc/alt_case.inc'

      external       bus_hash
      integer        bus_hash
      integer        p, h
      logical        found

      h = bus_hash (busname, basekv)
      p = ohtable_b(h)

      found = .false.
      do while ( p .ne. 0  .and.  .not. found )
         if ( busname .ne. oldbus(p)  .or. 
     &        basekv .ne. oldbase(p) ) then
            p = onextptr_b(p)
         else
            found = .true.
         endif
      end do

      findoldbus = p

      return
      end
