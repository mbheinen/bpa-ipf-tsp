C    @(#)find_bus.f	20.3 2/13/96
      integer function find_bus (busname, basekv)

      character busname *(*)
      real      basekv


      include 'ipfinc/parametr.inc'

      include 'ipfinc/bushasht.inc'
      include 'ipfinc/bus.inc'

      external       bus_hash
      integer        bus_hash
      integer        p, h
      logical        found

      h = bus_hash (busname, basekv)
      p = htable_b(h)

      found = .false.
      do while ( p .ne. 0  .and.  .not. found )
        if ( busname .ne. bus(p)  .or.  basekv .ne. base(p) ) then
           p = nextptr_b(p)
        else
           found = .true.
        endif
      end do

      find_bus = p

      return
      end
