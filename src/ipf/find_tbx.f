C    @(#)find_tbx.f	20.3 2/13/96
      integer function find_tbx (busname, basekv)

      character*(*)  busname
      real           basekv

      external       tbx_hash
      integer        tbx_hash

      include 'ipfinc/parametr.inc'

      include 'ipfinc/oldtbx.inc'

      integer        h, p
      logical        found

      h = tbx_hash (busname, basekv)
      p = htable_tbx(h)

      found = .false.
      do while ( p .ne. 0  .and.  .not. found )
         if ( busname .ne. tbxbus(p)  .or. 
     &        basekv .ne. tbxbase(p) ) then
            p = nextptr_tbx(p)
         else
            found = .true.
         endif
      end do

      find_tbx = p

      return
      end
