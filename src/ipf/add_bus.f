C    @(#)add_bus.f	20.3 2/13/96
      integer function add_bus (busname, basekv, kx)
 
      character*(*)  busname
      real           basekv

c     install a new bus(kx), base(kx) entry into hashtable

      include 'ipfinc/parametr.inc'

      include 'ipfinc/blank.inc'
c	Global Variables used:
c		None
      include 'ipfinc/bus.inc'
c	Global Variables used:
c		base, bus
      include 'ipfinc/bushasht.inc'
c	Global Variables used:
c		htable_b, nextptr_b

      integer        h, bus_hash, p
      logical        status

      status = (kx .le. MAXBUS)

      if (status) then
         h = bus_hash (busname, basekv)
         p = htable_b(h)
         do while (p .gt. 0)         !search for existing entities
            if (busname .ne. bus(p) .or. basekv .ne. base(p)) then
               p = nextptr_b(p)
            else
               p = -p                   
            endif
         enddo
         if (p .eq. 0) then
            nextptr_b(kx) = htable_b(h)
            htable_b(h) = kx
            bus(kx) = busname
            base(kx) = basekv
            add_bus = kx
         else
            add_bus = p                  ! p < 0 flags duplicate entity!
         endif
      else
         add_bus = 0                     ! overflow flagged!
      endif
      return
      end
