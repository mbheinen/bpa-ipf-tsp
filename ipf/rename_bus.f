C    @(#)rename_bus.f	20.3 2/13/96
C****************************************************************
C
C   File: rename_bus.f
C   Purpose: Rename a bus while preserving its input order. 
C            (Replace hash pointer of old name with hash pointer
c             of new name).
c
c   Parameter:
C
c         ix      - input index, i.e., bus(ix), base(ix)
c         newname - new bus name
c         newbase - new bus base kv
C 
c   Return status:
c
C          0 - succesful
c          1 - error, original bus is not in system
c          2 - error, renamed bus is already in system
c
C   Author: Walt Powell  Date: 20 February 1993
C   Called by: 
C
C****************************************************************
C
      integer function rename_bus ( ix, newname, newbase)
      integer ix
      character newname *(*)
      real newbase
 
      include 'ipfinc/parametr.inc' 

      include 'ipfinc/bus.inc' 
      include 'ipfinc/bushasht.inc' 

      integer p, old_hash, old_p, new_hash, bus_hash
      logical found 

      rename_bus = 0

      old_hash = bus_hash (bus(ix), base(ix))
      old_p = 0
      p = htable_b(old_hash)

      found = .false.    ! remove link to "ix"
      do while (p .gt. 0 .and. .not. found)
         if (p .eq. ix) then
            found = .true.
            if (old_p .gt. 0) then
               nextptr_b(old_p) = nextptr_b(p)
            else
               htable_b(old_hash) = nextptr_b(p)
            endif
         else
            old_p = p
            p = nextptr_b(p)
         endif
      enddo
      if (.not. found) then
         rename_bus = 1
         go to 900
      endif

      new_hash = bus_hash (newname, newbase)
      old_p = 0
      p = htable_b(new_hash)

      found = .false.    ! add link to "ix"
      do while (p .gt. 0 .and. .not. found)
         if (p .eq. ix) then
            found = .true.
         else
            old_p = p
            p = nextptr_b(p)
         endif
      enddo
      if (found) then
         rename_bus = 2
      else
         bus(ix) = newname
         base(ix) = newbase

         if (old_p .gt. 0) then
            nextptr_b(old_p) = ix
         else
            htable_b(new_hash) = ix
         endif
         nextptr_b(ix) = 0
      endif

  900 continue

      return
      end
