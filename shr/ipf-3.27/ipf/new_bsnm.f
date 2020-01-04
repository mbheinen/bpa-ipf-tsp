C    @(#)new_bsnm.f	20.1 10/10/96
C****************************************************************
C
C   	File: new_bsnm.f
C
C   	Purpose: Create a unique bus name from 6-characters of a 
C                reference bus, an user-supplied field character, and
C                a program-selected character [A-Z|0-9].
C                                                                      *
C       Input parameters:
C
C             busname, basekv  - the reference bus
C             field            - the field separator
C             num              - the assigned location in bus(), 
c                                base()
C       Return value:
C
c             status   - N > 0 : bus index in bus(), base()
C                        N = 0 : failure
C
C
C   	Author: Walt Powell            Date: 13 January 1993
C   	Called by: ext_bus.f
C
C****************************************************************
C
      integer function new_bsnm ( busname, basekv, field, num )
      character*(*) busname, field
      real basekv
      integer num
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/prt.inc'

      integer add_bus
      logical finished
      character newname*8
      real newbasekv
c
c     Find unique pseudo-bus name
c
      new_bsnm = 0
      next = 49
      finished = .false.
      do while (next .le. 90 .and. .not. finished)
         newname = busname(1:6) // field(1:1) // char(next)
         newbasekv = basekv
         new_bsnm = add_bus (newname, newbasekv, num)
         if (new_bsnm .gt. 0) then
            finished = .true.
         else if (next .lt. 57 .or. next .gt. 64) then
            next = next + 1
         else  
            next = 65
         endif
      enddo
      if (.not. finished) then
         write ( errbuf(1), 100) busname, basekv
  100    format (' Error constructing unique name for PSEUDOBUS ', 
     &      a8, f6.1)
         call prterx ('W', 1)
      endif
      return
      end
