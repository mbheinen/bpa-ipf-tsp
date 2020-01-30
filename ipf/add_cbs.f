C    @(#)add_cbs.f	20.3 1/15/98
C****************************************************************
C
C      File: add_cbs.f
C
C      Purpose: Integer function to add a continuation bus item.
C
C      Author: Walt Powell  Date: 14 December 1992
C      Called by: assequiv.f
C
C****************************************************************
C
       integer function add_cbs (nb, cbtype, cbowner, cbyear)
       character cbtype*1, cbowner*3, cbyear*2

       include 'ipfinc/parametr.inc'

       include 'ipfinc/blank.inc'
       include 'ipfinc/bus.inc'
       include 'ipfinc/cbus.inc'
       include 'ipfinc/prt.inc'

       logical found
       character cbtyp*1, cbown*3, cbkyr*2
       integer ptr

       add_cbs = 0
       found = .false.
       ptr = kbsdta(15,nb)
       do while (ptr .gt. 0 .and. .not. found)
         call getchr (1, cbtyp, kbctbl(8,ptr))
         call getchr (2, cbkyr, kbctbl(9,ptr))
         call getchr (3, cbown, kbctbl(10,ptr))
         if ((cbtyp .eq. cbtype .or. cbtype .eq. '*')   .and.
     &       (cbown .eq. cbowner .or. cbowner .eq. '*') .and.
     &       (cbkyr .eq. cbyear .or. cbyear .eq. '*')) then
           found = .true.
         else
           ptr = bctbl_nxt(ptr)
         endif
       enddo
       if (.not. found) then     
         if (ntot2 .ge. MAXCBS) then
           write (errbuf(1), 10070) MAXCBS
10070      format(' More than ', i4, ' + bus records entities.')
           write (errbuf(1), 10080) cbtype, cbowner, cbyear, bus(nb), 
     &       base(nb)
10080      format(' Type ', a, ' Owner ', a, ' Code year ', a, ' Bus ',
     &       a8, f6.1)
           call prterx ('W',2)
           ntot2 = 1
           ptr = 0
           go to 900
         endif
         ntot2 = ntot2 + 1
         kbctbl(1,ntot2) = nb
         call linkcbus (ntot2, error)
         do i = 2, 12
           bctbl(i,ntot2) = 0.0
         enddo
         call putchr (1, cbtype, kbctbl(8,ntot2))
         call putchr (2, cbyear, kbctbl(9,ntot2))
         call putchr (3, cbowner, kbctbl(10,ntot2))
         ptr = ntot2
       endif
  900  continue
       add_cbs = ptr
       return
       end         
