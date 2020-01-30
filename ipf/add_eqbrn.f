C    @(#)add_eqbrn.f	20.3 2/13/96
C****************************************************************
C
C      File: add_eqbrn.f
C
C      Purpose: Integer function to add basic equivalent branch data
C               (sans electrical data) to the network data base. 
C
C      Author: Walt Powell  Date: 14 December 1992
C      Called by: assequiv.f
C
C****************************************************************
C
       integer function add_eqbrn (k1, k2, id, nadd1, nadd2)
       character id*1

       include 'ipfinc/parametr.inc'

       include 'ipfinc/blank.inc'
c	Global variables used:
c		ltot
       include 'ipfinc/bus.inc'
c	Global variables used:
c		inp2opt, bus, base, inp2alf, kbsdta
       include 'ipfinc/branch.inc'
c	Global variables used:
c		kbrnch, brid, brnch_nxt, rateln, brnch_ptr, 
c		brsect, ky, kx, brtype
       include 'ipfinc/red7.inc'
c	Global variables used:
c		None
       include 'ipfinc/prt.inc'
c	Global variables used:
c		errbuf
       include 'ipfinc/ikk.inc'
c	Global variables used:
c		ikk

       common /scratch/ nbr, array(2,100),
     &                  ndel1, lindel1(MAXBRN), ndel2,
     &                  lindel2(MAXBRN2), ndel3, cbdel(MAXBUS),
     &                  br_status(MAXBRN)
       integer array, cbdel, br_status

       integer ptr, oldptr
       logical found

       add_eqbrn = 0
c
c      Check validity of equivalent branch
c
       kt = inp2opt(k1)
       mt = inp2opt(k2)
       if (ikk(1,kt) .ne. 1 .or. ikk(2,kt) .ne. 1 .or.
     &     ikk(1,mt) .ne. 1 .or. ikk(2,mt) .ne. 1) then
          write (errbuf(1), 100) bus(k1), base(k1), bus(k2), base(k2)
  100     format(' Cannot add equivalent branch to non-border nodes ',
     &        a8, f6.1, ' to ', a8, f6.1)
          call prterx ('W',1) 
          add_eqbrn = 1
          go to 9000
       endif
       ltotr = ltotr+1 
C       
C      Find double-entry index for adding for equivalent branch 
C       
       if (ndel2 .eq. 0) then 
          ltot2 = ltot2+1 
          nadd2 = ltot2   
       else  
          found = .false.
          i = 1
          do while (i .le. ndel2 .and. .not. found)
             if (lindel2(i) .gt. 0) then  
                nadd2 = lindel2(i)  
                lindel2(i) = 0 
                found = .true.
             else
                i = i + 1
             endif   
          enddo
          if (.not. found) then
             ltot2 = ltot2 + 1 
             nadd2 = ltot2 
          endif
       endif 
       if (nadd2 .gt. MAXBRN2) then   
          write (errbuf(1), 110) ltotr,MAXBRN2
  110     format(' More than ',i5,' equivalent branches added to system.
     & allocated space ',i5,' is insufficient') 
          call prterx ('W',1) 
          add_eqbrn = 1
          go to 9000
       endif 
C       
C      Find single-entry index for adding for equivalent branch 
C       
       nadd1 = 0
       if (inp2alf(k1) .gt. inp2alf(k2)) then
          ptr = kbsdta(16,k2)
          do while (ptr .gt. 0)
             if (ky(ptr) .eq. k1) then
                if (brid(ptr) .eq. id .or. id .eq. '*') then
                   nadd1 = brnch_ptr(ptr)
                   ptr = 0
                endif
             endif
             if (ptr .gt. 0) ptr = brnch_nxt(ptr)
          enddo
          if (nadd1 .le. 0) then
             write (errbuf(1), 120) bus(k1), base(k1), bus(k2), base(k2)
  120        format(' Error finding single-entry branch index for equiva
     &lent branch ', a8, f6.1, ' to ', a8, f6.1)
             call prterx ('W',1) 
             add_eqbrn = 1
             go to 9000
          else
             brnch_ptr(nadd2) = -nadd1
          endif
       else
          if (ndel1 .eq. 0) then 
             ltot = ltot+1 
             nadd1 = ltot   
             brnch_ptr(nadd2) = nadd1
          else  
             found = .false.
             i = 1
             do while (i .le. ndel1 .and. .not. found)
                if (lindel1(i) .gt. 0) then  
                   nadd1 = lindel1(i)  
                   lindel1(i) = 0 
                   found = .true.
                else
                   i = i + 1
                endif   
             enddo
             if (.not. found) then
                ltot = ltot + 1 
                nadd1 = ltot 
             endif
             brnch_ptr(nadd2) = nadd1
          endif 
       endif
c
c      Find insertion point
c
       oldptr = 0
       ptr = kbsdta(16,k1)
       do while (ptr .gt. 0 .and. 
     &          (inp2alf(ky(ptr)) .lt. inp2alf(k2) .or.
     &          (ky(ptr) .eq. k2 .and. brid(ptr) .ne. id) .or.
     &          (ky(ptr) .eq. k2 .and. id .ne. '*')))
          oldptr = ptr
          ptr = brnch_nxt(ptr)
       enddo
       if (oldptr .eq. 0) then
          kbsdta(16,k1) = nadd2
       else
          brnch_nxt(oldptr) = nadd2
       endif
       brnch_nxt(nadd2) = ptr
       if (base(k1) .ne. base(k2)) then  
          brtype(nadd2) = 5
       else
          brtype(nadd2) = 8  
       endif 
       kx(nadd2) = k1 
       ky(nadd2) = k2
       brid(nadd2) = id
       brsect(nadd2) = 0
       if (brnch_ptr(nadd2) .gt. 0) then
          nbr = iabs(brnch_ptr(nadd2))
          do i = 1, 18
             kbrnch(i,nbr) = 0  
          enddo
          if (k1 .gt. k2) kbrnch(17,nbr) = 1   
          if (base(k1) .ne. base(k2)) then  
             brnch(9,nbr) = base(k1)  
             brnch(10,nbr) = base(k2) 
          endif 
          do i = 1,3
             rateln(i,nbr) = 0.0
          enddo
       endif
 9000  continue
       return
       end
