C    @(#)del_brn_tp.f	20.3 2/13/96
C****************************************************************
C      File: del_brn_tp.f
C      Purpose: delete one transpose of a branch
C****************************************************************
C
       subroutine del_brn_tp(ptr)
       integer ptr

       include 'ipfinc/parametr.inc'

       include 'ipfinc/bus.inc'
       include 'ipfinc/branch.inc'

       integer p1, p2
C
C      Delete one transpose of a double-entry branch entity
C
       if ( ptr .gt. MAXBRN2  .or.  ptr .lt. 1 ) goto 9000
       k1 = kx(ptr)
       p1 = 0
       p2 = kbsdta(16,k1)
       do while ( p2 .ne. 0  .and.  p2 .ne. ptr )
          p1 = p2
          p2 = brnch_nxt(p2)
       enddo
       if ( p2 .ne. ptr ) goto 9000
       if (p1 .eq. 0) then
          kbsdta(16,k1) = brnch_nxt(ptr)
       else
          brnch_nxt(p1) = brnch_nxt(ptr)
       endif
       kx(ptr) = 7 777 777
       ky(ptr) = 7 777 777
       brnch_ptr(ptr) = 7 777 777
       brnch_nxt(ptr) = 7 777 777

       return

 9000  continue
       write(*,'(a)')' Programming error, bad branch link in' //
     &       ' DEL_BRN_TP'

       return
       end
