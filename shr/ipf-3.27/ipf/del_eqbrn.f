C    @(#)del_eqbrn.f	20.3 2/13/96
C****************************************************************
C
C      File: del_eqbrn.f
C
C      Purpose: Integer function to delete a branch from the 
C               network data base
C
C      Author: Walt Powell  Date: 14 December 1992
C      Called by: reduct, assequiv.f
C
C****************************************************************
C
       integer function del_eqbrn (ptr, oldptr)
       integer ptr, oldptr

       include 'ipfinc/parametr.inc'

       include 'ipfinc/blank.inc'
       include 'ipfinc/bus.inc'
       include 'ipfinc/branch.inc'
       include 'ipfinc/red7.inc'
       include 'ipfinc/prt.inc'

       common /scratch/ nbr, array(2,100),
     &                  ndel1, lindel1(MAXBRN), ndel2,
     &                  lindel2(MAXBRN2), ndel3, cbdel(MAXBUS),
     &                  br_status(MAXBRN)
       integer array, cbdel, br_status

       integer p

       del_eqbrn = 0
C       
C      Delete double-entry branch entity
C       
       k1 = kx(ptr)
       k2 = ky(ptr)
       if (oldptr .eq. 0) then
          kbsdta(16,k1) = brnch_nxt(ptr)
       else
          brnch_nxt(oldptr) = brnch_nxt(ptr)
       endif
       kx(ptr) = ntot + 1   
       ky(ptr) = ntot + 1  
       if (ndel2 .lt. MAXBRN2) then 
          ndel2=ndel2+1  
          lindel2(ndel2)=ptr
       else
          do i=1,MAXBRN2  
             if (lindel2(i) .eq. 0) then
                lindel2(i)=ptr  
                go to 100
             endif
          enddo
  100     continue 
       endif   
C       
C      Conditionally delete single-entry branch entity
C       
       nbr = iabs(brnch_ptr(ptr))
       brnch_ptr(ptr) = 0
       br_status(nbr) = br_status(nbr) + 1
       if (br_status(nbr) .eq. 2) then
          if (ndel1 .lt. MAXBRN) then 
             ndel1=ndel1+1  
             lindel1(ndel1)=nbr
          else
             do i=1,MAXBRN  
                if (lindel1(i) .eq. 0) then
                   lindel1(i)=nbr
                   go to 110
                endif
             enddo
  110        continue 
          endif
          do i = 1, 18
             kbrnch(i,nbr) = 0
          enddo   
          do i = 1,3
             rateln(i,nbr) = 0.0
          enddo
       endif
c
c      Update ptr, oldptr
c
       ptr = oldptr
       if (ptr .gt. 0) then
          oldptr = 0
          p = kbsdta(16,k1)
          do while (p .gt. 0 .and. p .ne. ptr)
             oldptr = p
             p = brnch_nxt(p)
          enddo
       endif
       return
       end
