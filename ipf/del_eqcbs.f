C    @(#)del_eqcbs.f	20.3 2/13/96
C****************************************************************
C
C      File: del_eqcbs.f
C
C      Purpose: Integer function to delete a continuation bus from the 
C               network data base
C
C      Author: Walt Powell  Date: 14 December 1992
C      Called by: reduct.f
C
C****************************************************************
C
       integer function del_eqcbs (ptr, oldptr)
       integer ptr, oldptr

       include 'ipfinc/parametr.inc'

       include 'ipfinc/blank.inc'
       include 'ipfinc/bus.inc'
       include 'ipfinc/cbus.inc'
       include 'ipfinc/red7.inc'
       include 'ipfinc/prt.inc'

       common /scratch/ nbr, array(2,100),
     &                  ndel1, lindel1(MAXBRN), ndel2,
     &                  lindel2(MAXBRN2), ndel3, cbdel(MAXBUS),
     &                  br_status(MAXBRN)
       integer array, cbdel, br_status

       del_eqcbs = 0

       k1 = kbctbl(1,ptr)
       if (oldptr .eq. 0) then
          kbsdta(15,k1) = bctbl_nxt(ptr)
       else
          bctbl_nxt(oldptr) = bctbl_nxt(ptr)
       endif
       if (ndel3 .lt. MAXBUS) then 
          ndel3=ndel3+1  
          cbdel(ndel3)=ptr
       else
          do i=1,MAXBUS
             if (cbdel(i) .eq. 0) then
                cbdel(i)=ptr
                go to 110
             endif
          enddo
  110     continue 
       endif
       do i = 1, 12
          kbctbl(i,ptr) = 0
       enddo   

 9000  continue
       return
       end
