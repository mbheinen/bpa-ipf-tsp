C    @(#)get_cbs.f	20.3 1/15/98
C****************************************************************
C
C      File: get_cbs.f
C
C      Purpose: Integer function to find a continuation bus item.
C
C      Author: Walt Powell  Date: 14 December 1992
C      Called by: assequiv.f
C
C****************************************************************
C
       integer function get_cbs (nb, cbtype, cbowner, cbyear)
       character cbtype*1, cbowner*3, cbyear*2

       include 'ipfinc/parametr.inc'

       include 'ipfinc/blank.inc'
       include 'ipfinc/bus.inc'
       include 'ipfinc/cbus.inc'
       include 'ipfinc/prt.inc'

       logical found
       character cbtyp*1, cbown*3, cbkyr*2
       integer ptr

       get_cbs = 0
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
       get_cbs = ptr
       return
       end         
