C    @(#)incr1red.f	20.3 2/13/96
C****************************************************************
C
C      File: incr1red.f
C
C      Purpose: Integer function to increment original branch nb 
C               indices.
C
C      Author: Walt Powell  Date: 14 December 1992
C      Called by: reduct.f
C
C****************************************************************
C
        integer function incr1red (ptr, oldptr, nb, k2, id, ksw)
        integer ptr, oldptr
        character id*1

        include 'ipfinc/parametr.inc'

        include 'ipfinc/blank.inc'
        include 'ipfinc/bus.inc'
        include 'ipfinc/branch.inc'
C       
C       Increment indices for YMTRX   
C       
c       ksw assignments: 1 - normal
c                        2 - e-o-f ptr (kx-ky-brnch)
c                        3 - e-o-f jt (yred)
c                        4 - e-o-f ptr and jt
c
 1920   if (ksw .eq. 1 .or. ksw .eq. 3) then
           if (ptr .eq. 0) then
              ptr = kbsdta(16,nb)
              oldptr = 0
           else
              oldptr = ptr
              ptr = brnch_nxt(ptr)
           endif
           if (ptr .gt. 0) then 
              k2=ky(ptr)
              if (brtype(ptr) .eq. 2 .or. brtype(ptr) .eq. 7) 
     &           go to 1920 
              id = brid(ptr)
           else
              ksw = ksw + 1   
              k2=0
           endif  
        endif 
        incr1red = ksw
        return
        end
