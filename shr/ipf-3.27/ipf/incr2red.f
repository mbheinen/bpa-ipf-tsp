C    @(#)incr2red.f	20.3 2/13/96
C****************************************************************
C
C      File: incr1red.f
C
C      Purpose: Integer function to increment YRED indices.
C
C      Author: Walt Powell  Date: 14 December 1992
C      Called by: reduct.f
C
C****************************************************************
C
        integer function incr2red (jt, js, nb, m1, m2, ksw)

        include 'ipfinc/parametr.inc'

        include 'ipfinc/bus.inc'
        include 'ipfinc/red7.inc'
C       
C       Increment indices for YRED
C       
c       ksw assignments: 1 - normal
c                        2 - e-o-f ptr (kx-ky-brnch)
c                        3 - e-o-f jt (yred)
c                        4 - e-o-f ptr and jt
c
C       Increment indices for YRED
C       
 1940   jt=jt+1   
        if (jt .le. js) then
           m2 = yred(1,jt)
           if (m2 .lt. 0) call erexit
           m1=inp2opt(m2)  
           if (m2 .eq. nb) go to 1940  ! skip diagonal
        else
          if (ksw .eq. 1 .or. ksw .eq. 2) ksw=ksw+2 
        endif
        incr2red = ksw
        return
        end
