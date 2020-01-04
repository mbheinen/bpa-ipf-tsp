C    %W% %G%
C****************************************************************
C
C   File: incm2vdif.f
C
C   Purpose: Integer function to increment branch m2 indices for 
c            bus k2
C
C   Author: Walt Powell  Date: 14 December 1992
C   Called by: lfodifrpt.f
C
C****************************************************************
C
        integer function incm2vdif (ptr2, nb2, m2, lsw)
        integer ptr2

        include 'ipfinc/parametr.inc'

        include 'ipfinc/alt_case.inc'

	external chkfltr
        logical chkfltr
c
c       lsw assignments: 1 - normal
c                        2 - e-o-f ptr1
c                        3 - e-o-f ptr2
c                        4 - e-o-f ptr1 and ptr2
c

  100   if (lsw .eq. 1 .or. lsw .eq. 2) then
           ptr2 = obrnch_nxt(ptr2)
           if (ptr2 .gt. 0) then
              m2 = oky(ptr2)
              if (oinp2alf(nb2) .lt. oinp2alf(m2)) then
              else if (chkfltr (oarcnam(oarzn(m2)), oldzone(m2), '***',
     1                          oldbase(m2), '**', 0)) then
                 go to 100
              endif
           else
              lsw = lsw + 2
           endif
        endif

        incm2vdif = lsw
        return
        end
