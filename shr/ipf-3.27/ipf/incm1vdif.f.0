C    %W% %G%
C****************************************************************
C
C   File: incm1vdif.f
C
C   Purpose: Integer function to increment branch indices m1 for
c            bus k1
C
C   Author: Walt Powell  Date: 14 December 1992
C   Called by: lfodifrpt.f
C
C****************************************************************
C
        integer function incm1vdif (ptr1, nb1, m1, lsw)
        integer ptr1

        include 'ipfinc/parametr.inc'

        include 'ipfinc/blank.inc'
        include 'ipfinc/bus.inc'
        include 'ipfinc/arcntl.inc'
        include 'ipfinc/area.inc'
        include 'ipfinc/owncom.inc'
        include 'ipfinc/sortuvov.inc'
        include 'ipfinc/branch.inc'
        include 'ipfinc/prt.inc'

	external chkfltr
        logical chkfltr
c
c       lsw assignments: 1 - normal
c                        2 - e-o-f ptr1
c                        3 - e-o-f ptr2
c                        4 - e-o-f ptr1 and ptr2
c

  100   if (lsw .eq. 1 .or. lsw .eq. 3) then
           ptr1 = brnch_nxt(ptr1)
           if (ptr1 .gt. 0) then
              m1 = ky(ptr1)
              if (inp2alf(nb1) .lt. inp2alf(m1)) then
              else if (chkfltr (arcnam(jarzn(m1)), zone(m1), '***',
     1                          base(m1), '**', 0)) then
                 go to 100
              endif
           else
              lsw = lsw + 1
           endif
        endif

        incm1vdif = lsw
        return
        end
