C    %W% %G%
C****************************************************************
C
C   File: inck2vdif.f
C
C   Purpose: Integer function to increment bus k2 indices
C
C   Author: Walt Powell  Date: 14 December 1992
C   Called by: lfodifrpt.f
C
C****************************************************************
C
        integer function inck2vdif (inb2, nb2, ksw)

        include 'ipfinc/parametr.inc'

        include 'ipfinc/alt_case.inc'

	external chkfltr
        logical chkfltr, check
c
c       ksw assignments: 1 - normal
c                        2 - e-o-f inb1
c                        3 - e-o-f inb2
c                        4 - e-o-f inb1 and inb2
c
  100   if (ksw .eq. 1 .or. ksw .eq. 2) then
           inb2 = inb2 + 1
           if (inb2 .gt. ontot_alf) then
              ksw = ksw + 2
              nb2 = ontot + 1
           else
              nb2 = oalf2inp(inb2)
              check = chkfltr (oarcnam(oarzn(nb2)), oldzone(nb2), '***',
     &                         oldbase(nb2), '**', 0)
              if (.not. check) go to 100
           endif
        endif
        inck2vdif = ksw
        return
        end
