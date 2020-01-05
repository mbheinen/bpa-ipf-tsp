C    %W% %G%
C****************************************************************
C
C   File: inck1vdif.f
C
C   Purpose: Integer function to increment bus k1 indices
C
C   Author: Walt Powell  Date: 14 December 1992
C   Called by: lfodifrpt.f
C
C****************************************************************
C
        integer function inck1vdif (inb1, nb1, ksw)

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
        logical chkfltr, check
c
c       ksw assignments: 1 - normal
c                        2 - e-o-f inb1
c                        3 - e-o-f inb2
c                        4 - e-o-f inb1 and inb2
c

  100   if (ksw .eq. 1 .or. ksw .eq. 3) then
           inb1 = inb1 + 1
           if (inb1 .gt. ntot_alf) then
              ksw = ksw + 1
              nb1 = ntot + 1
           else
              nb1 = alf2inp(inb1)
              check = chkfltr (arcnam(jarzn(nb1)), zone(nb1), '***', 
     &                         base(nb1), '**', 0)
              if (.not. check) go to 100
           endif
        endif
        inck1vdif = ksw
        return
        end
