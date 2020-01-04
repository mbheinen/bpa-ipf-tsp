C    %W% %G%
C****************************************************************
C
C   File: inckrvdif.f
C
C   Purpose: Integer function to increment indices.
C            TYPE = 1 : increment bus k1 indices
C                   2 : increment branch k2 indices
C                   3 : increment bus k2 indices
C                   4 : increment branch k2 indices
C
C   Author: Walt Powell  Date: 14 December 1992
C   Called by: lfodifrpt.f
C
C****************************************************************
C
        integer function incrvdif (type, in1, in2, in3, switch)
        integer type, switch

        include 'ipfinc/parametr.inc'

        include 'ipfinc/blank.inc'
        include 'ipfinc/bus.inc'
        include 'ipfinc/arcntl.inc'
        include 'ipfinc/area.inc'
        include 'ipfinc/owncom.inc'
        include 'ipfinc/sortuvov.inc'
        include 'ipfinc/branch.inc'
        include 'ipfinc/prt.inc'
        include 'ipfinc/alt_case.inc'

	external chkfltr
        logical chkfltr, check
C
C       Entry of integer function to increment bus k1 indices
C
        if (type .eq. 1) then

c          ksw = switch
c          inb1 = in1
c          nb1 = in2
c
c          ksw assignments: 1 - normal
c                        2 - e-o-f inb1 (in1)
c                        3 - e-o-f inb2
c                        4 - e-o-f inb1 (in1) and inb2
c
  100      if (switch .eq. 1 .or. switch .eq. 3) then
              in1 = in1 + 1
              if (in1 .gt. ntot_alf) then
                 switch = switch + 1
                 in2 = ntot + 1
              else
                 in2 = alf2inp(in1)
                 check = chkfltr (arcnam(jarzn(in2)), zone(in2), '***', 
     &                            base(in2), '* ', 0)
                 if (.not. check) go to 100
              endif
           endif
c
C       Entry of integer function to increment branch indices m1 for
c       bus k1
C
        else if (type .eq. 2) then

c          lsw = switch
c          nbr1 = in1
c          nb1 = in2
c          m1 = in3
c
c          lsw assignments: 1 - normal
c                           2 - e-o-f nbr1 (in1)
c                           3 - e-o-f nbr2
c                           4 - e-o-f nbr1 (in1) and nbr2
c
  110      if (switch .eq. 1 .or. switch .eq. 3) then
              in1 = brnch_nxt(in1)
              if (in1 .gt. 0) then
                 in3 = ky(in1)
                 if (inp2alf(in2) .lt. inp2alf(in3)) then
                 else if (chkfltr (arcnam(jarzn(in3)), zone(in3), '***',
     1                             base(in3), '* ', 0)) then
                    go to 110
                 endif
              else
                 switch = switch + 1
              endif
           endif
C
C       Entry of integer function to increment bus k2 indices
C
        else if (type .eq. 3) then

c          ksw = switch
c          inb2 = in1
c          nb2 = in2
c
c          ksw assignments: 1 - normal
c                           2 - e-o-f inb1 
c                           3 - e-o-f inb2(in1)
c                           4 - e-o-f inb1 and inb2 (in1)
c
  120      if (switch .eq. 1 .or. switch .eq. 2) then
              in1 = in1 + 1
              if (in1 .gt. ontot) then
                 switch = switch + 2
                 in2 = ontot + 1
              else
                 in2 = oalf2inp(in1)
                 check = chkfltr (oarcnam(oarzn(in2)), oldzone(in2), 
     &                            '***', oldbase(in2), '* ', 0)
                 if (.not. check) go to 120
              endif
           endif
c
C       Entry of integer function to increment branch indices m2 for
c       bus k2
C
        else if (type .eq. 4) then

c          lsw = switch
c          nbr2 = in1
c          nb2 = in2
c          m2 = in3
c
c          lsw assignments: 1 - normal
c                           2 - e-o-f nbr1
c                           3 - e-o-f nbr2 (in1)
c                           4 - e-o-f nbr1 and nbr2 (in1)
c
  130      if (switch .eq. 1 .or. switch .eq. 2) then
              in1 = obrnch_nxt(in1)
              if (in1 .gt. 0) then
                 in3 = oky(in1)
                 if (oinp2alf(in2) .lt. oinp2alf(in3)) then
                 else if (chkfltr (oarcnam(oarzn(in3)), oldzone(in3), 
     &                             '***', oldbase(in3), '* ', 0)) then
                    go to 130
                 endif
              else
                 switch = switch + 2
              endif
           endif

        endif
        incrvdif = switch
        return
        end
