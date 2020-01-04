C    @(#)kpovlt.f	20.3 2/13/96
      function kpovlt(i,j)
 
C *** This function determines the sort key for overloaded transformer *
C *** array OLTR.                                                      *
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/anlys.inc'
      include 'ipfinc/arcntl.inc'
      include 'ipfinc/area.inc'
      include 'ipfinc/branch.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/busanl.inc'

      common /sortsw/ sortsw, vltsrt(MAXBUS)
      integer sortsw, vltsrt
 
      character own1 * 3, own2 * 3
 
      j1 = koltr(1,i)
      j2 = koltr(1,j)
 
      if (sortsw .eq. 1) then  !          Sort by ownership-bus
         nbr = iabs (brnch_ptr(j1))
         write (own1,100) kbrnch(3,nbr)
  100    format (a3)
         nbr = iabs (brnch_ptr(j2))
         write (own2,100) kbrnch(3,nbr)
         kpovlt = kompr (own1,own2,kpovlt)
         if (kpovlt .eq. 0) then
            kpovlt = inp2alf(kx(j1)) - inp2alf(kx(j2))
         endif
         if (kpovlt .eq. 0) then
            kpovlt = inp2alf(ky(j1)) - inp2alf(ky(j2))
         endif
         if (kpovlt .eq. 0) then
            kpovlt = kompr (brid(j1), brid(j2), junk)
         endif
         if (kpovlt .eq. 0) then
            kpovlt = isign (brsect(j1), brnch_ptr(j1)) 
     &             - isign (brsect(j2), brnch_ptr(j2))
         endif
 
      else if (sortsw .eq. 2) then   !    Sort by bus
         kpovlt = inp2alf(kx(j1)) - inp2alf(kx(j2))
         if (kpovlt .eq. 0) then
            kpovlt = inp2alf(ky(j1)) - inp2alf(ky(j2))
         endif
         if (kpovlt .eq. 0) then
            kpovlt = kompr (brid(j1), brid(j2), junk)
         endif
         if (kpovlt .eq. 0) then
            kpovlt = isign (brsect(j1), brnch_ptr(j1)) 
     &             - isign (brsect(j2), brnch_ptr(j2))
         endif
 
      else if (sortsw .eq. 3) then   !    Sort by zone-bus
         k1 = kx(j1)
         k2 = kx(j2)
         kpovlt = kompr (zone(k1),zone(k2),kpovlt)
         if (kpovlt .eq. 0) then
            m1 = ky(j1)
            m2 = ky(j2)
            kpovlt = kompr (zone(m1),zone(m2),kpovlt)
            if (kpovlt .eq. 0) then
               kpovlt = inp2alf(kx(j1)) - inp2alf(kx(j2))
            endif
            if (kpovlt .eq. 0) then
               kpovlt = inp2alf(ky(j1)) - inp2alf(ky(j2))
            endif
            if (kpovlt .eq. 0) then
               kpovlt = kompr (brid(j1), brid(j2), junk)
            endif
            if (kpovlt .eq. 0) then
               kpovlt = isign (brsect(j1), brnch_ptr(j1)) 
     &                - isign (brsect(j2), brnch_ptr(j2))
            endif
         endif
      else if (sortsw .eq. 4) then   !    Sort by area-bus
         k1 = kx(j1)
         k2 = kx(j2)
         l1 = jarzn(k1)
         l2 = jarzn(k2)
         kpovlt = kompr (arcnam(l1),arcnam(l2),kpovlt)
         if (kpovlt .eq. 0) then
            m1 = ky(j1)
            m2 = ky(j2)
            l1 = jarzn(k1)
            l2 = jarzn(k2)
            kpovlt = kompr (arcnam(l1),arcnam(l2),kpovlt)
            if (kpovlt .eq. 0) then
               kpovlt = inp2alf(kx(j1)) - inp2alf(kx(j2))
            endif
            if (kpovlt .eq. 0) then
               kpovlt = inp2alf(ky(j1)) - inp2alf(ky(j2))
            endif
            if (kpovlt .eq. 0) then
               kpovlt = kompr (brid(j1), brid(j2), junk)
            endif
            if (kpovlt .eq. 0) then
               kpovlt = isign (brsect(j1), brnch_ptr(j1)) 
     &                - isign (brsect(j2), brnch_ptr(j2))
            endif
         endif
      endif
      return
      end
