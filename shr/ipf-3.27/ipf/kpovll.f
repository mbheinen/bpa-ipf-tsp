C    @(#)kpovll.f	20.3 2/13/96
      function kpovll(i,j)
 
C *** This function determines the sort key for overloaded branch      *
C *** array OLBR.                                                      *
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/anlys.inc'
      include 'ipfinc/arcntl.inc'
      include 'ipfinc/area.inc'
      include 'ipfinc/branch.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/busanl.inc'


      common /sortsw/ sortsw, vltsrt(MAXBUS)
      integer sortsw, vltsrt, ptr1,ptr2
 
      character own1 * 3, own2 * 3
 
      ptr1 = kolbr(1,i)
      j1 = iabs(brnch_ptr(ptr1))

      ptr2 = kolbr(1,j)
      j2 = iabs(brnch_ptr(ptr2))

      if (sortsw .eq. 1) then   !         Sort by ownership-bus
         write (own1,100) kbrnch(3,j1)
  100    format (a3)
         write (own2,100) kbrnch(3,j2)
         kpovll = kompr (own1,own2,kpovll)
         if (kpovll .eq. 0) then
            kpovll = inp2alf(kx(ptr1)) - inp2alf(kx(ptr2))
         endif
 
      else if (sortsw .eq. 2) then   !     Sort by bus
         kpovll = inp2alf(kx(ptr1)) - inp2alf(kx(ptr2))
 
      else if (sortsw .eq. 3) then   !     Sort by zone-bus
         k1 = kx(ptr1)
         k2 = kx(ptr2)
         kpovll = kompr (zone(k1),zone(k2),kpovll)
         if (kpovll .eq. 0) then
            m1 = ky(ptr1)
            m2 = ky(ptr2)
            kpovll = kompr (zone(m1),zone(m2),kpovll)
            if (kpovll.eq.0) kpovll=inp2alf(kx(ptr1))-inp2alf(kx(ptr2))
         endif
 
      else if (sortsw .eq. 4) then    !    Sort by area-bus
         k1 = kx(ptr1)
         k2 = kx(ptr2)
         l1 = jarzn(k1)
         l2 = jarzn(k2)
         kpovll = kompr (arcnam(l1),arcnam(l2),kpovll)
         if (kpovll .eq. 0) then
            m1 = ky(ptr1)
            m2 = ky(ptr2)
            l1 = jarzn(k1)
            l2 = jarzn(k2)
            kpovll = kompr (arcnam(l1),arcnam(l2),kpovll)
            if (kpovll.eq.0) kpovll=inp2alf(kx(ptr1))-inp2alf(kx(ptr2))
         endif
      endif
      return
      end
