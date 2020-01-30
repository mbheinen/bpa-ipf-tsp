C    @(#)kpovuv.f	20.3 2/13/96
      function kpovuv(i,j)
 
C *** This function determines the sort key for over/under voltage     *
C *** array ABUS.                                                      *
 
      include 'ipfinc/parametr.inc'
 
      include 'ipfinc/anlys.inc'
      include 'ipfinc/arcntl.inc'
      include 'ipfinc/area.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/busanl.inc'

      common /sortsw/ sortsw, vltsrt(MAXBUS)
      integer sortsw, vltsrt
 
      ix = vltsrt(i)
      jx = vltsrt(j)
 
      voltpu = abus(4,ix)
      dvkv1 = dim (voltpu,abus(15,ix)) - dim(abus(12,ix),voltpu)
      if ( abs(dvkv1) .lt. 0.0005 ) dvkv1 = 0.0
 
      voltpu = abus(4,jx)
      dvkv2 = dim (voltpu,abus(15,jx)) - dim(abus(12,jx),voltpu)
      if ( abs(dvkv2) .lt. 0.0005 ) dvkv2 = 0.0
C ***                                                                  *
C *** Sort non-zero voltage violations first                           *
C ***                                                                  *
      if (dvkv1 .ne. 0.0 .and. dvkv2 .ne. 0.0) then
 
         k1 = kabus(1,ix)
         k2 = kabus(1,jx)
 
         if (sortsw .eq. 1) then   !   Sort by ownership-bus
            kpovuv = kompr (owner(k1),owner(k2),kpovuv)
            if (kpovuv .eq. 0) then
               kpovuv = k1 - k2
            endif
 
         else if (sortsw .eq. 2) then  !  Sort by bus
            kpovuv = k1 - k2
 
         else if (sortsw .eq. 3) then  !  Sort by zone-bus
            kpovuv = kompr (zone(k1),zone(k2),kpovuv)
            if (kpovuv .eq. 0) kpovuv = k1 - k2
 
         else if (sortsw .eq. 4) then  !  Sort by area-bus
            l1 = jarzn(k1)
            l2 = jarzn(k2)
            kpovuv = kompr (arcnam(l1),arcnam(l2),kpovuv)
            if (kpovuv .eq. 0) kpovuv = k1 - k2
 
         endif
 
 
      else if (dvkv1 .ne. 0.0 .and. dvkv2 .eq. 0.0) then
         kpovuv = -1
 
      else if (dvkv1 .eq. 0.0 .and. dvkv2 .ne. 0.0) then
         kpovuv = 1
 
      else
         kpovuv = 0
 
      endif
      return
      end
