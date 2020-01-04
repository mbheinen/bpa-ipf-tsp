C    @(#)areasr.f	20.3 2/13/96
      integer function areasr (area)
      character area *(*)
C
C     This function searches for AREA in array ARCNAM.
C
      include 'ipfinc/parametr.inc'

      include 'ipfinc/arcntl.inc'
      include 'ipfinc/blank.inc'
 
      areasr = 0
      do i = 1,ntotc
         if (arcnam(i) .eq. area) then
            areasr = i
            go to 900
         endif
      enddo

  900 return
      end
