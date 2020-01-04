C    @(#)find_ara.f	20.4 7/18/96
      integer function find_ara (areaname)
      character areaname *(*)
C
C     This function searches for AREA in array ARENAM.
C
      include 'ipfinc/parametr.inc'

      include 'ipfinc/blank.inc'
      include 'ipfinc/arcntl.inc'
 
      integer first, last, size
      logical found

      size = len (areaname)
      first = 1
      last = ntotc
      find_ara = 0
      found = .false.
 
      do while (first .le. last .and. .not.found)
         i = (first+last)/2
         komp = kompr (areaname(1:size), arcnam(i)(1:size), komp)
         if (komp .eq. 0) then
            found = .true.
            find_ara = i
         else if (komp .gt. 0) then
            first = i + 1
         else 
            last = i - 1
         endif
      enddo
      return
      end
