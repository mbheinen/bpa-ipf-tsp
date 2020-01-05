C    @(#)find_xara.f	20.4 7/18/96
      integer function find_xara (areaname) 
C                                                                       
C     This function searches for AREA in array OARENAM.                  
C                                                                       
      include 'ipfinc/parametr.inc'

      include 'ipfinc/alt_case.inc'
                                                                        
      character areaname *(*)                                               
      integer first, last
      logical found

      first = 1
      last = ontotc
      find_xara = 0
      found = .false.
                                                                          
      do while (first .le. last .and. .not.found)
         i = (first+last)/2
         komp = kompr (areaname, oarcnam(i), komp)
         if (komp .eq. 0) then
            found = .true.
            find_xara = i
         else if (komp .gt. 0) then
            first = i + 1
         else 
            last = i - 1
         endif
      enddo
      return
      end                                                               
