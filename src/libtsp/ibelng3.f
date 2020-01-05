C    %W% %G%
      logical function ibelng3 (ivar,ival1,ival2,ival3)
        implicit none 
        integer ivar, ival1, ival2, ival3
c     -  Tests whether ivar is one of the three ival's.  
c     -  Function returns true if it is, false if not. 
c     -  Newly created on Oct/29/92
C
      ibelng3 = .true.
      if (ivar .eq. ival1) return
      if (ivar .eq. ival2) return
      if (ivar .eq. ival3) return
c     -
      ibelng3 = .false.
      return
      end
