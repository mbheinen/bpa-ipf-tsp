C    %W% %G%
      logical function dbghere (subname)
      implicit none
      character subname*(*)

c     Indicates whether the subroutine "subname" is to be debugged.

      common /dbgset/ anydbg, sublist(20)
      logical anydbg
      character sublist*8 
c
      integer la, ln
c
      dbghere = .false.
      if (.not. anydbg) return
c
      ln = len (subname)
      do la = 1,20
        if (sublist(la) .eq. ' ') return
        if (subname(1:ln) .eq. sublist(la)) then 
          dbghere = .true. 
          return
        endif
      enddo
c
      return
      end
