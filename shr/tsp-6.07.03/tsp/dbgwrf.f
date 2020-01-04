C    %W% %G%
      subroutine dbgwrf (text,fval)
      character text*(*)
      real fval

      include 'tspinc/blkcom1.inc'
C 
C     - Writes the TEXT and a floating point value to the debug file 
C       (unit l13)
C
      write (l13,'(1x,a,f18.8)') text,fval
      return
      end
