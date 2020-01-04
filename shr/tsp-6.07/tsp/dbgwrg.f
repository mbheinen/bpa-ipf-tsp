C    %W% %G%
      subroutine dbgwrg (text,fval)
      character text*(*)
      real fval

      include 'tspinc/blkcom1.inc'
C 
C     Writes the TEXT and a floating point value to the debug file 
C     (unit l13) using a G floating-point format
C
      write (l13,'(1x,a,g16.8)') text, fval
      return
      end
