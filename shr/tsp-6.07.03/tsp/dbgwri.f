C    %W% %G%
      subroutine dbgwri (text,ival)
      character text*(*)
      integer ival

      include 'tspinc/blkcom1.inc'
C 
C     Writes the TEXT and an integer value to the debug file (unit l13)
C
      write (l13,'(1x,a,i7)') text, ival
      return
      end
