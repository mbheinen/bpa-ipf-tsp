C    %W% %G%
      subroutine dbgwrc (text,cval)
      character text*(*), cval*(*)

      include 'tspinc/blkcom1.inc'
C 
C     Writes the TEXT and a char string value to the debug file 
C     (unit l13)
C
      write (l13,'(1X,A,A)') text,cval
      return
      end
