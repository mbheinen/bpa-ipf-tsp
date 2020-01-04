C    %W% %G%
      subroutine dbgeko2 (text1,text2)
      character text1*(*),text2*(*)

      include 'tspinc/blkcom1.inc'
C 
C     Writes two text strings to the debug file (unit 13)
C
      write (l13,'(1x,2a)') text1,text2
      return
      end
