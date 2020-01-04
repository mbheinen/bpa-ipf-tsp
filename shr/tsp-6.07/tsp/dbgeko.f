C    %W% %G%
      subroutine dbgeko (text)
      character text*(*)

      include 'tspinc/blkcom1.inc'
c 
c     Writes the TEXT to the debug file (unit 13)
c
      write (l13,'(1x,a)') text
      return
      end
