C    %W% %G%
      subroutine dbgwrl (text,qval)
        character text*(*)
        logical qval
C 
C     - Writes the TEXT and a logical value to the debug file (unit 13)
C
      write (13,'(1X,A,L2)') text,qval
      return
      end
