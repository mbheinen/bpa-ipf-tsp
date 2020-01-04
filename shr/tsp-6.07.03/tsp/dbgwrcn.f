C    %W% %G%
      subroutine dbgwrcn (text,cval,nchars)
      character text*(*), cval*(*)
      
      include 'tspinc/blkcom1.inc'
C 
C     - Writes the TEXT and a char string value to the debug file 
C      (unit l13)
C
      lna = nchars
      lnb = len (cval)
      if (lna .gt. lnb) lna = lnb
      write (l13,'(1X,4A)') text,'{',cval(1:lna),'}'
      return
      end
