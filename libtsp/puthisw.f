C    %W% %G%
c     -                           puthisw.f

      subroutine puthisw (fltval)
        real fltval
c     -  Writes a single word to the record buffer but does not advance 
c        the buffer pointer
c 
      include 'tspinc/dahist.inc'
c
      dabuff(posptr) = fltval
      return
      end

