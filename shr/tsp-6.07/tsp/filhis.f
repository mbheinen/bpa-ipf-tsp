C    %W% %G%
c     -                           filhis.f

      subroutine filhis 
c 
c     -  forces the buffer to be zero-filled and written to the dir-acc
c        file.  If pointer at position one, then buffer is empty and no
c        writing is required.
c 
      include 'tspinc/blkcom1.inc'
      include 'tspinc/dahist.inc'
c
      if (posptr .gt. 1) then 
        do la = posptr,recsiz
          dabuff(la) = 0.0
        enddo
        write (l8,rec=recptr) (dabuff(lb), lb = 1,recsiz)
        recptr = recptr + 1
        posptr = 1
      endif
c
      return
      end
c     -  end of filhis

