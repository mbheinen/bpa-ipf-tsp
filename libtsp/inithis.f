C    %W% %G%
c     -                           inithis.f

      subroutine inithis (functn)
        character functn*4
c
c     -  Sets global record size and buffer & record pointers to uno 
c        before the first write to buffer operation.
c     -  If reading the file, the subroutine brings the first record 
c        into the local buffer.
c     -  If writing the file, put a "soft" e-o-file marker on the first
c        in case the application crashes early on.
c 
      include 'tspinc/blkcom1.inc'
      include 'tspinc/dahist.inc'
c
      recsiz = 128
      posptr = 1
      recptr = 1
      if (functn .eq. 'READ') then
        read (l8,rec=recptr) (dabuff(lc),lc = 1,recsiz)
      endif
      if (functn .eq. 'WRIT') then
        dabuff(1) = -1  
        write (l8,rec=recptr) (dabuff(lc),lc = 1,recsiz)
      endif
      return
      end
c     -  end of inithis

