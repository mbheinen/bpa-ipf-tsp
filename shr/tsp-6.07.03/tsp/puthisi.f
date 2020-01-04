C    %W% %G%
c     -                           puthisi.f

      subroutine puthisi (intarr, kount)
        real intarr(*) 
        integer kount
c     -    Writes out kount integers from the intarr array to the 
c          buffer for da file unit L8 and writes a record if the array
c          fills up more than one record.
c     -    Intarr is declared real so as to directly copy into the
c          real buffer.
c 
      call puthisf (intarr, kount)
      return
      end
c     -  end of puthisi

