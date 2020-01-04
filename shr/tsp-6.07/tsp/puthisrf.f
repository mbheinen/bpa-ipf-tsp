C    %W% %G%
c     -                           puthisrf.f

      subroutine puthisrf (irectp, idesc, numwords, fltarr)
        integer irectp,idesc,numwords
        real fltarr(*) 
c     -    Writes an entire record with a single call.  The record
c          structure is record type (1 long), description (1 long), 
c          num of 4-byte (floats) chunks to write (1 long), data array 
c          (numwords floats).  The var fltarr can be of either float
c          or integer type; it will be written to the d-a file with 
c          no bit pattern changes.
      call puthisi (irectp,1)
      call puthisi (idesc,1)
      call puthisi (numwords,1)
      call puthisf (fltarr,numwords)
      return
      end
