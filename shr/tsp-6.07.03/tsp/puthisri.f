C    %W% %G%
c     -                           puthisri.f

      subroutine puthisri (irectp, idesc, numwords, intarr)
        integer irectp,idesc,numwords
        integer intarr(*) 
c     -    Writes an entire record with a single call.  The record's
c          data is integer, but can be passed down to a float 
c          parameter.
c
      call puthisrf (irectp, idesc, numwords, intarr)
      return
      end      

