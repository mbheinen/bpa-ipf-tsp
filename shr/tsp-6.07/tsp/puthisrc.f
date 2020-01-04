C    %W% %G%
c     -                           puthisrc.f

      subroutine puthisrc (irectp, idesc, numwords, charr)
        integer irectp,idesc,numwords
        character charr(*) 
c     -    Writes an entire record with a single call when the data is
c          an array of characters which will fill up numwords 4-byte
c          chunks.  This routine is needed for compilers which can't 
c          pass down a character argument to a numeric parameter.
c
      call puthisi (irectp,1)
      call puthisi (idesc,1)
      call puthisi (numwords,1)
      call puthisc (charr,numwords)
      return
      end
c   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-

