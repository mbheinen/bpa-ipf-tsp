C    %W% %G%
c
c     This module consists of a set of subroutines for reading from 
c     the direct-access history file

c   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-

      subroutine gethisw (fltvar)
      real fltvar 

c     This subroutine reads the next word from the record buffer 
c     without advancing the position pointer
c 
      include 'tspinc/dahist.inc'
c  
      fltvar = dabuff(posptr)
      return
      end
 
c   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-

      subroutine gethisc (charr, kount)

c     This subroutine reads in kount 4-byte words into the charr 
c     array from the buffer for da file unit l8 and reads one or 
c     more following records if the array requires data beyond the 
c     current one.

c     By ANSI standard, a real param can't be passed up to a 
c     character argument, so equivalencing to char*4 must be done
c     for each incoming real.
c 
      character charr(*)*4, ch4*4
      integer kount
      real flt4
      equivalence (ch4,flt4)
c
      do la = 1,kount
        call gethisf (flt4, 1)
        charr(la) = ch4
      enddo
      return
      end

c   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-

      subroutine gethisi (intarr, kount)
      real intarr(*) 
      integer kount

c     This subroutine reads in kount integers from the intarr array 
c     from the buffer for da file unit l8 and reads one or more 
c     following records if the array requires data beyond the current 
c     one.

c     intarr is declared real so as to directly copy from the real 
c     buffer.
c 
      call gethisf (intarr, kount)
      return
      end

c   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-

      subroutine gethisf (fltarr, kount)
      real fltarr(*)
      integer kount

c     This subroutine reads in kount integers from the intarr array 
c     from the buffer for da file unit l8 and reads one or more 
c     following records if the array requires data beyond the current 
c     one.

c     The convention is to have record & position pointers point to 
c     file record & position in buffer next piece of data will 
c     come from. 
c     The minimun value for both pointers is 1. 
c 
      include 'tspinc/blkcom1.inc'
      include 'tspinc/dahist.inc'
c
      lb = posptr 
      iend = lb + kount - 1
      if (iend .le. recsiz) then 

c       All data we want resides in rest of buffer

        do la = 1,kount
          fltarr(la) = dabuff(lb) 
          lb = lb + 1
        enddo 
        posptr = lb
      else 

c       We want more data than buffer holds.  Must read in one or 
c       more records. 
c       First get rest of what's in buffer now and advance recptr

        la = 1
        lb = posptr
        do while (lb .le. recsiz)
          fltarr(la) = dabuff(lb)
          la = la + 1
          lb = lb + 1
        enddo

c       If reading several records, can put incoming data directly 
c       into caller's data array  

        ka = la  
        kb = ka + recsiz - 1

c       read full records only

        do while (kb .lt. kount) 
          recptr = recptr + 1
          read (l8,rec=recptr) (fltarr(lc),lc = ka,kb)
          ka = ka + recsiz
          kb = kb + recsiz
        enddo 

c       Only partial record needed, read into buffer first

        recptr = recptr + 1
        read (l8,rec=recptr) (dabuff(lc),lc = 1,recsiz)
        lb = 1
        do la = ka,kount
          fltarr(la) = dabuff(lb) 
          lb = lb + 1
        enddo
      endif 
      posptr = lb

c     if buffer "used up", read next one from file

      if (posptr .gt. recsiz) then
        recptr = recptr + 1
        posptr = posptr - recsiz
        read (l8,rec=recptr) (dabuff(lc),lc = 1,recsiz)
      endif
c
      return
      end

c   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-

      subroutine sethpos (recrd,postn) 
      integer recrd, postn
c
c     This subroutine Moves the dir-acc file position to the recrd + 
c     position pointer pair. 
c
      include 'tspinc/blkcom1.inc'
      include 'tspinc/dahist.inc'
c
      if (recrd .ne. recptr) then
        recptr = recrd
        read (l8,rec=recptr) (dabuff(lb), lb = 1,recsiz)
      endif 
      posptr = postn
      return
      end

c   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-

      subroutine shohpos (recrd,postn) 
      integer recrd, postn
c
c     This subroutine returns the current dir-acc file recrd + 
c     position pointer pair. 
c
      include 'tspinc/dahist.inc'
c
      recrd = recptr
      postn = posptr
      return
      end

c   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-

      subroutine calchpos (oldrec,oldpos,newrec,newpos,addby) 
      integer oldrec, oldpos, newrec, newpos, addby
c
c     This subroutine calculates a new file position from an older 
c     position + an adding offset. 
c
      include 'tspinc/dahist.inc'
c
      iaddrec = addby / recsiz
      iaddpos = addby - (iaddrec * recsiz)
      newrec = oldrec + iaddrec
      newpos = oldpos + iaddpos
      if (newpos .gt. recsiz) then
        newpos = newpos - recsiz
        newrec = newrec + 1
      endif
      return
      end

c   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-   -=-

      subroutine skiphis (numwrds)
      integer numwrds

c     This subroutine advances the current file pointer by nunmwrds.  
c     It is similar to gethisi, except no data is read.
c
      include 'tspinc/dahist.inc'
c
      call calchpos (recptr,posptr,inewrec,inewpos,numwrds)
      call sethpos (inewrec,inewpos)
      return
      end 
