C    %W% %G%
c     -                           puthisf.f

      subroutine puthisf (fltarr, kount)
        real fltarr(*)
        integer kount
c     -    Writes out kount floats from the fltarr array to the 
c          buffer for da file unit L8 and writes a record if the array
c          fills up more than one record.
c     -    Convention is to have position pointer point to place in 
c          buffer where next piece of data will go, and record pointer 
c          to place in file where next record will go.  
c     -    It is possible for the position pointer to be one higher 
c          than the record size.
c 
      include 'tspinc/blkcom1.inc'
      include 'tspinc/dahist.inc'
c
c     -  if buffer full, send it to file
      if (posptr .gt. recsiz) then
        write (l8,rec=recptr) (dabuff(lc),lc = 1,recsiz)
        recptr = recptr + 1
        posptr = 1
      endif
c
      lb = posptr
      iend = posptr + kount - 1
      if (iend .le. recsiz) then 
c       -  can fit data in rest of buffer
        do la = 1,kount
          dabuff(lb) = fltarr(la)
          lb = lb + 1
        enddo 
      else 
c       -  data overflows buffer.  Must write out one or more records
c       -  first fill out buffer and write once
        la = 1
        do lb = posptr,recsiz 
          dabuff(lb) = fltarr(la)
          la = la + 1
        enddo 
        write (l8,rec=recptr) (dabuff(lc),lc = 1,recsiz)
        recptr = recptr + 1
c       -  If writing several records, can put incoming data directly 
c          into file  
        ka = la  
        kb = ka + recsiz - 1
c       -  write full records only
        do while (kb .lt. kount) 
          write (l8,rec=recptr) (fltarr(lc),lc = ka,kb)
          recptr = recptr + 1
          ka = ka + recsiz
          kb = kb + recsiz
        enddo 
c       -  not enough data left,  stick into buffer
        lb = 1
        do la = ka,kount
          dabuff(lb) = fltarr(la)
          lb = lb + 1
        enddo
      endif 
c
      posptr = lb
c     -  It's possible for posptr to be one past the last position here
      if (posptr .gt. recsiz) then
        write (l8,rec=recptr) (dabuff(lc),lc = 1,recsiz)
        recptr = recptr + 1
        posptr = posptr - recsiz
      endif
c 
      return
      end
c     -  end of puthisf

