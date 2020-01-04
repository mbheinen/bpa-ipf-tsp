C    @(#)chk_xdtdta.f	20.3 6/27/97
      integer function chk_xdtdta(ptr, field, count, out_buffer)
      integer field, ptr, count
      character out_buffer(10)*120

C     This subroutine checks XDATA(*,PTR) extensively for data errors.

      include 'ipfinc/parametr.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/xdata.inc'
      include 'ipfinc/prt.inc'

      integer error, status
      logical finished

      chk_xdtdta = 0
      do i = 1, 8
        j = 2 * i + 5
        kfield = 27 + 6 * i
        if ((field .eq. 0 .or. field .eq. kfield .or. 
     &       field .eq. kfield+1) .and.
     &      (xdata(j,ptr) .gt. 0.0)) then
           if (xdata(j,ptr) * xdata(j+1,ptr) .lt. -3000.0 .or. 
     &         xdata(j,ptr) * xdata(j+1,ptr) .gt. 3000.0) then
              chk_xdtdta = 1
              count = min0 (count + 1, 10)
              write (out_buffer(count), 10010) xdata(j,ptr), 
     &          xdata(j+1,ptr)
10010         format (' Steps (', f3.0, ') * Bshunt (', f7.1, 
     &          ') < -3000 or > 3000 ')
           endif
        endif
      enddo
      if (field .eq. 0) then
         nb = xdata(1,i)
         total_cap = amax1 (busdta(6,nb), 0.0)
         total_rek = amin1 (busdta(6,nb), 0.0)
         if (abs (total_rek-xdata(3,ptr)) + 
     &       abs (total_cap-xdata(4,ptr)) .gt. 1.0) then
            chk_xdtdta = 1
            count = min0 (count + 1, 10)
            write (out_buffer(count), 10020) xdata(3,ptr), total_rek, 
     &        xdata(4,ptr), total_cap
10020         format (' X-Reactors (', f6.0, ') < B-reactors (', f6.0, 
     &          ') or < X_Caps (', f6.0, ') < B-Caps (', f6.0, ')')
         endif
      endif
  220 return
      end
