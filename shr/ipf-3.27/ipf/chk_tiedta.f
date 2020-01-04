C    @(#)chk_tiedta.f	20.3 12/21/96
      integer function chk_tiedta(ptr, field, count, out_buffer)
      integer ptr, field, count
      character out_buffer(10)*120

C     This subroutine checks ARCINP(*, PTR) extensively for data errors.

      include 'ipfinc/parametr.inc'
      include 'ipfinc/arcntl.inc' 
      include 'ipfinc/area.inc'
      include 'ipfinc/prt.inc'

      chk_tiedta = 0
      if ((field .eq. 0 .or. field .eq. 27) .and.
     &    (arcinp(ptr) .lt. -6000.0 .or. arcinp(ptr) .gt. 6000.0)) then    
        chk_tiedta = 1
        count = min0 (count + 1, 10)
        write (out_buffer(count), 10020) arcinp(ptr)
10020   format (' Export (', f10.1, ') < -6000 or > 6000 ')
      endif

  220 return
      end
