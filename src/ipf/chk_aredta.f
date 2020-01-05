C    @(#)chk_aredta.f	20.3 12/21/96
      integer function chk_aredta(ptr, field, count, out_buffer)
      integer ptr, field, count
      character out_buffer(10)*120

C     This subroutine checks AREA(*,PTR) extensively for data errors.

      include 'ipfinc/parametr.inc'
      include 'ipfinc/blank.inc' 
      include 'ipfinc/arcntl.inc' 
      include 'ipfinc/area.inc'
      include 'ipfinc/prt.inc'

      chk_aredta = 0
      if ((field .eq. 0 .or. field .eq. 27) .and.
     &    (arcnet(ptr)*bmva .lt. -6000.0 .or. 
     &     arcnet(ptr)*bmva .gt. 6000.0)) then
        chk_aredta = 1
        count = min0 (count + 1, 10)
        write (out_buffer(count), 10010) arcnet(ptr) * bmva
10010   format (' Export (', f10.1, ') < -6000 or > 6000 ')
      endif

      return
      end
