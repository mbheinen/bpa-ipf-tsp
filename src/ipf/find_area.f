C    @(#)find_area.f	20.1 5/27/98
      integer function find_area (nb)
      integer nb
C
C     This function returns the area number for bus nb.
C
      include 'ipfinc/parametr.inc'

      include 'ipfinc/arcntl.inc'
      include 'ipfinc/area.inc'
 
      if (nb .eq. 0) then
        find_area = 0
      else
        find_area = jarzn(nb)
      endif

  900 return
      end
