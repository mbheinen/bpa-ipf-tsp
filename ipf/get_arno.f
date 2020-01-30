C    @(#)get_arno.f	20.1 5/27/98
      integer function get_arno (nb)
      integer nb
C
C     This function returns the area number for bus nb.
C
      include 'ipfinc/parametr.inc'

      include 'ipfinc/arcntl.inc'
      include 'ipfinc/area.inc'
 
      if (nb .eq. 0) then
        get_arno = 0
      else
        get_arno = jarzn(nb)
      endif

  900 return
      end
