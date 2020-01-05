C    @(#)get_volt.f	20.1 5/27/98
      subroutine get_volt (nb, vr, vi, pload, qload, zn, own)
      integer nb
      character zn*2, own*3
      real vr, vi
C
C     This function returns the area number for bus nb.
C
      include 'ipfinc/parametr.inc'

      include 'ipfinc/blank.inc'
      include 'ipfinc/bus.inc'
      include 'ipfinc/alpha.inc'
 
      kt = inp2opt(nb)
      vr = e(kt)
      vi = f(kt)
      if (ntypu(kt) .eq. 5 .or. ntypu(kt) .eq. 12) then
        pload = 0.0
        qload = 0.0
      else
        pload = ploadu(kt) * bmva
        qload = qloadu(kt) * bmva
      endif
      zn = zone(nb)
      own = owner(nb)
  900 return
      end
