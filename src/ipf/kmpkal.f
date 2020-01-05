C    @(#)kmpkal.f	20.3 2/13/96
      function kmpkal(m,n)
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/area.inc'
      include 'ipfinc/sort.inc'

      kmpkal=kaloc(m)-kaloc(n)
      if (kmpkal.eq.0) kmpkal= iabs(nsysno(m)) - iabs(nsysno(n))
      return
      end
