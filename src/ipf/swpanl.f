C    @(#)swpanl.f	20.3 2/13/96
      subroutine swpanl   (m,n)

      include 'ipfinc/parametr.inc'
      include 'ipfinc/anlys.inc'
      save
C
      entry swapan (m,n)

      kps = lstown(m)
      lstown(m) = lstown(n)
      lstown(n) = kps
      return


      entry swpan2  (m,n)

      kps = lstvlt(m)
      lstvlt(m) = lstvlt(n)
      lstvlt(n) = kps
      return
      end
