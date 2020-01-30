C    @(#)swtxbs.f	20.3 2/13/96
      subroutine swtxbs (ia,ib)
c
c  Swaps the two OVEREX table pointers
c
      include 'ipfinc/parametr.inc'

      include 'ipfinc/overex.inc'
c
      iswp = iotxptr(ia)
      iotxptr(ia) = iotxptr(ib)
      iotxptr(ib) = iswp

      return
      end
