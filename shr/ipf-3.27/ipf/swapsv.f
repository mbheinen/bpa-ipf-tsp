C    @(#)swapsv.f	20.3 2/13/96
        subroutine swapsv (m,n)
C
      include 'ipfinc/sensit.inc'
C
        ktemp = maxsrt(m)
        maxsrt(m) = maxsrt(n)
        maxsrt(n) = ktemp
        return
        end
