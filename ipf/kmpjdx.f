C    @(#)kmpjdx.f	20.3 2/13/96
        function kmpjdx(m,n)
C
      include 'ipfinc/parametr.inc'
      include 'ipfinc/ikk.inc'
C
        kmpjdx=jndx(1,m)-jndx(1,n)
        if (kmpjdx.eq.0) kmpjdx=jndx(2,m)-jndx(2,n)
        if (kmpjdx.eq.0) kmpjdx=jndx(3,m)-jndx(3,n)
        return
        end
