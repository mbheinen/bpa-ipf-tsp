C    @(#)kmpecn.f	20.3 2/13/96
        function kmpecn(m,n)
C
      include 'ipfinc/parametr.inc'

      include 'ipfinc/komps.inc'
      include 'ipfinc/optim1.inc'
C
        kmpecn=kecon(1,m) - kecon(1,n)
        if (kmpecn.eq.0.and.m.ne.n) ndup=1
        return
        end
