C    @(#)kmpsen.f	20.3 2/13/96
        function kmpsen (m,n)
C
      include 'ipfinc/parametr.inc'

      include 'ipfinc/optim1.inc'
C
        kmpsen = ksen(1,m) - ksen(1,n)
        return
        end
