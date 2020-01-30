C    @(#)kmpqpn.f	20.3 2/13/96
        function kmpqpn(m,n)
C
      include 'ipfinc/parametr.inc'

      include 'ipfinc/komps.inc'
      include 'ipfinc/optim1.inc'
C
        kmpqpn=kqpent(1,m)-kqpent(1,n)
        if(kmpqpn.eq.0 .and. m.ne.n) ndup=1
        return
        end
