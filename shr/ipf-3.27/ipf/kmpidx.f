C    @(#)kmpidx.f	20.3 2/13/96
        function kmpidx(m,n)
C
      include 'ipfinc/parametr.inc'
      include 'ipfinc/ikk.inc'
C
        kmpidx=indx(1,m)-indx(1,n)
        if(kmpidx.eq.0) kmpidx=indx(2,m)-indx(2,n)
        if(kmpidx.eq.0) kmpidx=indx(3,m)-indx(3,n)
        return
        end
