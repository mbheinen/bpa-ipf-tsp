C    @(#)swpqpn.f	20.3 2/13/96
        subroutine swpqpn(m,n)
C
      include 'ipfinc/parametr.inc'
      include 'ipfinc/optim1.inc'
C
        do 100 i=1,10
           itemp=kqpent(i,m)
           kqpent(i,m)=kqpent(i,n)
           kqpent(i,n)=itemp
  100   continue
        return
        end
