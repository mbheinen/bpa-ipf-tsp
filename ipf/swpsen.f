C    @(#)swpsen.f	20.3 2/13/96
        subroutine swpsen(m,n)
C
      include 'ipfinc/parametr.inc'
      include 'ipfinc/optim1.inc'
C
        do 100 i=1,3
           itemp=ksen(i,m)
           ksen(i,m)=ksen(i,n)
           ksen(i,n)=itemp
  100   continue
        return
        end
