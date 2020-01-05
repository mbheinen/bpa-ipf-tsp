C    @(#)swptrt.f	20.3 2/13/96
        subroutine swptrt (m,n)
C
      include 'ipfinc/parametr.inc'

      include 'ipfinc/arcntl.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/transf.inc'
 
        do 100 i = 1,8
           ktemp = ktdata(i,m)
           ktdata(i,m) = ktdata(i,n)
           ktdata(i,n) = ktemp
  100   continue
        return
        end
