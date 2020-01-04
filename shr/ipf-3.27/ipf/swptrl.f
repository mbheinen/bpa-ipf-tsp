C    @(#)swptrl.f	20.3 2/13/96
        subroutine swptrl (m,n)
C
      include 'ipfinc/parametr.inc'

      include 'ipfinc/arcntl.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/transf.inc'

        do 100 i = 1,14
           ktemp = kldata(i,m)
           kldata(i,m) = kldata(i,n)
           kldata(i,n) = ktemp
  100   continue
 
        return
        end
