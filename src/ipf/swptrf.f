C    @(#)swptrf.f	20.3 2/13/96
        subroutine swptrf (m,n)
C
      include 'ipfinc/parametr.inc'

      include 'ipfinc/arcntl.inc'
      include 'ipfinc/prt.inc'
      include 'ipfinc/transf.inc'
 
        do 100 i = 1,22
           ktemp = kfdata(i,m)
           kfdata(i,m) = kfdata(i,n)
           kfdata(i,n) = ktemp
  100   continue
 
        return
        end
