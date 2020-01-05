C    @(#)swpari.f	20.3 2/13/96
      subroutine swpari (m,n)
c
c       swap arcint(1,*) and arcint (2,*)
c
      include 'ipfinc/parametr.inc'
      include 'ipfinc/arcntl.inc'
c
        character tempc*10

        do 100 i = 1,2
           tempc = arcint(i,m)
           arcint(i,m) = arcint(i,n)
           arcint(i,n) = tempc
  100   continue

        temp = arcinp(m)
        arcinp(m) = arcinp(n)
        arcinp(n) = temp
        return
        end
