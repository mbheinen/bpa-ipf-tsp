C    @(#)swapef.f	20.3 2/13/96
        subroutine swapef(i,j)
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/anlys.inc'
      include 'ipfinc/busanl.inc'
 
        do 100 k=1,13
           lxxx = leff(k,i)
           leff(k,i) = leff(k,j)
           leff(k,j) = lxxx
  100   continue
        return
        end
