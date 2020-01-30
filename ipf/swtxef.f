C    @(#)swtxef.f	20.3 2/13/96
        subroutine swtxef(i,j)

      include 'ipfinc/parametr.inc'

      include 'ipfinc/anlys.inc'
      include 'ipfinc/busanl.inc'
 
        do 100 k=1,9
           lxxx=ltxeff(k,i)
           ltxeff(k,i)=ltxeff(k,j)
           ltxeff(k,j)=lxxx
  100   continue
        return
        end
