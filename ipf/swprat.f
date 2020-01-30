C    @(#)swprat.f	20.3 2/13/96
        subroutine swprat (i,j)
C
C       swap the default ratings in array ZRAT.
C
        include 'ipfinc/dflrat.inc'
C
        do 100 k = 1,3
           xrat = zrat(k,i)
           zrat(k,i) = zrat(k,j)
           zrat(k,j) = xrat
  100   continue
        return
        end
