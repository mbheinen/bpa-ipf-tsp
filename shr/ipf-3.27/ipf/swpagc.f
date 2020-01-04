C    @(#)swpagc.f	20.3 2/13/96
        subroutine swpagc (m,n)
C
      include 'ipfinc/parametr.inc'
      include 'ipfinc/agc.inc'
C
        do 100 i = 1, 14
           itemp = kagc(i,m)
           kagc(i,m) = kagc(i,n)
           kagc(i,n) = itemp
  100   continue
 
        return
        end
