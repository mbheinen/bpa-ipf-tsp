C    @(#)swpjdx.f	20.3 2/13/96
        subroutine swpjdx(m,n)
C
      include 'ipfinc/parametr.inc'
      include 'ipfinc/ikk.inc'
C
        do 100 i=1,3
           itemp=jndx(i,m)
           jndx(i,m)=jndx(i,n)
           jndx(i,n)=itemp
  100   continue
        return
        end
