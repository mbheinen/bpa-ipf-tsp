C    @(#)swpidx.f	20.3 2/13/96
        subroutine swpidx(m,n)
C
      include 'ipfinc/parametr.inc'
      include 'ipfinc/ikk.inc'
C
        do 100 i=1,3
           itemp=indx(i,m)
           indx(i,m)=indx(i,n)
           indx(i,n)=itemp
  100   continue
        return
        end
