C    @(#)swpkl2.f	20.3 2/13/96
        subroutine swpkl2(m,n)
C
      include 'ipfinc/dcsrt.inc'
C
        itemp=kolum(m)
        kolum(m)=kolum(n)
        kolum(n)=itemp
        return
        end
