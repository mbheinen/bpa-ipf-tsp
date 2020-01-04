C    @(#)swpmdc.f	20.3 2/13/96
        subroutine swpmdc(m,n)
C
      include 'ipfinc/dcsrt.inc'
C
        itemp=mdc(m)
        mdc(m)=mdc(n)
        mdc(n)=itemp
        return
        end
