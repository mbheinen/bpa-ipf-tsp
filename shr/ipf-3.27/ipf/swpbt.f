C    @(#)swpbt.f	20.3 2/13/96
        subroutine swpbt(m,n)
 
C       Swap used in subroutine
C       PRESLN to prepare a table of TRACE_BUS's
C       for subroutine STRACE.
 
      include 'ipfinc/parametr.inc'
      include 'ipfinc/optim1.inc'
      include 'ipfinc/strce.inc'
C
        itemp = ibus(1,m)
        ibus(1,m) = ibus(1,n)
        ibus(1,n) = itemp
        return
        end
