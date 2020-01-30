C    @(#)swplt.f	20.3 2/13/96
        subroutine swplt(m,n)
 
C  Subroutine to establish a swap for subroutines QIKSRT
C   and PRESLN.  This swap is used in subroutine PRESLN
C   to prepare a table of TRACE_LTC's for subroutine STRACE.
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/optim1.inc'
      include 'ipfinc/strce.inc'
 
        itemp = itx(1,m)
        itx(1,m) = itx(1,n)
        itx(1,n) = itemp
 
        return
 
        end
