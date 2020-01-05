C    @(#)kmpbt.f	20.3 2/13/96
        function kmpbt(m,n)
 
C  This comparison is used in subroutine PRESLN to prepare a table of
C     TRACE_BUS's for subroutine STRACE.
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/optim1.inc'
      include 'ipfinc/qksrt.inc'
      include 'ipfinc/strce.inc'
 
        kmpbt = ibus(1,m) - ibus(1,n)
        if( ( kmpbt .eq. 0 ) .and. ( m .ne. n ) ) key = key +1
 
        return
        end
