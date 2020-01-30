C    @(#)kmplt.f	20.3 2/13/96
        function kmplt(m,n)
 
C  This comparison is used in subroutine PRESLN to prepare TRACE_LTC's
C     for subroutine STRACE.
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/optim1.inc'
      include 'ipfinc/qksrt.inc'
      include 'ipfinc/strce.inc'
 
 
        kmplt = itx(1,m) - itx(1,n)
        if( ( kmplt .eq. 0) .and. ( m .ne. n ) ) key = key + 1
 
        return
        end
