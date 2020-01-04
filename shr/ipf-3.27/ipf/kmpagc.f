C    @(#)kmpagc.f	20.3 2/13/96
        function kmpagc (m,n)
C
      include 'ipfinc/parametr.inc'

      include 'ipfinc/agc.inc'
      include 'ipfinc/qsdup.inc'
C
        kmpagc = kagc(1,m) - kagc(1,n)
        if (kmpagc .eq. 0 .and. m .ne. n) dupsw = .true.
        return
        end
