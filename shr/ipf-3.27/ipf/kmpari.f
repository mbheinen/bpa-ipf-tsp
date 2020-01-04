C    @(#)kmpari.f	20.3 2/13/96
      function kmpari (m,n)
c
c       compare arcint(1,*) and arcint (2,*)
c
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/arcntl.inc'
      include 'ipfinc/qsdup.inc'
c
      kmpari = kompr (arcint(1,m),arcint(1,n),junk)
      if (kmpari .eq. 0) then
         kmpari = kompr (arcint(2,m),arcint(2,n),junk)
      endif
      if (kmpari .eq. 0 .and. m .ne. n) dupsw=.true.
      return
      end
