C    @(#)kmparc.f	20.3 2/13/96
      integer function kmparc (m,n)
c
c       This function compares ARCNAM(M) with ARCNAM(N)
C
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/arcntl.inc'
      include 'ipfinc/blank.inc'
      include 'ipfinc/qsdup.inc'
C
        kmparc = kompr (arcnam(m),arcnam(n), idummy)
        if ( kmparc .eq. 0 .and. m .ne. n ) dupsw = .true.
        return
        end
