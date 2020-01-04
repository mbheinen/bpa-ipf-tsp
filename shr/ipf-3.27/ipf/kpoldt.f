C    @(#)kpoldt.f	20.3 2/13/96
        function kpoldt (m,n)
C
C       Compare OLDTBX entities M & N
C
      include 'ipfinc/parametr.inc'

      include 'ipfinc/oldtbx.inc'
      include 'ipfinc/qsdup.inc'
C
        kpoldt = oldtbx(2,m) - oldtbx(2,n)
        if ((kpoldt .eq. 0) .and. (m .ne. n)) dupsw = .true.
        return
        end
