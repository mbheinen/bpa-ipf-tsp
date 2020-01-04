C    @(#)komxdt.f	20.3 2/13/96
      function komxdt (m,n)
C
C     COMPARE XDATA RCDS M & N
C
      include 'ipfinc/parametr.inc'

      include 'ipfinc/qsdup.inc'
      include 'ipfinc/xdata.inc'
C
      komxdt = xdata(1,m) - xdata(1,n)
      if (komxdt .eq. 0 .and. m .ne. n) dupsw = .true.
      return
      end
