C    @(#)komcbs.f	20.3 2/13/96
        function komcbs(m,n)
C
C       COMPARE CUSTOMER BUSES M & N
C
 
 
      include 'ipfinc/parametr.inc'

      include 'ipfinc/cbsorc.inc'
      include 'ipfinc/cbus.inc'
      include 'ipfinc/qsdup.inc'
 
 
        if (kompr(cbkey(m),cbkey(n),komcbs).eq.0) then
           if (m.ne.n)dupsw=.true.
        endif
C
        return
        end
