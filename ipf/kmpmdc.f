C    @(#)kmpmdc.f	20.3 2/13/96
        function kmpmdc(m,n)
C
      include 'ipfinc/dcsrt.inc'
C
        kmpmdc = mdc(m) - mdc(n)
        return
        end
