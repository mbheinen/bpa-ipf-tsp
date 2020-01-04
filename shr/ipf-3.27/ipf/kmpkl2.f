C    @(#)kmpkl2.f	20.3 2/13/96
        function kmpkl2(m,n)
C
      include 'ipfinc/dcsrt.inc'
C
        kmpkl2=kolum(m) - kolum(n)
        return
        end
