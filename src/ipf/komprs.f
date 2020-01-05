C    @(#)komprs.f	20.3 2/13/96
        function komprs (m,n)
 
      include 'ipfinc/snput.inc'
 
        komprs = kompr ( zondat(m), zondat(n), komprs)
        return
        end
