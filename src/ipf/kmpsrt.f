C    @(#)kmpsrt.f	20.3 2/13/96
        function kmpsrt (m,n)                                            
c                                                                       
        include 'ipfinc/parametr.inc'

        include 'ipfinc/komps.inc'
c                                                                       
        kmpsrt = ksort(1,m) - ksort(1,n)                                    
        if (kmpsrt .eq. 0) kmpsrt = ksort(2,m) - ksort(2,n)                   
        if (kmpsrt .eq. 0) kmpsrt = ksort(3,m) - ksort(3,n)                   
        return                                                          
        end                                                             
