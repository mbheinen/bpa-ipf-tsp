C    @(#)swpsrt.f	20.3 2/13/96
        subroutine swpsrt (m,n)                                          
c                                                                       
        include 'ipfinc/parametr.inc'
        include 'ipfinc/komps.inc'
c                                                                       
        do 100 i=1,3                                                    
           itemp=ksort(i,m)                                                
           ksort(i,m)=ksort(i,n)                                           
           ksort(i,n)=itemp                                                
  100   continue
        return                                                          
        end                                                             
