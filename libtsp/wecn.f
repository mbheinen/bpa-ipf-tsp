C    %W% %G%
        subroutine wecn(am,l,k,n)                                       
C       -  CDC extended core simulation routine 
C       -  Sets ECN(K+I,KK) <-- AM(KK,I) 
C       -     for KK = 1 to L and I = 1 to N                            
C       -
      include 'tspinc/params.inc' 
      include 'tspinc/space0.inc' 
        dimension am(l,*)
        if(n .eq. 0) return                                             
        do 20 kk=1,l                                                    
           do 10 i=1,n                                                  
               ecn(k+i,kk)=am(kk,i)                                     
10         continue                                                     
20      continue                                                        
        return                                                          
        end
