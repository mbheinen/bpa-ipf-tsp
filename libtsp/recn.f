C    %W% %G%
        subroutine recn(am,l,k,n)
C       -  CDC extended core simulation routine 
C       -  Sets AM(KK,I) <-- ECN(K+I,KK) 
C       -     for KK = 1 to L and I = 1 to N                            
C       -
      include 'tspinc/params.inc' 
      include 'tspinc/space0.inc' 
        dimension am(l,*)
        do 200 kk=1,l                                                   
           do 100 i=1,n                                                 
              am(kk,i)=ecn(k+i,kk)                                      
100        continue                                                     
200     continue                                                        
        return                                                          
        end
