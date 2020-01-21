C    %W% %G%
        subroutine eqvsec (y,yscr)                                      
        dimension y(2,2),yscr(3,3)                                      
        complex *8 y                               
        complex *16 yscr, z                                   
C                                                                       
C*****************************************************                  
C                                                                       
C                                                                       
C       THIS SUBROUTINE CONSOLIDATES SECTIONS. THE FIRST SECTION IS     
C       PROCESSED WITH                                                  
C                                                                       
C               CALL EQVFIR (Y,YSCR)                                    
C                                                                       
C       AND SUBSEQUENT SECTIONS WITH                                    
C                                                                       
C               CALL EQVNEX (Y,YSCR)                                    
C                                                                       
C       "Y" IS THE COMPLEX 2-PORT ADMITTANCE MATRIX OF THE              
C       SECTION BEING PROCESSED. IT IS NOT ALTERED BY EITHER OF         
C       THE AFORE MENTIONED CALLS.                                      
C                                                                       
C       THE PI EQUIVALENT OF CUMULATIVE SECTIONS CAN BE RETRIEVED WITH  
C                                                                       
C               CALL EQVFIN (Y,YSCR)                                    
C                                                                       
C       WHERE Y NOW CONTAINS  THE COMPLEX 2-PORT ADMITTANCE             
C       MATRIX.                                                         
C                                                                       
C*****************************************************                  
C                                                                       
C       PROCESS FIRST SECTION                                           
C                                                                       
        entry eqvfir (y,yscr)                                           
        do 100 i=1,2                                                    
        do 100 j=1,2                                                    
  100   yscr(j,i) = y(j,i)                                              
        do 110 i=1,3                                                    
        yscr(i,3) = (0.0,0.0)                                           
        yscr(3,i) = (0.0,0.0)                                           
  110   continue                                                        
        return                                                          
C                                                                       
C       PROCESS SUBSEQUENT SECTIONS                                     
C                                                                       
        entry eqvnex (y,yscr)                                           
        do 120 i=2,3                                                    
        do 120 j=2,3                                                    
  120   yscr(j,i) = yscr(j,i) + y(j-1,i-1)                              
C                                                                       
C       ELIMINATE "INTERIOR" NODE                                       
C                                                                       
        z=yscr(2,2)                                                     
        if (z .eq. 0.0) z = 1.0e-6                                      
        do 130 j=1,3,2                                                  
  130   yscr(2,j)=yscr(2,j)/z                                           
        do 140 i=1,3,2                                                  
        z=yscr(i,2)                                                     
        do 140 j=1,3,2                                                  
  140   yscr(i,j)=yscr(i,j)-z*yscr(2,j)                                 
C                                                                       
C       REORDER EQUIVALENT MATRIX                                       
C                                                                       
        yscr(1,2)=yscr(1,3)                                             
        yscr(2,2)=yscr(3,3)                                             
        yscr(2,1)=yscr(3,1)                                             
        do 150 i=1,3                                                    
        yscr(3,i)=(0.0,0.0)                                             
  150   yscr(i,3)=(0.0,0.0)                                             
        return                                                          
C                                                                       
C       RETRIEVE PI EQUIVALENT                                          
C                                                                       
        entry eqvfin (y,yscr)                                           
        do 160 i=1,2                                                    
        do 160 j=1,2                                                    
  160   y(j,i)=yscr(j,i)                                                
        return                                                          
        end                                                             
