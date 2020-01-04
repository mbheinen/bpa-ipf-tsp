C    %W% %G%
      subroutine closts(j,k)                                            
C * * *                                                                 
C * * * THIS SUBROUTINE CLOSES FILES USED BY THE PROGRAM                
C * * * FILES 1 AND 2 ARE SCRATCH FILES SO THEY ARE DELETED             
C * * * WHEN CLOSED                                                     
C * * *                                                                 
      if(k .eq. 0)then                                                  
         close(unit = j)                                                
      else                                                              
         close(unit=j,status = 'DELETE')                                
      endif                                                             
      return                                                            
      end                                                               
