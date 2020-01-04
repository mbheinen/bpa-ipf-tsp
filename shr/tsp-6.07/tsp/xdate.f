C    %W% %G%
       subroutine xdate(date)                                           
C                                                                       
C               THIS ROUTINE GETS THE CURRENT DATE FROM THE SYSTEM      
C               AS :                                                    
C                       IMON = MM                                       
C                       JDAY = DD                                       
C                       KYER = YY                                       
C                                                                       
C               AND REFORMATS IT AS THE CHARACTER STRING:               
C                                                                       
C                       'MM/DD/YY'                                      
C                                                                       
        character*(*) date
        integer i,j,k                                              
                                                                        
c       CALL IDATE(I,J,K)                                                
       call n_date(i,j,k)
C                                                                       
       write (date,11) i,j,k                                            
 11    format(i2,'/',i2,'/',i2)                                         
C                                                                       
        return                                                          
C                                                                       
       end                                                              
