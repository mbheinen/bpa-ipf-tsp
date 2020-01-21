C    %W% %G%
        subroutine jpinfo(cpu,faults,wssize,user,accnt  )               
C                                                                       
C ***   THIS SUBROUTINE RETURNS THE FOLLOWING ARGUMENTS :               
                                                                        
C          CPU      CURRENT ACCUMULATED CPU TIME  (REAL*4, SEC)         
C          FAULTS   CURRENT PAGE FAULT COUNT      (INTEGER*4, FAULTS)   
C          WSSIZE   CURRENT WORKING SET SIZE      (INTEGER*4, PAGES)    
C          USER     THE CALLER'S USER NAME        (CHARACTER*(*))       
C          ACCNT    ACCOUNT NAME                  (CHARACTER*8, ASCII)  
C                                                                       
C     include 'tspinc/jpilst.inc' 
C                                                                       
C               PERORM THE SYSTEM SERVICE CALL                          
C                                                                       
C       call sys$getjpi(,,,lenacc,,,)                                   
                                                                        
C               Transfer the returned values to the parameters          
                                                                        
        cpu     =       cpu_secs (0.0)                                          
        cpu     =       cpu/100.0                                       
        faults  =       0                                          
        wssize  =       0                                          
        user    =       0                                         
        accnt   =       0                                         
                                                                        
        return                                                          
                                                                        
        end                                                             
