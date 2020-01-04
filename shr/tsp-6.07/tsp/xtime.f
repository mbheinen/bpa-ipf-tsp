C    %W% %G%
      subroutine xtime(itrr)                                            
c                                                                       
C     This subroutine totals the CPU time used by various portions of 
C     the solution logic. It calls CPUTMR to get CPU time, and is called
C     by SWINGM, CNTRL, and DERIV.                                    
c
C       ITRR = 0 inititalizes arrays                                    
C            = 1 timing for input                                       
C            = 2 timing for inititalization                             
C            = 3 timing for cntrl (differential equations)              
C            = 4 timing for deriv (current vector formulation)          
C            = 5 timing for reduce and matmod (matrix manipulation)     
C            = 6 timing for dwnbak (voltage solution)                   
C            = 7 timing for dc multi terminal dc logic                  
C            = 8 timing for output                                      
C            = 9,10 spares                                              
c                                                                       
      common /xtimex/ time1(10),time2(10)                                 

      if (itrr .gt. 10) return                                            
      if (itrr .eq. 0) then                                               
        do 100 i=1,10                                                   
        time1(i) = 0.0                                                  
 100    time2(i) = 0.0                                                  
        return                                                          
      endif                                                             
c                                                                       
c     Initialize timer                                                
c                                                                       
      if (itrr .gt. 0)then                                               
c                                                                       
c       TE is elapsed CPU time                                          
c                                                                       
         te = cpu_secs (ts)
         time2(itrr) = te                                               
         return                                                         
      endif                                                             
c                                                                       
c     Stop TIMER and calculate total time                             
c                                                                       
      if (itrr .lt. 0) then                                               

         te = cpu_secs(0.0)
         jtrr = iabs(itrr)                                              
         timdif = te - time2(jtrr)                                      
         time1(jtrr) = time1(jtrr) + timdif                             
         return                                                         
       endif                                                            
       return                                                           
       end                                                              
