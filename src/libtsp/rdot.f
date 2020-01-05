C    %W% %G%
        subroutine rdot(i)                                              
C * * *                                                                 
C * * * THIS SUBROUTINE CALCULATES THE APPARENT R AND RATE OF CHANGE OF 
C * * * APPARENT R (R DOT) OF A LINE.  THE LOGIC FOR REPRESENTING THE   
C * * * FILTERS AND TRANSDUCERS USED IN THE R RDOT RELAY IS             
C * * * INCLUDE IN THIS SUBROUTINE.  THE R AND R-DOT CALCULATED HERE    
C * * * IS THE SAME AS THE INPUTS TO THE RELAY MODEL.                   
C * * *                                                                 
      include 'tspinc/rdcal.inc' 
C * * *                                                                 
C * * * IF LINE IS OPEN, MAKE NO CALCULATION                            
C * * *                                                                 
      if(izer .eq. 0) go to 7000                                        
      if(i .eq. 1)then                                                  
C * * *                                                                 
C * * * FOR FIRST TIME STEP, INITALIZE PRESENT AND PAST STATE VARIABLES 
C * * *                                                                 
C * * * INITALIZE PMWO,QMVO,VKVO,PMW1,QMV1,VKV1,PMW1O,QMV10,VKV1O       
C * * *                                                                 
        do 1000 ird = 1,3                                               
        rdcalo(ird) = rdcal(ird)                                        
        rdcalo(ird+3) = rdcal(ird)                                      
 1000   rdcal(ird +3) =rdcal(ird)                                       
        r = abs((pmw*vkv*vkv)/(pmw*pmw +qmv*qmv))                       
        ro = r                                                          
        r1 = r                                                          
        r1o = r                                                         
C * * *                                                                 
C * * * INITALIZE RD,RDO,RD1,RD1O,RD2,RD2O                              
C * * *                                                                 
        do 2000 ird = 9,11                                              
        rdcal(ird) = 0.0                                                
 2000   rdcalo(ird) = 0.0                                               
        go to 7000                                                      
      endif                                                             
      if(deltim .lt. .001) then                                         
C * * *                                                                 
C * * * PRECESS STATE VARIABLES AT A DISCONTINUITY ( DELTA T = 0.0)     
C * * *                                                                 
C * * * INPUT VARIABLES PMW,QMV,VKV PRECESSED                           
C * * *                                                                 
        do 3000 ird = 1,3                                               
 3000   rdcalo(ird) = rdcal(ird)                                        
C * * *                                                                 
C * * * INTERNAL STATE VARIABLES DO NOT CHANGE (R,R1,RD,RD1,RD2)        
C * * *                                                                 
        do 4000 ird = 4,11                                              
 4000   rdcal(ird) = rdcalo(ird)                                        
        go to 7000                                                      
C * * *                                                                 
C * * * CALCULATE NEW STATE VECTORS IF TIME HAS ADVANCED (T(J) .GT. T(J-
C * * *                                                                 
      else                                                              
C * * *                                                                 
C * * * CALCULATE TIME FACTORS (2* TIME CONSTANT/DELTA T) + 1.          
C * * *                                                                 
        dtb = (2./deltim)                                               
        dtd = ctd*dtb + 1.                                              
        dt1 = ct1*dtb + 1.                                              
        dt2 = ct2*dtb + 1.                                              
                                                                        
C * * *                                                                 
C * * *  CALCULATE PMW1,QMV1,VKV1                                       
C * * *                                                                 
        do 5000 ird = 1,3                                               
 5000   rdcal(ird+3) = (rdcal(ird) +rdcalo(ird) +rdcalo(ird+3)*(dtd-2.))
     1  /dtd                                                            
        if((pmw*pmw + qmv*qmv) .ne. 0.0)then                            
           r = abs((pmw1*vkv1*vkv1)/(pmw*pmw +qmv*qmv))                 
        else                                                            
           r = 0.0                                                      
        endif                                                           
        r1 = (r + ro + r1o*(dt1-2.))/dt1                                
        rd =  dtb*(r1-r1o) - rdo                                        
C * * *                                                                 
C * * * CALCULATE RD1 AND RD2                                           
C * * *                                                                 
        do 6000 ird = 10,11                                             
 6000   rdcal(ird) =(rdcal(ird-1) + rdcalo(ird-1)+rdcalo(ird)*(dt2-2.)) 
     1  /dt2                                                            
      endif                                                             
C * * *                                                                 
C * * * CONVERT R AND RDOT TO OHMS AND OHMS/SEC                         
 7000 rnow = r1*zbase                                                   
      rdnow = rd2*zbase*60.                                             
C * * *                                                                 
C * * * PRECESS ALL STATE VECTORS TO BE READY FOR NEXT TIME STEP        
C * * *                                                                 
      do 8000 ird = 1,11                                                
 8000 rdcalo(ird) = rdcal(ird)                                          
      return                                                            
      end                                                               
