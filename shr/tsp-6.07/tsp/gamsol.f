C    %W% %G%
      subroutine gamsol(i)                                              
C * * *                                                                 
C * * * THIS SUBROUTINE SOLVES THE DIFFERENTIAL EQUATIONS IN THE        
C * * * GAMMA MODULATION MODEL FOR THE TWO TERMINAL DC LINE             
C * * *                                                                 
      include 'tspinc/params.inc' 
      include 'tspinc/vrgov.inc' 
      include 'tspinc/lnk12.inc' 
      include 'tspinc/comvar.inc' 
      include 'tspinc/gamma.inc' 
      include 'tspinc/busvolt.inc' 
      ino = igamno(i)                                                   
      if(lppwr .eq. 0)return                                            
      if(lppwr .eq. 1)then                                              
C * * *                                                                 
C * * * AT THE START OF A TIME STEP, FIND PAST STATE VARIABLES          
C * * *                                                                 
       v1o =( vaco(i) +gamhb1(i) )/gamdt1(i)                            
       v1po = v1o - vacref(i)                                           
       v2o = v1po*gamdt3(i) - gamhb3(i)                                 
       v3o = (v2o + gamhb4(i))/gamdt4(i)                                
       v4o = v3o*gamdt5(i) - gamhb5(i)                                  
       v5o = (v4o + gamhb6(i))/gamdt6(i)                                
C * * *                                                                 
C * * * IF THE TIME STEP HAS CHANGED, CHANGE THE TIME FACTORS           
C * * *                                                                 
        if(al .ne. 0.0)then                                             
           gamdt1(i) = gamdt1(i)*tfac - tfac +1.                        
           gamdt3(i) = gamdt3(i)*tfac - tfac * agam(i) +agam(i)         
           gamdt4(i) = gamdt4(i)*tfac - tfac +1.                        
           gamdt5(i) = gamdt5(i)*tfac - tfac * bgam(i) +bgam(i)         
           gamdt6(i) = gamdt6(i)*tfac - tfac +1.                        
        endif                                                           
C * * *                                                                 
C * * * CALCULATE PAST VALUE FACTORS                                    
C * * *                                                                 
        if(idsw .eq. 3 .or. idsw .eq. 5) vaco(i) = vaco1(i)             
        gamhb1(i) = vaco(i) +v1o*(gamdt1(i)-2.)                         
        gamhb3(i) = v2o + (v1o-vacref(i))*(gamdt3(i)-2.*agam(i))        
        gamhb4(i) = v2o + v3o*(gamdt4(i)-2.)                            
        gamhb5(i) = v4o + v3o*(gamdt5(i)-2.*bgam(i))                    
        gamhb6(i) = v4o + v5o*(gamdt6(i) - 2.)                          
        gamden(i)=  gamdt5(i)*gamdt3(i)/(gamdt1(i)*gamdt4(i)*gamdt6(i)) 
        temp = gamdt5(i)/(gamdt4(i)*gamdt6(i))                          
        gamcon(i) = gamden(i)*(gamhb1(i)-vacref(i)*gamdt1(i))           
     1              +(gamhb4(i)-gamhb3(i))*temp+(gamhb6(i)-gamhb5(i))   
     2              /gamdt6(i)                                          
      endif                                                             
C * * *                                                                 
C * * * CALCULATE NEW STATE VARIABLES                                   
C * * *                                                                 
      vaco(i) = vac(i)                                                  
      v5 = vac(i)*gamden(i) + gamcon(i)                                 
      delgam = cgama(i)*v5                                              
      gama(i) = delgam + gamref(i)                                      
      if(gama(i) .gt. gamax(i)) gama(i) = gamax(i)                      
      if(gama(i) .lt. gamin(i)) gama(i) = gamin(i)                      
      return                                                            
      end                                                               
