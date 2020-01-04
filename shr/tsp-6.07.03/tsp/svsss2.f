C    %W% %G%
      subroutine svsss2                                                 
C * * *                                                                 
C * * * THIS SUBROUTINE DECODES AND ERROR CHECKS TYPE X SUPPLEMENTARY   
C * * * SIGNAL DATA FOR THE SVS MODEL.  IT IS CALLED BY INPUT3.         
C * * *                                                                 
      include 'tspinc/param.inc' 
      include 'tspinc/svs.inc'
      include 'tspinc/inp3.inc' 
      include 'tspinc/inp3a.inc' 
      include 'tspinc/tzro.inc' 
      include 'tspinc/prt.inc' 
      dimension temp(16)                                                
C     -     begin     begin     begin     begin     begin     begin 
      read (work80(icard),8420) (temp(i),i=1,4)                          
8420  format (bz,16x,f4.0,3f4.4)                                            

      if (isupt(ksv) .le. 0)then                                         
         write(errbuf(1),8422)                                          
8422     format(1h0,5x,'SUPPLEMENTARY DATA SUBTYPE D,E,OR F MUST INCLUDE
     1           subtype a,b, or c data')                               
          call prterr('E',1)                                            
          imchn = 1                                                     
       endif                                                            

       if (temp(1) .eq. 0.0) then                                        
         write(errbuf(1),8430) nbname,bkv,nid                           
8430     format(1h0,2x,a8,1x,f5.1,2x,a1,5x,                             
     1         'KS2 CANNOT BE ZERO')                                    
         call prterr('E',1)                                             
         imchn = 1                                                      
       endif                                                            

      if (temp(4) .le. 0.0)then                                          
         write(errbuf(1),8450) nbname,bkv,nid                           
8450     format(1h0,2x,a8,1x,f5.1,2x,a1,5x,                             
     1          'TS12 MUST BE GREATER THAN ZERO')                       
         call prterr('E',1)                                             
         imchn = 1                                                      
      endif                                                             

      if (imchn .eq. 1) return                                           

      if (subtyp .eq.'D') then                                           
         isupb(ksv) = 1                                                 
         go to 8490                                                     
      endif                                                             

8470  if (subtyp .eq. 'E') then                                          
         isupb(ksv) = 2                                                 
         go to 8490                                                     
      endif                                                             

      isupb(ksv) = 3                                                    

8490  cks2(ksv) = temp(1)                                               
      xcon = 2.0*frqbse/dtsvs
      as10(ksv) = xcon*temp(2) + 1.0                                    
      as11(ksv) = xcon*temp(3) + 1.0                                    
      as12(ksv) = xcon*temp(4) + 1.0                                    
      ispcde(ksv) = 1                             !csw added 5/94

      return                                                            
      end                                                               
