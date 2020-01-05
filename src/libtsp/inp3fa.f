C    %W% %G%
      subroutine inp3fa                                                 
                                                                        
C                                                                       
C THIS SUBROUTINE DECODES DATA CARDS FOR THE FA EXCITER AND             
C FORMS THE INITAL DATA TABLES                                          
C                                                                       
      include 'tspinc/param.inc' 
      include 'tspinc/tzro.inc' 
      include 'tspinc/params.inc' 
      include 'tspinc/prt.inc' 
      include 'tspinc/comn34.inc' 
      include 'tspinc/citer.inc' 
      include 'tspinc/inp3.inc' 
      dimension temp(13)                                                
C     -     begin     begin     begin     begin     begin     begin 
      if (newex .eq. 1) go to 1000                                       
C                                                                       
C  DECODE FA CARD                                                       
C                                                                       
          read ( work80( icard), 100) (temp(i), i = 1, 13)              
 100      format (bz,16x, 3f5.4, 4f5.3, f5.2, 4f5.3, f4.3)                  
C                                                                       
C ERROR CHECKING DATA                                                   
C                                                                       
       if (temp(3) .lt. tzero)then                                       
          temp(3) = tzero                                               
          write (errbuf(1),105) nbname,bkv,nid,temp(3)                   
 105      format (1h0,2x,a8,2x,f5.1,2x,a1,5x,'TR IS BEING SET',          
     1           ' EQUAL TO: ',f7.5)                                    
          call prterr('W',1)                                            
       endif                                                            
       if (temp(8) .le. 0.0) then                                       
          write (errbuf(1),110) nbname,bkv,nid                          
 110      format(1h0,2x,a8,2x,f5.1,2x,a1,5x,'KA MUST BE GREATER THAN '  
     1       ,'0.0')                                                    
          call prterr('E',1)                                            
          imchn = 3                                                     
       endif                                                            
       if (temp(9) .le. 0.0) then                                       
          write (errbuf(1),120) nbname,bkv,nid                          
 120      format(1h0,2x,a8,2x,f5.1,2x,a1,5x,'TA MUST BE GREATER THAN '  
     1       ,'0.0')                                                    
          call prterr('E',1)                                            
          imchn = 3                                                     
       endif                                                            
       if (temp(10) .le. 0.0 .or. temp(10) .le. temp(11)) then          
          write (errbuf(1),130) nbname,bkv,nid                          
 130      format(1h0,2x,a8,2x,f5.1,2x,a1,5x,'IMPROPER VOLTAGE LIMITS '  
     1    ,' SPECIFIED ')                                               
          call prterr('E',1)                                            
          imchn = 3                                                     
       endif                                                            
       if (temp(13) .le. 0.0) then                                      
          write (errbuf(1),160) nbname,bkv,nid                          
 160      format(1h0,2x,a8,2x,f5.1,2x,a1,5x,'TE MUST BE GREATER THAN '  
     1        ,'0.0')                                                   
          call prterr('E',1)                                            
          imchn = 3                                                     
       endif                                                            
       if (temp(7) .gt. 0.0 .and. temp(6) .le. 0.0) then                
          write (errbuf(1),170) nbname,bkv,nid                          
 170      format(1h0,2x,a8,2x,f5.1,2x,a1,5x,'TB MUST BE GREATER THAN '  
     1      ,'0.0 SINCE TC HAS BEEN SPECIFIED')                         
          call prterr('E',1)                                            
          imchn = 3                                                     
       endif                                                            
       if(imchn .gt. 0) return                                          
C                                                                       
C STORE FA EXCITER DATA INTO CITER TABLE                                
C                                                                       
C  RC                                                                   
          citer(21,isgg) = temp( 1)                                     
C  XC                                                                   
          citer(23,isgg) = temp( 2)                                     
C  TR                                                                   
          citer( 9,isgg) = temp( 3)*frqbse                              
C  TB                                                                   
          citer(17,isgg) = temp( 6)*frqbse                              
C  TC                                                                   
          citer(18,isgg) = temp( 7)*frqbse                              
C  KA                                                                   
          citer( 8,isgg) = temp( 8)                                     
C CKAP                                                                  
          citer(16,isgg) = temp(8)                                      
C  TA                                                                   
          citer(25,isgg) = temp( 9)*frqbse                              
C  VRMAX                                                                
          vrmax = temp(10)                                              
          citer( 1,isgg) = temp(10)                                     
C  VRMIN                                                                
          vrmin = temp(11)                                              
          citer( 2,isgg) = temp(11)                                     
C  KE                                                                   
          cke = temp(12)                                                
          citer( 7,isgg) = temp(12)                                     
C  TE                                                                   
          citer(20,isgg) = temp(13)*frqbse                              
          return                                                        
C                                                                       
C DECODE DATA FOR FA EXCITER FROM FZ CARD                               
C SET NEWEX TO INDICATE FZ HAS BEEN READ                                
C                                                                       
 1000     newex = 1                                                     
          read ( work80( icard), 1100) (temp(i),i = 1, 12)              
 1100     format (bz,16x, 6f5.3, 2f5.4, 4f5.3)                              
C                                                                       
C ERROR CHECK DATA FROM FZ CARD                                         
C                                                                       
       if (temp(1) .le.0.0) then                                        
          write (errbuf(1),1120) nbname,bkv,nid                         
1120      format(1h0,2x,a8,2x,f5.1,2x,a1,5x,'SE1 MUST BE GREATER THAN ' 
     1       ,'0.0')                                                    
          call prterr('E',1)                                            
          imchn = 3                                                     
       endif                                                            
       if (temp(2) .le.0.0) then                                        
          write (errbuf(1),1130) nbname,bkv,nid                         
1130      format(1h0,2x,a8,2x,f5.1,2x,a1,5x,'SE2 MUST BE GREATER THAN ' 
     1       ,'0.0')                                                    
          call prterr('E',1)                                            
          imchn = 3                                                     
       endif                                                            
       if (temp(1) .le. temp(2)) then                                   
          write (errbuf(1),4100) nbname,bkv,nid                         
          call prterr ('E',1)                                           
 4100     format (1h ,2x,a8,2x,f5.1,2x,a1,5x,'SE2 SHOULD BE GREATER'    
     1       ,' THAN 0.0 BUT LESS THAN SE1 ')                           
          imchn=3                                                       
       else if (temp(4) .le. 0.0) then                                  
          write (errbuf(1),4120) nbname,bkv,nid                         
          call prterr ('E',1)                                           
 4120     format (1h ,2x,a8,2x,f5.1,2x,a1,5x,'EFD1 SHOULD BE GREATER'   
     1       ,' THAN 0.0 ')                                             
          imchn=3                                                       
       else if (temp(1) .gt. 0.0 .and. temp(2) .gt. 0.0) then           
C                                                                       
C         CALCULATE SATURATION CONSTANTS FROM MAXMIMUM ABSOLUTE FIELD   
C         VOLTAGE                                                       
C                                                                       
          efd1 = temp(4)                                                
          sd100 = temp(1)                                               
          sd75 = temp(2)                                                
          esat = (alog(sd100/sd75))/(.25*efd1)                          
          csat = sd100/(exp(esat*efd1))                                 
       endif                                                            
       if (temp(5) .le.0.0) then                                        
          write (errbuf(1),1140) nbname,bkv,nid                         
1140      format(1h0,2x,a8,2x,f5.1,2x,a1,5x,'KF MUST BE GREATER THAN '  
     1       ,'0.0')                                                    
          call prterr('E',1)                                            
          imchn = 3                                                     
       endif                                                            
       if (temp(6) .le. 0.0) then                                       
          write (errbuf(1),1150) nbname,bkv,nid                         
1150      format(1h0,2x,a8,2x,f5.1,2x,a1,5x,'TF MUST BE GREATER THAN '  
     1       ,'0.0')                                                    
          call prterr('E',1)                                            
          imchn = 3                                                     
          endif                                                         
       if(imchn .gt. 0) return                                          
C                                                                       
C     VOLTAGE REGULATOR LIMITS COULD BE A FUNCTION OF THE OPERATING     
C     POINT WHICH WILL BE COMPUTED LATER.                               
C     TEMPORARILY FIELD VOLTAGE LIMITS ARE STORED.                      
C                                                                       
                                                                        
C  ESAT                                                                 
          citer(4,isgg) = esat                                          
C  CSAT                                                                 
          citer(5,isgg) = csat                                          
                                                                        
C  EFDMAX,EFDMIN                                                        
          citer(31,isgg) = -efd1                                        
          citer(32,isgg) = efd1                                         
C  KF                                                                   
          citer(11,isgg) = temp(5 )*frqbse                              
C  TF                                                                   
          citer(12,isgg) = temp(6 )*frqbse                              
C                                                                       
C CONVERT MACHINE BASE IMPEDANCES TO SYSTEM BASE                        
C                                                                       
          if (bmvac .ne. 0.0) then                                      
C RC                                                                    
             citer(21,isgg) = citer(21,isgg)*bmvac                      
C XC                                                                    
             citer(23,isgg) = citer(23,isgg)*bmvac                      
       endif                                                            
C                                                                       
C STORE FA EXCITER DATA FROM FZ CARD                                    
          return                                                        
          end                                                           
