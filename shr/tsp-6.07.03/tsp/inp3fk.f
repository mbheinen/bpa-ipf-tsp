C    %W% %G%
      subroutine inp3fk                                                 
                                                                        
C                                                                       
C THIS SUBROUTINE DECODES DATA CARDS FOR THE FK EXCITER AND             
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
      if(newex .eq. 1) go to 1000                                       
C                                                                       
C  DECODE FK CARD                                                       
C                                                                       
          read ( work80( icard), 100) (temp(i), i = 1, 13)              
 100      format (bz,16x, 3f5.4, 4f5.3, f5.2, 4f5.3, f4.3)                  
C                                                                       
C ERROR CHECKING DATA                                                   
C                                                                       
       if(temp(3) .lt. tzero)then                                       
          temp(3) = tzero                                               
          write(errbuf(1),105) nbname,bkv,nid,temp(3)                   
 105      format(1h0,2x,a8,2x,f5.1,2x,a1,5x,'TR IS BEING SET',          
     1           ' EQUAL TO: ',f7.5)                                    
          call prterr('W',1)                                            
       endif                                                            
       if(temp(11) .ge. temp(10))then                                   
       write(errbuf(1),120)nbname,bkv,nid                               
 120   format(1h0,2x,a8,2x,f5.1,2x,a1,5x,'VRMAX MUST BE GREATER THAN',  
     1 ' VRMIN')                                                        
      call prterr('E',1)                                                
      imchn = 3                                                         
          endif                                                         
       if(temp(8) .le.0.0) then                                         
       write(errbuf(1),130)nbname,bkv,nid                               
 130   format(1h0,2x,a8,2x,f5.1,2x,a1,5x,'KA MUST BE GREATER THAN 0.0') 
       call prterr('E',1)                                               
       imchn = 3                                                        
          endif                                                         
       if(temp(9) .le.0.0) then                                         
       write(errbuf(1),140)nbname,bkv,nid                               
 140   format(1h0,2x,a8,2x,f5.1,2x,a1,5x,'TA MUST BE GREATER THAN 0.0') 
       call prterr('E',1)                                               
       imchn = 3                                                        
          endif                                                         
       if(temp(7) .gt. 0.0 .and. temp(6) .le. 0.0)then                  
       write(errbuf(1),160)nbname,bkv,nid                               
 160   format(1h0,2x,a8,2x,f5.1,2x,a1,5x,'TB MUST BE GREATER THAN 0.0') 
      call prterr('E',1)                                                
      imchn = 3                                                         
          endif                                                         
       if(temp(5) .ge. temp(4))then                                     
       write(errbuf(1),170)nbname,bkv,nid                               
 170   format(1h0,2x,a8,2x,f5.1,2x,a1,5x,'VIMAX MUST BE GREATER THAN',  
     1 ' VIMIN')                                                        
      call prterr('E',1)                                                
      imchn = 3                                                         
          endif                                                         
       if(temp(4) .le.0.0) then                                         
       write(errbuf(1),180)nbname,bkv,nid                               
 180   format(1h0,2x,a8,2x,f5.1,2x,a1,5x,'VIMAX MUST BE GREATER THAN',  
     1 ' 0.0')                                                          
       call prterr('E',1)                                               
       imchn = 3                                                        
          endif                                                         
       if(imchn .gt. 0)return                                           
C                                                                       
C STORE FK EXCITER DATA INTO CITER TABLE                                
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
C  TA                                                                   
          citer(25,isgg) = temp( 9)*frqbse                              
C  VRMAX                                                                
          citer( 1,isgg) = temp(10)                                     
C  VRMIN                                                                
          citer( 2,isgg) = temp(11)                                     
C  VIMAX                                                                
          citer(11,isgg) = temp(4)                                      
C  VIMIN                                                                
          citer( 12,isgg) = temp(5)                                     
          return                                                        
C                                                                       
C DECODE DATA FOR FK EXCITER FROM FZ CARD                               
C SET NEWEX TO INDICATE FZ HAS BEEN READ                                
C                                                                       
 1000     newex = 1                                                     
          read ( work80( icard), 1100) (temp(i),i = 1, 12)              
 1100     format (bz,16x, 6f5.3, 2f5.4, 4f5.3)                              
C                                                                       
C ERROR CHECK DATA FROM FZ CARD                                         
C                                                                       
       if(temp(5) .gt. 0.0 .and. temp(6).le.0.0) then                   
       write(errbuf(1),1120)nbname,bkv,nid                              
1120   format(1h0,2x,a8,2x,f5.1,2x,a1,5x,'TF MUST BE GREATER THAN 0.0') 
       call prterr('E',1)                                               
       imchn = 3                                                        
          endif                                                         
       if(imchn .gt. 0)return                                           
C                                                                       
C STORE FK EXCITER DATA FROM FZ CARD                                    
C                                                                       
C  KF                                                                   
          citer(24,isgg) = temp(5 )*frqbse                              
C  TF                                                                   
          citer(19,isgg) = temp(6 )*frqbse                              
C KC                                                                    
          citer(10,isgg) = temp(7 )                                     
C                                                                       
C CONVERT MACHINE BASE IMPEDANCES TO SYSTEM BASE                        
C                                                                       
       if(bmvac .ne. 0.0) then                                          
C RC                                                                    
       citer(21,isgg) = citer(21,isgg)*bmvac                            
C XC                                                                    
       citer(23,isgg) = citer(23,isgg)*bmvac                            
       endif                                                            
          return                                                        
          end                                                           
