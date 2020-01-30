C    %W% %G%
      subroutine inp3fl                                                 
C                                                                       
C THIS SUBROUTINE DECODES DATA CARDS FOR THE FL EXCITER AND             
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
      character*16 extmsg
      data extmsg /'Type FL exciter:'/
C     -     Begin     Begin     Begin     Begin     Begin     Begin
      if(newex .eq. 1) go to 1000                                       
C                                                                       
C  DECODE FL CARD                                                       
C                                                                       
          read ( work80( icard), 100) (temp(i), i = 1, 13)              
 100      format (bz,16x, 3f5.4, 4f5.3, f5.2, 4f5.3, f4.3)                  
C                                                                       
C ERROR CHECKING DATA                                                   
C                                                                       
       if(temp(3) .lt. tzero)then                                       
          temp(3) = tzero                                               
          write(errbuf(1),105) nbname,bkv,nid,extmsg,temp(3)            !DEM
 105        format(2x,a8,2x,f5.1,2x,a1,2x,a16,1x,                   !DEM
     1      'T_r will be set equal to: ',f7.5)                          !DEM
          call prterr('W',1)                                            
       endif                                                            
       if(temp(5) .ge. temp(4))then                                     
       write(errbuf(1),110)nbname,bkv,nid,extmsg                        !DEM
 110     format(2x,a8,2x,f5.1,2x,a1,2x,a16,1x,                      !DEM
     1   'V_i_max must be greater than V_i_min.')                       !DEM
      call prterr('E',1)                                                
      imchn = 3                                                         
          endif                                                         
       if(temp(11) .ge. temp(10))then                                   
       write (errbuf(1),120) nbname,bkv,nid,extmsg                      !DEM
 120     format (2x,a8,2x,f5.1,2x,a1,2x,a16,1x,                     !DEM
     1   'V_r_max must be greater than V_r_min')                        !DEM
      call prterr('E',1)                                                
      imchn = 3                                                         
          endif                                                         
       if(temp(8) .le. 0.0) then                                         
         write (errbuf(1),130) nbname,bkv,nid,extmsg                    !DEM
 130       format(2x,a8,2x,f5.1,2x,a1,2x,a16,1x,                    !DEM
     1     'K_a must be greater than 0.0')                              !DEM
       call prterr('E',1)                                               
       imchn = 3                                                        
          endif                                                         
       if(temp(9) .le. 0.0) then                                         
         write (errbuf(1),140) nbname,bkv,nid,extmsg                    !DEM
 140       format (2x,a8,2x,f5.1,2x,a1,2x,a16,1x,                   !DEM
     1     'T_a must be greater than 0.0')                              !DEM
       call prterr('E',1)                                               
       imchn = 3                                                        
          endif                                                         
       if(temp(12) .le. 0.0) then                                        
         write (errbuf(1),150) nbname,bkv,nid,extmsg                    !DEM
 150       format (2x,a8,2x,f5.1,2x,a1,2x,a16,1x,                   !DEM
     1     'K_j must be greater than 0.0')                              !DEM
       call prterr('E',1)                                               
       imchn = 3                                                        
          endif                                                         
       if(temp(7) .gt. 0.0 .and. temp(6) .le. 0.0)then                  
         write (errbuf(1),160) nbname,bkv,nid,extmsg                    !DEM
 160       format (2x,a8,2x,f5.1,2x,a1,2x,a16,1x,                   !DEM
     1     'T_b must be greater than 0.0')                              !DEM
      call prterr('E',1)                                                
      imchn = 3                                                         
          endif                                                         
      if(imchn .gt. 0)return                                            
C                                                                       
C STORE FL EXCITER DATA INTO CITER TABLE                                
C                                                                       
C  RC                                                                   
          citer(21,isgg) = temp( 1)                                     
C  XC                                                                   
          citer(23,isgg) = temp( 2)                                     
C  TR                                                                   
          citer( 9,isgg) = temp( 3)*frqbse                              
C  VIMAX                                                                
          citer(11,isgg) = temp(4 )                                     
C  VIMIN                                                                
          citer(12,isgg) = temp(5 )                                     
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
C  KJ                                                                   
          citer(16,isgg) = temp(12)                                     
          return                                                        
C                                                                       
C DECODE DATA FOR FL EXCITER FROM FZ CARD                               
C SET NEWEX TO INDICATE FZ HAS BEEN READ                                
C                                                                       
 1000     newex = 1                                                     
          read ( work80( icard), 1100) (temp(i),i = 1, 12)              
 1100     format (bz,16x, 6f5.3, 2f5.4, 4f5.3)                              
C                                                                       
C ERROR CHECK DATA FROM FZ CARD                                         
C                                                                       
       if(temp(1) .le. 0.0) then                                         
         write (errbuf(1),1120) nbname,bkv,nid,extmsg                   !DEM
1120       format (2x,a8,2x,f5.1,2x,a1,2x,a16,1x,                       !DEM
     1     'K_i must be greater than 0.0')                              !DEM
         call prterr('E',1)                                               
         imchn = 3                                                        
       endif                                                         
       if(temp(2) .le. 0.0) then                                         
         write (errbuf(1),1130) nbname,bkv,nid,extmsg                   !DEM
1130     format(2x,a8,2x,f5.1,2x,a1,2x,a16,1x,                          !DEM
     1     'K_p must be greater than 0.0')                              !DEM
         call prterr('E',1)                                               
         imchn = 3                                                        
       endif                                                         
       if(temp(4) .le. 0.0) then                                         
         write (errbuf(1),1140) nbname,bkv,nid,extmsg                   !DEM
 1140    format (2x,a8,2x,f5.1,2x,a1,2x,a16,1x,                         !DEM
     1     'E_fd_max must be greater than 0.0')                         !DEM
         call prterr('E',1)                                               
         imchn = 3                                                        
       endif                                                         
       if(temp(5) .le. 0.0) then                                         
         write (errbuf(1),1150) nbname,bkv,nid,extmsg                   !DEM
 1150    format (2x,a8,2x,f5.1,2x,a1,2x,a16,1x,                         !DEM
     1     'K_g must be greater than 0.0')                              !DEM
         call prterr('E',1)                                               
         imchn = 3                                                        
       endif                                                         
       if(temp(8) .lt. 0.0) then                                         
         write (errbuf(1),1160) nbname,bkv,nid,extmsg                   !DEM
 1160    format(2x,a8,2x,f5.1,2x,a1,2x,a16,1x,                          !DEM
     1     'X_l must be greater or equal to 0.0')                              !DEM
         call prterr('E',1)                                               
         imchn = 3                                                        
       endif                                                         
       if(temp(7) .le. 0.0) then                                         
         write (errbuf(1),1180) nbname,bkv,nid,extmsg                   !DEM
 1180    format (2x,a8,2x,f5.1,2x,a1,2x,a16,1x,                         !DEM
     1     'K_c must be greater than 0.0')                              !DEM
         call prterr('E',1)                                               
         imchn = 3                                                        
       endif                                                         
       if(temp(6) .le. 0.0) then                                         
         write (errbuf(1),1190) nbname,bkv,nid,extmsg                   !DEM
 1190    format (2x,a8,2x,f5.1,2x,a1,2x,a16,1x,                         !DEM
     1     'V_g_max must be greater than 0.0')                          !DEM
         call prterr('E',1)                                               
         imchn = 3                                                        
       endif                                                         
       if (imchn .gt. 0) return                                            
C                                                                       
C STORE FL EXCITER DATA FROM FZ CARD                                    
C                                                                       
C  KI                                                                   
       citer(5,isgg) = temp(1 )                                      
C  KP                                                                   
       citer(7,isgg) = temp(2 )                                      
C  THEATA P                                                             
       citer(28,isgg) = temp(3 )                                     
C  EFDMAX                                                               
       citer(4,isgg) = temp(4 )                                      
C  KG                                                                   
       citer(24,isgg) = temp(5 )                                     
C VGMAX                                                                 
       citer(19,isgg) = temp(6 )                                     
C KC                                                                    
       citer(10,isgg) = temp(7 )                                     
C XL                                                                    
       citer(20,isgg) = temp(8 )                                     
C                                                                       
C CONVERT MACHINE BASE IMPEDANCES TO SYSTEM BASE                        
C                                                                       
       if(bmvac .ne. 0.0) then                                          
C XL                                                                    
       citer(20,isgg) = citer(20,isgg)*bmvac                            
C RC                                                                    
       citer(21,isgg) = citer(21,isgg)*bmvac                            
C XC                                                                    
       citer(23,isgg) = citer(23,isgg)*bmvac                            
C KI                                                                    
       citer(5,isgg) = citer(5,isgg)*bmvac                              
                                                                        
       endif                                                            
       return                                                        
       end                                                           
