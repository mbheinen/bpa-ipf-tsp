C    %W% %G%
      subroutine inp3ff
C
C     This subroutine decodes data cards for the FF exciter and
c     forms the inital data tables
C
      include 'tspinc/param.inc' 
      include 'tspinc/tzro.inc' 
      include 'tspinc/params.inc' 
      include 'tspinc/prt.inc' 
      include 'tspinc/comn34.inc' 
      include 'tspinc/citer.inc' 
      include 'tspinc/inp3.inc' 
      dimension temp(13)

      if (newex .eq. 1) go to 1000
C
C     DECODE FF CARD
C
      read ( work80( icard), 100) (temp(i), i = 1, 13)              
 100  format (bz,16x, 3f5.4, 4f5.3, f5.2, 4f5.3, f4.3)
C
C     ERROR CHECKING DATA
C
      if (temp(3) .lt. tzero) then
         temp(3) = tzero                                               
         write(errbuf(1),105) nbname,bkv,nid,temp(3)                   
 105     format('0',2x,a8,2x,f5.1,2x,a1,5x,'TR is being set',          
     1           ' equal to: ',f7.5)
         call prterr('W',1)                                            
      endif                                                            

      if (temp(4) .le. 0.0 .or. temp(4) .le. temp(5)) then             
         write (errbuf(1),101) nbname,bkv,nid                          
 101     format('0',2x,a8,2x,f5.1,2x,a1,5x,'Improper VA voltage limits'
     1    ,' specified ')
         call prterr('E',1)                                            
         imchn = 3                                                     
      endif                                                            

      if (temp(6) .le. 0.0 .and. temp(7) .gt. 0.0) then                

 102     format('0',2x,a8,2x,f5.1,2x,a1,5x,'TB must be greater than '  
     1       ,'0.0 since TC is specified. ')
         call prterr('E',1)                                            
         imchn = 3                                                     
      endif                                                            

      if (temp(8) .le. 0.0) then                                       
         write (errbuf(1),110) nbname,bkv,nid                          
 110     format('0',2x,a8,2x,f5.1,2x,a1,5x,'KA must be greater than '  
     1       ,'0.0')
         call prterr('E',1)                                            
         imchn = 3                                                     
      endif                                                            

      if (temp(9) .le. 0.0) then                                       
         write (errbuf(1),120) nbname,bkv,nid                          
 120     format('0',2x,a8,2x,f5.1,2x,a1,5x,'TA must be greater than '  
     1      ,'0.0')                                                    
         call prterr('E',1)                                            
         imchn = 3                                                     
      endif                                                            

      if (temp(10) .le. 0.0 .or. temp(10) .le. temp(11)) then          
         write (errbuf(1),130) nbname,bkv,nid                          
 130     format('0',2x,a8,2x,f5.1,2x,a1,5x,'Improper VR voltage limits'
     1   ,' specified ')                                               
         call prterr('E',1)                                            
         imchn = 3                                                     
      endif                                                            

      if (temp(12) .le. 0.0) then                                      
         write (errbuf(1),140) nbname,bkv,nid                          
 140     format('0',2x,a8,2x,f5.1,2x,a1,5x,'KE must be greater than '  
     1       ,'0.0')                                                   
         call prterr('E',1)                                            
         imchn = 3                                                     
      endif                                                            

      if (temp(13) .le. 0.0) then                                      
         write (errbuf(1),160) nbname,bkv,nid                          
 160     format('0',2x,a8,2x,f5.1,2x,a1,5x,'TE must be greater than '  
     1        ,'0.0')
         call prterr('E',1)                                            
         imchn = 3                                                     
      endif                                                            

      if (imchn .gt. 0) return
C
C     Store FF exciter data into CITER table
C
      citer(21,isgg) = temp( 1)         ! RC
      citer(23,isgg) = temp( 2)         ! XC
      citer( 9,isgg) = temp( 3)*frqbse  ! TR
      citer(28,isgg) = temp( 4)         ! VAMAX
      citer(33,isgg) = temp( 5)         ! VAMIN 
      citer(17,isgg) = temp( 6)*frqbse  ! TB
      citer(18,isgg) = temp( 7)*frqbse  ! TC
      citer( 8,isgg) = temp( 8)         ! KA
      citer(25,isgg) = temp( 9)*frqbse  ! TA
      vrmax = temp(10)                  
      citer( 1,isgg) = temp(10)         ! VRMAX
      vrmin = temp(11)                  
      citer( 2,isgg) = temp(11)         ! VRMIN
      cke = temp(12)                    
      citer( 7,isgg) = temp(12)         ! KE
      citer(20,isgg) = temp(13)*frqbse  ! TE
      return                                                        
C
C     Decode data for FF exciter from FZ card
C     set NEWEX to indicate FZ has been read
C
 1000 newex = 1                                                     
      read ( work80( icard), 1100) (temp(i),i = 1, 12)              
 1100 format (bz,16x, 6f5.3, 2f5.4, 4f5.3)
C
C     Error check data from FZ card
C
      if (temp(1) .le. 0.0) then
         write (errbuf(1),1120) nbname,bkv,nid                         
1120     format('0',2x,a8,2x,f5.1,2x,a1,5x,'SE1 MUST BE GREATER THAN ' 
     1       ,'0.0')
         call prterr('E',1)                                            
         imchn = 3                                                     
      endif                                                            

      if (temp(2) .le.0.0) then                                        
         write (errbuf(1),1130) nbname,bkv,nid                         
1130     format('0',2x,a8,2x,f5.1,2x,a1,5x,'SE2 MUST BE GREATER THAN ' 
     1       ,'0.0')
         call prterr('E',1)                                            
         imchn = 3                                                     
      endif                                                            

      if (temp(1) .le. temp(2)) then                                   
         write (errbuf(1),4100) nbname,bkv,nid                         
         call prterr ('E',1)                                           
 4100    format (1h ,2x,a8,2x,f5.1,2x,a1,5x,'SE2 SHOULD BE GREATER'    
     1       ,' THAN 0.0 BUT LESS THAN SE1 ')
         imchn=3                                                       
      else if (temp(4) .le. 0.0) then                                  
         write (errbuf(1),4120) nbname,bkv,nid                         
         call prterr ('E',1)                                           
 4120    format (1h ,2x,a8,2x,f5.1,2x,a1,5x,'VE1 SHOULD BE GREATER'    
     1       ,' THAN 0.0 ')
         imchn=3                                                       
         temp(4) = 0.001                                               
      else if (temp(1) .gt. 0.0 .and. temp(2) .gt. 0.0) then           
C
C        Calculate saturation constants from maxmimum absolute field   
c        voltage                                                       
C
         ve1 = temp(4)                                                 
         sd100 = temp(1)                                               
         sd75 = temp(2)                                                
         esat = (alog(sd100/sd75)) / (0.25*ve1)                        
         csat = sd100 / (exp(esat*ve1))                                
      endif                                                            

      if (temp(5) .le.0.0) then                                        
         write (errbuf(1),1140) nbname,bkv,nid                         
1140     format('0',2x,a8,2x,f5.1,2x,a1,5x,'KF MUST BE GREATER THAN '  
     1      ,'0.0')                                                    
         call prterr('E',1)                                            
         imchn = 3                                                     
      endif                                                            

      if (temp(6) .le. 0.0) then                                       
         write (errbuf(1),1150) nbname,bkv,nid                         
1150     format('0',2x,a8,2x,f5.1,2x,a1,5x,'TF MUST BE GREATER THAN '  
     1       ,'0.0')
         call prterr('E',1)                                            
         imchn = 3                                                     
      endif                                                            

      if (temp(9) .le. 0.0) then                                       
         write (errbuf(1),1160) nbname,bkv,nid                         
1160     format('0',2x,a8,2x,f5.1,2x,a1,5x,'KB MUST BE GREATER THAN '  
     1      ,'0.0')                                                    
         call prterr('E',1)                                            
         imchn = 3                                                     
      endif                                                            

      if (temp(10) .le. 0.0) then                                      
         write (errbuf(1),1170) nbname,bkv,nid                         
1170     format('0',2x,a8,2x,f5.1,2x,a1,5x,'KL MUST BE GREATER THAN '  
     1       ,'0.0')
         call prterr('E',1)                                            
         imchn = 3                                                     
      endif                                                            

      if (temp(12) .le. 0.0) then                                      
         write (errbuf(1),1180) nbname,bkv,nid                         
 1180    format('0',2x,a8,2x,f5.1,2x,a1,5x,'VLR MUST BE GREATER THAN ' 
     1       ,'0.0')
         call prterr('E',1)                                            
         imchn = 3                                                     
      endif                                                            

      if (imchn .gt. 0) return
C
C     Voltage regulator limits could be a function of the operating    
c     point which will be computed later. Temporary field voltage 
c     limits are stored.                     
C
      citer(4,isgg) = esat              ! ESAT
      citer(5,isgg) = csat              ! CSAT
      citer(13,isgg) = temp(5 )*frqbse  ! KF
      citer(12,isgg) = temp(6 )*frqbse  ! TF
      citer(16,isgg) = temp(7)          ! KC
      citer(30,isgg) = temp(8)          ! KD
      citer(35,isgg) = temp(9 )         ! KB

C     EFDMAX = (VLR*KL*KB)/(1 + KL*KB)
 
      citer(39,isgg) = temp(12)*temp(10)*temp(9) /                  
     1                 ( 1.0 + temp(10)*temp(9))                    
      citer(40,isgg) = temp(11)         ! KH
C
C     Convert machine base impedances to system base
C
      if (bmvac .ne. 0.0) then                                      
        citer(21,isgg) = citer(21,isgg)*bmvac   ! RC
        citer(23,isgg) = citer(23,isgg)*bmvac   ! XC
      endif                                                            
C
C     Store FF exciter data from FZ card

      return                                                        
      end                                                           
