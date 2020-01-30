C    %W% %G%
      subroutine gaminp(i,j)                                            
C * * *                                                                 
C * * * THIS SUBROUTINE DECODES THE GAMMA MODULATION CARD AND           
C * * * PLACES THE DATA IN THE PROPER TABLES.  I COUNTS THE             
C * * * NUMBER OF GAMMA MODULATION CARDS.  J IS THE POINTER             
C * * * TO THE DCCARD TABLE FOR THE DS CARD.  IT IS CALLED BY DCINP     
C * * * AND MDCINP                                                      
C * * *                                                                 
      include 'tspinc/params.inc' 
      include 'tspinc/pointr.inc' 
      include 'tspinc/prt.inc' 
      include 'tspinc/param.inc' 
      include 'tspinc/dcard.inc' 
      include 'tspinc/dcinfo.inc' 
      include 'tspinc/gamma.inc' 
      include 'tspinc/busnum.inc' 
      character*1 idc                                                   
C * * *                                                                 
C * * * READ DS CARD FOR GAMMA MODULATION                               
C * * *                                                                 
C     -     begin     begin     begin     begin     begin     begin 
      read (dccd80(j),1000) gamdt1(i),gamdt3(i),gamdt4(i),gamdt5(i),     
     1  gamdt6(i),cgama(i),gamax(i),agam(i),bgam(i),gamin(i),idc          
 1000 format (bz,15x,5f5.1,f6.2,f5.1,2f1.0,1x,f5.1,8x,a1)                   
C * * *                                                                 
C * * * IF IGAMDC(I) = 1, DC TERMINAL VOLTAGE WILL BE USED AS INPUT     
C * * *                                                                 
      if (idc .eq. 'D')then                                              
         igamdc(i) = 1                                                  
      else                                                              
         igamdc(i) = 0                                                  
      endif                                                             
C * * *                                                                 
C * * * GET BUS NUMBER OF REMOTE BUS FOR VOLTAGE SIGNAL                 
C * * *                                                                 
      igamrb(i) = ii3                                                   
C * * *                                                                 
C * * * CONVERT TIME CONSTANTS FROM SECONDS TO CYCLES                   
C * * *                                                                 
      gamdt1(i) = gamdt1(i)*frqbse                                      
      gamdt3(i) = gamdt3(i)*frqbse                                      
      gamdt4(i) = gamdt4(i)*frqbse                                      
      gamdt5(i) = gamdt5(i)*frqbse                                      
      gamdt6(i) = gamdt6(i)*frqbse                                      
C * * *                                                                 
C * * * CONVERT GAMMA MAX AND GAMMA MIN TO FROM DEGREES TO RADIANS      
C * * *                                                                 
      gamax(i) = gamax(i)*0.0174533                                     
      gamin(i) = gamin(i)*0.0174533                                     
      if (gamin(i) .ge. gamax(i)) then                                   
         write(errbuf(1),2000) (dccard(ix,j),ix = 1,8)                  
 2000    format(8a10)                                                   
         write(errbuf(2),2500)                                          
 2500    format(1h0,' GAMMA MIN IS GREATER THAN GAMMA MAX ')            
         call prterr('E',2)                                             
         iabort = 1                                                     
         modcod = 0                                                     
      endif                                                             
C * * *                                                                 
C * * * CONVERT GAMMA K CONSTANT FROM DEGREES/VOLTS PU TO               
C * * * RADIANS/VOLTS PU                                                
C * * *                                                                 
      cgama(i) = cgama(i) * 0.0174533                                   
      modcod = 5                                                        
      return                                                            
      end                                                               
