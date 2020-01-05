C    %W% %G%
      subroutine gamint(igam,vdcgam)                                    
C * * *                                                                 
C * * * THIS SUBROUTINE INITIALIZES VARIABLES FOR THE GAMMA MODULATION  
C * * * MODEL USED WITH THE TWO TERMINAL AND MULTITERMINAL DC LINE.     
C * * * IT IS CALLED BY DCINT AND MDCINT.                               
C * * *                                                                 
      include 'tspinc/params.inc' 
      include 'tspinc/param.inc' 
      include 'tspinc/pointr.inc' 
      include 'tspinc/namec.inc' 
      include 'tspinc/busvolt.inc' 
      include 'tspinc/gamma.inc' 
      include 'tspinc/prt.inc' 
      character*8 name                                                  
      data radeg / 57.29578/                                            
C * * * INITALIZE AC VOLTAGE INPUT AND VREF                             
      ino = igamno(igam)                                                
      if(igamdc(igam) .eq. 1)then                                       
         vac(igam) = vdcgam                                             
         go to 50                                                       
      endif                                                             
      if(igamrb(igam) .eq. 0)then                                       
         vac(igam) = sqrt(eyr(ino)*eyr(ino) +eyi(ino)*eyi(ino))         
      else                                                              
         in1 = igamrb(igam)                                             
         vac(igam) = sqrt(eyr(in1)*eyr(in1) +eyi(in1)*eyi(in1))         
      endif                                                             
  50  vacref(igam) = vac(igam)                                          
      vaco(igam) = vac(igam)                                            
      vaco1(igam) = vac(igam)                                           
C * * * INITALIZE GAMMA REFERENCE                                       
                                                                        
      gamref(igam) = gama(igam)                                         
                                                                        
      name = exnamc(ino)                                                
      ibkv = ixnamn(ino)                                                
      bkv = basekv(ibkv)                                                
                                                                        
      if(gama(igam) .gt. gamax(igam))then                               
         call mpost('GAMINT')                                           
         g1 = gama(igam)*radeg                                          
         gmax = gamax(igam)*radeg                                       
         write(errbuf(1),100) name,bkv                                  
 100     format(1h0,' GAMMA MODULATION AT ',a8,1x,f5.1,' INITIAL GAMMA  
     1   ANGLE VIOLATES GAMMA MAXIMUM.')                                
         write(errbuf(2),110)g1,gmax                                    
 110     format(1h0, ' GAMMA = ',f6.2,' GAMMA MAX = ',f6.2)             
         call prterr('E',2)                                             
         iabort = 1                                                     
      endif                                                             
      if(gama(igam) .lt. gamin(igam))then                               
         call mpost('GAMINT')                                           
         g1 = gama(igam)*radeg                                          
         gmin = gamin(igam)*radeg                                       
         write(errbuf(1),200) name,bkv                                  
 200     format(1h0,' GAMMA MODULATION AT ',a8,1x,f5.1,' INITIAL        
     1   ANGLE VIOLATES GAMMA MINIMUM.')                                
         write(errbuf(2),210)g1,gmin                                    
 210     format(1h0, ' GAMMA = ',f6.2,' GAMMA MIN = ',f6.2)             
         call prterr('E',2)                                             
         iabort = 1                                                     
      endif                                                             
C * * *                                                                 
C * * * INITIALIZE TIME FACTORS                                         
C * * *                                                                 
      t2 = 2./dt                                                        
      gamdt1(igam) = gamdt1(igam)*t2 +1.                                
      gamdt3(igam) = gamdt3(igam)*t2 +agam(igam)                        
      gamdt4(igam) = gamdt4(igam)*t2 +1.                                
      gamdt5(igam) = gamdt5(igam)*t2 +bgam(igam)                        
      gamdt6(igam) = gamdt6(igam)*t2 +1.                                
C * * *                                                                 
C * * * INITALIZE PAST VALUE FACTORS                                    
C * * *                                                                 
      gamhb1(igam) = vac(igam)*(gamdt1(igam)-1.)                        
      gamhb3(igam) = 0.0                                                
      gamhb4(igam) = 0.0                                                
      gamhb5(igam) = 0.0                                                
      gamhb6(igam) = 0.0                                                
      gamden(igam) = 0.0                                                
      gamcon(igam) = 0.0                                                
      return                                                            
      end                                                               
