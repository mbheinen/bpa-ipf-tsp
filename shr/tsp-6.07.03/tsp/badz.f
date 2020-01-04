C    %W% %G%
      subroutine badz(ts)                                               
C * * *                                                                 
C * * * THIS SUBROUTINE CALCULATES APPARENT IMPEDANCES FOR EACH         
C * * * LINE IN THE STUDY AND PRINTS OUT A LIST OF LINES WHOSE          
C * * * ANGLE EXCEEDS AREF AS ENTERED ON THE MH CARD.  THIS             
C * * * SUBROUTINE ALSO CALCULATES THE BUS VOLTAGE MAGNITUDE AT         
C * * * EACH BUS AND PRINTS OUT THE VOLTAGES THAT EXCEED THE            
C * * * LINIMS EXMAX AND EXMIN AS ENTERED ON THE MH CARD.               
C * * * IT IS CALLED BY NOUT2. TS IS THE TIME STEP IN CYCLES.           
C * * *                                                                 
      include 'tspinc/params.inc' 
      include 'tspinc/room.inc' 
      include 'tspinc/dc.inc' 
      include 'tspinc/wstequ.inc' 
      include 'tspinc/out512.inc' 
      include 'tspinc/prt.inc' 
      include 'tspinc/angle.inc' 
      include 'tspinc/mtbl.inc' 
      include 'tspinc/newtab.inc' 
      include 'tspinc/link56.inc' 
      include 'tspinc/indx2n.inc' 
      dimension rimp2(500),rimp4(500),rimp5(500),rimp6(500),            
     1          rvolm(500),rvolb(500)                                   
      character*8 cimp1(500),cimp3(500),cvoln(500)                      
      character*8 ibus1,ibus2,ewbus                                     
      k=1                                                               
      jnear=0                                                           
      do 1900 i=1,itbld                                                 
      inear=mtbl(1,i)                                                   
      ifar=mtbl(2,i)                                                    
      if (inear.eq.jnear) go to 1000                                    
      jnear=inear                                                       
      enear=eyr(inear)                                                  
      fnear=eyi(inear)                                                  
 1000 efar=eyr(ifar)                                                    
      ffar=eyi(ifar)                                                    
      oposit=enear*ffar-fnear*efar                                      
      adjcnt=enear*efar+fnear*ffar                                      
      if (adjcnt.lt.0.0) go to 1200                                     
      if (abs(oposit).lt.tref*abs(adjcnt)) go to 1900                   
 1200 sqnear=enear**2+fnear**2                                          
      sqfar=efar**2+ffar**2                                             
      sum=sqnear+sqfar-2.0*adjcnt                                       
      if(sum .ne. 0.0)then                                              
         denom = 1.0/sum                                                
      else                                                              
         denom = 0.0                                                    
      endif                                                             
      areal=sqnear-adjcnt                                               
      piji=0.0                                                          
      if (oposit.eq.0.0.and.areal.eq.0.0) go to 1400                    
      piji=degrad*atan2(oposit,areal)                                   
      piji = piji+pij(i)                                                
      if (piji.gt.180.0.or.piji.lt.0.0) go to 1600                      
 1400 nmx3=4000                                                         
      ibus1 = newtbc(inear)                                             
      kbase = inwtb(inear)                                              
      base1=basekv(kbase)                                               
      ibus2 = newtbc(ifar)                                              
      kbase = inwtb(ifar)                                               
      base2=basekv(kbase)                                               
      cimp1(k) = ibus1                                                  
      rimp2(k) = base1                                                  
      cimp3(k) = ibus2                                                  
      rimp4(k) = base2                                                  
      rimp5(k) = sqrt(sqnear*denom)*zij(i)                              
      rimp6(k) = piji                                                   
C * * *                                                                 
C * * * CHECK FOR TABLE OVERFLOW                                        
C * * *                                                                 
      if(k .eq. 500) go to 2000                                         
      k = k+1                                                           
 1600 areal=sqfar-adjcnt                                                
      oposit=-oposit                                                    
      piji=0.0                                                          
      if (oposit.eq.0.0.and.areal.eq.0.0) go to 1800                    
      piji=degrad*atan2(oposit,areal)                                   
      piji = piji+pij(i)                                                
      if (piji.gt.180.0.or.piji.lt.0.0) go to 1900                      
 1800 nmx3=4000                                                         
      ibus1 = newtbc(ifar)                                              
      kbase = inwtb(ifar)                                               
      base1=basekv(kbase)                                               
      ibus2 = newtbc(inear)                                             
      kbase = inwtb(inear)                                              
      base2=basekv(kbase)                                               
      cimp1(k) = ibus1                                                  
      rimp2(k) = base1                                                  
      cimp3(k) = ibus2                                                  
      rimp4(k) = base2                                                  
      rimp5(k) = sqrt(sqfar*denom)*zij(i)                               
      rimp6(k) = piji                                                   
C * * *                                                                 
C * * * CHECK FOR TABLE OVERFLOW                                        
C * * *                                                                 
      if(k .eq. 500) go to 2000                                         
      k = k+1                                                           
 1900 continue                                                          
      go to 2400                                                        
 2000 write(errbuf(1),2200)                                             
 2200 format(1h0,' WARNING BAD IMPEDANCES TABLES EXCEEDED--OUTPUT IS',  
     1' BEING TRUNCATED.')                                              
      call prterr('W',1)                                                
 2400 if (k.eq.1) go to 3400                                            
      k=k-1                                                             
      write (outbuf,2800) ts                                            
      call prtout (1)                                                   
      write (outbuf,3000)                                               
      call prtout (1)                                                   
      do 2600 jjj = 1,k,2                                               
      kkk = min0 (jjj+1,k)                                              
      write (outbuf,3200) (cimp1(i),rimp2(i),cimp3(i),rimp4(i),         
     1        rimp5(i),rimp6(i), i = jjj,kkk)                           
      call prtout (1)                                                   
 2600 continue                                                          
 2800 format(1h0,29x,'BAD IMPEDANCES AT ',f7.2,'CYCLES')                
 3000 format(1h ,29x,  'BRANCH,APPARENT IMPEDANCE (MAGNITUDE AND ANGLE)'
     1)                                                                 
 3200 format(12x,2(a8,f6.1,2x,a8,f6.1,2x,f11.5,f11.2,2x))               
 3400 k = 0                                                             
C * * *                                                                 
C * * *           ABNORMAL VOLTAGE OUTPUT                               
C * * *                                                                 
      do 3800 izold=1,ntotd                                             
      iz = indx2n(izold)                                                
      if ((iz.gt.nmx).or.(iz.le.0)) go to 3800                          
      esr=eyr(iz)                                                       
      esi=eyi(iz)                                                       
      emag=sqrt(esr*esr+esi*esi)                                        
      if (emag.ge.exmax.or.emag.lt.exmin) go to 3600                    
      go to 3800                                                        
3600  k = k+1                                                           
      rvolm(k) = emag                                                   
      ewbus = newtbc(iz)                                                
      kbase = inwtb(iz)                                                 
      ewbase=basekv(kbase)                                              
      cvoln(k) = ewbus                                                  
      rvolb(k) = ewbase                                                 
C * * *                                                                 
C * * * CHECK FOR TABLE OVERFLOW                                        
C * * *                                                                 
      if(k .eq. 500) go to 4000                                         
 3800 continue                                                          
      go to 4400                                                        
 4000 write(errbuf(1),4200)                                             
 4200 format(1h0,' WARNING BAD BUS VOLTAGE TABLES EXCEEDED--OUTPUT IS', 
     1' TRUNCATED.')                                                    
      call prterr('W',1)                                                
 4400 if(nopz .gt. 0)go to 5400                                         
      if (k.eq.0) go to 5400                                            
      write (outbuf,4800) ts                                            
      call prtout (1)                                                   
      write (outbuf,5000)                                               
      call prtout (1)                                                   
      do 4600 jjj = 1,k,5                                               
      kkk = min0 (jjj+4,k)                                              
      write (outbuf,5200) (cvoln(i),rvolb(i),rvolm(i),i=jjj,kkk)        
      call prtout (1)                                                   
 4600 continue                                                          
 4800 format(1h0,29x,'BAD VOLTAGES AT ',f7.2,' CYCLES')                 
 5000 format(1h ,29x,' BUS BASE EMAG ')                                 
 5200 format(12x,5(a8,f6.1,f7.4,3x))                                    
 5400 return                                                            
      end                                                               
