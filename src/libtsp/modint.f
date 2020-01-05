C    %W% %G%
      subroutine modint(imod)                                           
C * * *                                                                 
C * * * THIS SUBROUTINE CALCULATES INITIAL VALUES FOR HIGH POWER,LOW POW
C * * * AND DUAL FREQUENCY MODULATION                                   
C * * *                                                                 
      include 'tspinc/params.inc' 
      include 'tspinc/ectba.inc' 
      include 'tspinc/comn34.inc' 
      include 'tspinc/namec.inc' 
      include 'tspinc/busvolt.inc' 
      include 'tspinc/ecsind.inc' 
      include 'tspinc/matrow.inc' 
      include 'tspinc/prt.inc' 
      include 'tspinc/pointr.inc' 
      include 'tspinc/param.inc' 
      include 'tspinc/dcmodd.inc' 
      character*8 bus1, bus2                                            

      if(itab(7) .eq. 2 .and. itab(114) .gt. 2)then                     
         ib1 = itab(35)                                                 
         jb1 = itab(37)                                                 
         bus1 = exnamc(ib1)                                             
         bus2 = exnamc(jb1)                                             
         write(errbuf(1),100)bus1,bus2                                  
 100     format(1h0,5x,'DC LINE ',a8,' TO ',a8,' HAS CONSTANT CURRENT', 
     1   ' MODE AND MAY ONLY HAVE LOW LEVEL MODULATION.')               
         call prterr('E',1)                                             
         iabort = 1                                                     
      endif                                                             
      tdmod1(imod) = tdmod1(imod) * frqbse                              
      tfmod1(imod) = tfmod1(imod) * frqbse                              
      if(emod1(imod) .le. 0.0)then                                      
         ib1 = itab(35)                                                 
         jb1 = itab(37)                                                 
         bus1 = exnamc(ib1)                                             
         bus2 = exnamc(jb1)                                             
         write(errbuf(1),200)bus1,bus2                                  
  200    format(1h0,5x, 'DC MODULATION ON LINE FROM ',a8,' TO ',a8,     
     1   ' HAS E CONSTANT LESS THAN OR EQUAL TO 0.0.')                  
         call prterr('E',1)                                             
         iabort = 1                                                     
         return                                                         
      endif                                                             
      emod1(imod) = frqbse / emod1(imod)                                
      camod1(imod) = camod1(imod)/frqbse                                
      cbmod1(imod) = cbmod1(imod)/(frqbse*frqbse)                       
      ccmod1(imod) = ccmod1(imod)/frqbse                                
      cdmod1(imod) = cdmod1(imod)/(frqbse*frqbse)                       
C  CMODK1 CONTAINS INPUT DATA VALUE OF GAIN 'K' FOR MODULATION CONTROL. 
C  UNITS OF K DEPEND UPON THE TYPE OF INPUT AND THE VALUE OF K MAY      
C  HAVE TO BE CHANGED BY A FACTOR.  SAME IS TRUE OF TAB(185).           
C * * *                                                                 
C * * * FOR LOW LEVEL MODULATION GAIN IS INPUTTED AS AMPERE SECS/MW     
C * * * CHANGE TO PER UNIT AMPERE CYCLES/MW                             
C * * *                                                                 
      if(itab(114) .eq. 6) go to 1000                                   
      if(itab(114) .eq. 1 .or. itab(114) .eq. 2)then                    
         ckmod1(imod) = ckmod1(imod)*frqbse/1000.                       
      else                                                              
C * * *                                                                 
C * * * FOR HIGH LEVEL MODULATION GAIN IS INPUTTED AS MW SECS/MW        
C * * * CHANGE TO MW CYCLES/MW                                          
C * * *                                                                 
         ckmod1(imod) = ckmod1(imod)*frqbse                             
      endif                                                             
      if(itab(114) .eq. 1 .or. itab(114) .eq. 2) then                   
C * * *                                                                 
C     CHANGE LIMITS FROM POWER TO CURRENT IN PER UNIT                   
C * * *                                                                 
         pxmod1(imod) = pxmod1(imod)*ptab(14)                           
         pnmod1(imod) = pnmod1(imod)*ptab(14)                           
      else                                                              
         pxmod1(imod) = pxmod1(imod) - ptab(5)                          
         pnmod1(imod) = pnmod1(imod) - ptab(5)                          
      endif                                                             
C * * *                                                                 
C     OBTAIN INITAL BRANCH POWER/CURRENT FOR MONITORED LINE             
C * * *                                                                 
  300 ibs = idcbs1(imod)                                                
      jbs = jdcbs1(imod)                                                
      call getmat(ibs, ii)                                                  
      lens=matrow(2)                                                    
      loclen=matrow(3)                                                  
      indx=4                                                            
      if (lens.eq.0) go to 500                                          
      do 400 i=1,lens                                                   
      if (matrow(indx).eq.jbs) go to 800                                
  400 indx=indx+3                                                       
  500 do 600 i=1,loclen                                                 
      if (matrow(indx).eq.jbs) go to 800                                
  600 indx=indx+3                                                       
      write (errbuf(1),700)                                             
      call prterr ('E',1)                                               
  700 format ('0DC CNTRL CKT BUS NO. NOT FOUND')                        
      iabort=1                                                          
      go to 900                                                         
C * * *                                                                 
C     BRANCH ADMITTANCE FOUND SO STORE AWAY                             
C * * *                                                                 
 800  gij=-atrow(indx+1)                                                
      dcgij(imod) = gij                                                 
      bij=-atrow(indx+2)                                                
      dcbij(imod) = bij                                                 
C * * *                                                                 
C     CALCULATE INITAL BRANCH PWR FOR DC CONTROL BLK                    
C * * *                                                                 
 900  edc1 = eyr(ibs)                                                   
      fdc1 = eyi(ibs)                                                   
      edc2 = eyr(jbs)                                                   
      fdc2 = eyi(jbs)                                                   
      de=edc1-edc2                                                      
      df=fdc1-fdc2                                                      
      cr=de*gij-df*bij                                                  
      ci=de*bij+df*gij                                                  
      if(itab(114) .eq. 1 .or. itab(114) .eq. 3)then                    
C * * *                                                                 
C * * * BRANCH POWER INPUT                                              
C * * *                                                                 
         brpwr = bmva*(edc1*cr+fdc1*ci)                                 
      else                                                              
C * * *                                                                 
C * * * BRANCH CURRENT INPUT                                            
C * * *                                                                 
         ibkv = ixnamn(ibs)                                             
         bkv = basekv(ibkv)                                             
         brpwr = sqrt(cr*cr +ci*ci)*(bmva/bkv)*(1000./1.73203)          
      endif                                                             
C * * *                                                                 
C     INITALIZE STATE VARIABLES + SWITCHES                              
C * * *                                                                 
      x1omod(imod) = brpwr                                              
      x0omod(imod) = brpwr                                              
      x1nmod(imod) = brpwr                                              
      x0nmod(imod) = brpwr                                              
      xoomod(imod) = brpwr                                              
      return                                                            
C * * *                                                                 
C * * * INITIALIZE SECOND SET OF VARIABLES FOR DUAL FREQUENCY           
C * * *                                                                 
C  * * *                                                                
C     FOR FREQ BASED DC MODULATION, GAIN 'K' IS INPUTTED AS MW-SEC/HZ.  
C     WE WILL CHANGE IT TO MW-CYC.CYC/RAD .                             
 1000 ckmod1(imod) = ckmod1(imod)*(frqbse*frqbse/6.2831852)             
      tdmod2(imod) = tdmod2(imod) * frqbse                              
      tfmod2(imod) = tfmod2(imod) * frqbse                              
      if(emod2(imod) .le. 0.0)then                                      
         ib1 = itab(35)                                                 
         jb1 = itab(37)                                                 
         bus1 = exnamc(ib1)                                             
         bus2 = exnamc(jb1)                                             
         write(errbuf(1),200)bus1,bus2                                  
                                                                        
         call prterr('E',1)                                             
         iabort = 1                                                     
         return                                                         
      endif                                                             
      emod2(imod) = frqbse / emod2(imod)                                
      camod2(imod) = camod2(imod)/frqbse                                
      cbmod2(imod) = cbmod2(imod)/(frqbse*frqbse)                       
      ccmod2(imod) = ccmod2(imod)/frqbse                                
      cdmod2(imod) = cdmod2(imod)/(frqbse*frqbse)                       
      ckmod2(imod) = ckmod2(imod)*(frqbse*frqbse/ 6.2831852)            
      ibs = idcbs1(imod)                                                
      jbs = jdcbs1(imod)                                                
      dceo1(imod) = eyr(ibs)                                            
      dceo2(imod) = eyr(jbs)                                            
      dcfo1(imod) = eyi(ibs)                                            
      dcfo2(imod) = eyi(jbs)                                            
      return                                                            
      end                                                               
