C    %W% %G%
      subroutine brksol                                                 
C * * *                                                                 
C * * * THIS SUBROUTINE CONTAINS THE SOLUTION LOGIC FOR THE             
C * * * RESISTIVE BRAKE MODEL.  IT IS CALLED BY CNTRL.                  
C * * * IT CALLS GETMAT AND PUTMAT.                                     
C * * *                                                                 
      include 'tspinc/params.inc' 
      include 'tspinc/lnk12.inc' 
      include 'tspinc/prt.inc' 
      include 'tspinc/vrgov.inc' 
      include 'tspinc/param.inc' 
      include 'tspinc/bname.inc' 
      include 'tspinc/buskv.inc' 
      include 'tspinc/matrow.inc' 
      include 'tspinc/busvolt.inc' 
      include 'tspinc/pointr.inc' 
      include 'tspinc/brake1.inc' 
      include 'tspinc/rbcom.inc' 
      include 'tspinc/prate.inc' 
      character*8 name, name1, name2                                    
      character*1 id                                                    
      bdto=edt                                                          
      if(idisw.eq.2) bdto=0.0                                           
      do 1500 n=1,nbrake                                                
      irc=0                                                             
      ibusb = ibkbno(n)                                                 
      id = ibkid(n)                                                     
      name = bname(ibusb)                                               
      base = buskv(ibusb)                                               
      ibus = ibkib1(n)                                                  
      jbus = ibkjb1(n)                                                  
      gij = bkgij1(n)                                                   
      bij = bkbij1(n)                                                   
C * * *                                                                 
C * * * CALCLULATE POWER FLOW ON SPECIFIED LINE IBUS TO JBUS            
C * * *                                                                 
  100 ei = eyr(ibus)                                                    
      fi = eyi(ibus)                                                    
      ej = eyr(jbus)                                                    
      fj = eyi(jbus)                                                    
      eij = ei - ej                                                     
      fij = fi - fj                                                     
      ctr = eij * gij - fij * bij                                       
      cti = eij * bij + fij * gij                                       
      pwr=ei*ctr+fi*cti                                                 
      if(irc.ne.0) go to 300                                            
      bkx1o(n) = bkx1n(n)                                               
      bkx2o(n) = bkx2n(n)                                               
      bkx1n(n) = pwr                                                    
      if(idisw.eq.2) go to 110                                          
      twdt2=2.*bktw(n)/bdto                                             
      bkx2n(n)=(bkx2o(n)*(twdt2-1.)+twdt2*(bkx1n(n)-bkx1o(n)))          
     1         /(twdt2+1.)                                              
      go to 120                                                         
  110 bkx2n(n) = bkx2o(n) + bkx1n(n) - bkx1o(n)                         
  120 pdrop = bkx2o(n) - bkx2n(n)                                       
      vlow1=sqrt(ej*ej+fj*fj)                                           
C * * *                                                                 
C * * * IF MODEL INCLUDES AN RC CARD, GO BACK AND CALCULATE POWER       
C * * * ON SECOND LINE.                                                 
C * * *                                                                 
      if(ibrkc(n) .eq. 0) go to 420                                     
  200 ibus = ibkib2(n)                                                  
      jbus = ibkjb2(n)                                                  
      gij = bkgij2(n)                                                   
      bij = bkbij2(n)                                                   
      irc=1                                                             
      go to 100                                                         
  300 bkx3o(n) = bkx3n(n)                                               
      bkx4o(n) = bkx4n(n)                                               
      bkx5o(n) = bkx5n(n)                                               
      bkx3n(n) = pwr                                                    
      if(idisw.eq.2) go to 410                                          
      if(bkconc(n) .eq. 0.0) go to 400                                  
      consap = 2.*bkcona(n)/bdto                                        
      tdt2 = 2.*bkconc(n)/bdto                                          
      bkx4n(n)=(bkx4o(n)*(tdt2-1.)-tdt2*(bkx3n(n)-bkx3o(n)))/(tdt2+1.)  
c     -  Calc on next line done with global vars                        !dem
c     X4N=(X4O*(TDT2-1.)-TDT2*(X3N-X3O))/(TDT2+1.)                      
      bkx5n(n)=(bkx5o(n)*(consap-bkconb(n))+bkx4n(n)+bkx4o(n))/         
     1         (consap+bkconb(n))                                       
      go to 420                                                         
  400 t1dt2=2.*bkcona(n)/bdto+1.                                        
      bkx4n(n) = (bkx4o(n)*(t1dt2-2.)+bkx3n(n)+bkx3o(n))/t1dt2          
      if(bkcona(n) .eq. 0.) bkx4n(n) = bkx3n(n)                         
      t2dt2 = 2.*bkconb(n)/bdto + 1.                                    
      bkx5n(n) = (bkx5o(n)*(t2dt2-2.)+(2.*frqbse/bdto)*                 
     1            (bkx4n(n)-bkx4o(n)))/t2dt2                            
      go to 420                                                         
  410 if(bkcona(n) .eq. 0.) go to 415                                   
      if(bkconc(n) .eq. 0.) go to 416                                   
      bkx4n(n) = bkx4o(n) - (bkx3n(n) - bkx3o(n))                       
      bkx5n(n) = bkx5o(n)                                               
      go to 420                                                         
  415 bkx4n(n) = bkx3n(n)                                               
      go to 417                                                         
  416 bkx4n(n) = bkx4o(n)                                               
  417 bkx5n(n) = bkx5o(n) + (bkx4n(n) - bkx4o(n))/(bkconb(n)/frqbse)    
C * * *                                                                 
C * * *   CHECK TIMER 5                                                 
C * * *                                                                 
  420 if(bktim5(n) .gt. 0.0) bktim5(n) = bktim5(n) - bdto               
C * * *                                                                 
C * * *  CHECK TIMER 3                                                  
C * * *                                                                 
  430 if(bktim3(n) .eq. 0.0)go to 700                                   
      bktim3(n) = bktim3(n) - bdto                                      
      if(bktim3(n) .gt. 0.0) go to 1300                                 
      bktim3(n) = 0.0                                                   
      ibrk=0                                                            
      write (outbuf,440) name,base,id,to                                
      call prtout (1)                                                   
  440     format(1h0, ' BRAKE AT ', a8, 1x, f5.1, 1x, a1, ' HAS ',      
     1           'COMPLETED THE -OFF- CYCLE AT TIME ', f8.4,            
     2           ' CYCLES.')                                            
  500 if(ibrk.eq.0) go to 1200                                          
      go to 1300                                                        
C * * *                                                                 
C * * *   CHECK TIMER 2                                                 
C * * *                                                                 
  700 if(bktim2(n) .eq. 0.0) go to 1100                                 
      bktim2(n) = bktim2(n) - bdto                                      
      if(bktim2(n) .gt. 0.0) go to 900                                  
  800 bktim2(n) = 0.0                                                   
      bktim3(n) = bktded(n)                                             
      gbrke = -bkgbrk(n)                                                
      write (outbuf,820) name,base,id,to                                
      call prtout (1)                                                   
  820     format(1h0,'  BRAKE AT ',a8,1x,f5.1,1x,a1,' HAS COMPLETED THE 
     1-on- cycle and is starting the -off- cycle at time ', F8.4,       
     2' CYCLES')                                                        
C * * *                                                                 
C * * *  BRAKE  ON/OFF LOGIC                                            
C * * *                                                                 
  840 if(ibusb.lt.lfrst) lfrst=ibusb                                    
      call getmat(ibusb, ii)                                                
      atrow(ii-1) = atrow(ii-1) + gbrke                                   
      call putmat(ibusb, ii)                                                
      ivpc=2                                                            
      go to 1300                                                        
  900 if(ibrkc(n) .eq. 0) go to 1300                                    
      if(bktim4(n) .eq. 0.0) go to 920                                  
      bktim4(n) = bktim4(n) - bdto                                      
      if(bktim4(n) .gt. 0.0)go to 1300                                  
      bktim4(n) = 0.0                                                   
      go to 800                                                         
C * * *                                                                 
C * * *  CHECK IF TBLOCK TIMER FOR BRAKE OFF LOGIC                      
C * * *                                                                 
  920 if(bktim5(n) .gt. 0.0) go to 1300                                 
C * * *                                                                 
C * * *  RC CARD LOGIC TO CHECK IF THE BRAKE SHOULD GO 'OFF'            
C * * *                                                                 
 1000 if(bkx5n(n) .gt. bktrg2(n)) go to 1300                            
      if(bkconc(n) .eq. 0.0) go to 1010                                 
      if(bkx4n(n) .gt. 0.0) go to 1300                                  
 1010 ibus2 = ibkib2(n)                                                 
      jbus2 = ibkjb2(n)                                                 
      name1 = bname(ibus2)                                              
      base1 = buskv(ibus2)                                              
      name2 = bname(jbus2)                                              
      base2 = buskv(jbus2)                                              
      write (outbuf,1020) name1,base1,name2,base2                       
      call prtout (1)                                                   
 1020     format(1h0,' BRAKE -OFF- SIGNAL WAS RECEIVED FROM LINE ',     
     1           a8, 1x, f5.1, 8x, a8, 1x, f5.1)                        
      bktim4(n) = bktrp2(n)                                             
      write (outbuf,1040) name,base,id,to                               
      call prtout (1)                                                   
 1040     format(1h0,'  BRAKE AT ', a8, 1x, f5.1, 1x, a1, ' HAS RECEIVE'
     1           , 'D THE -OFF- SIGNAL AND IS STARTING THE -DELAY- ',   
     2           'PERIOD AT TIME ', f8.4, ' CYCLES.')                   
      write (outbuf,1060) bkx4n(n),bkx5n(n)                             
      call prtout (1)                                                   
      call skipln (1)                                                   
 1060 format(1h ,'  OUTPUT1 =  ',e15.5,'  OUTPUT2 =  ',e15.5)           
      go to 1300                                                        
C * * *                                                                 
C * * *   CHECK TIMER 1                                                 
C * * *                                                                 
 1100 if(bktim1(n) .eq. 0.0) go to 1200                                 
      bktim1(n) = bktim1(n) - bdto                                      
      if(bktim1(n) .gt. 0.0) go to 1300                                 
      bktim1(n) = 0.0                                                   
      bktim2(n) = bktrp1(n)                                             
      gbrke = bkgbrk(n)                                                 
      write (outbuf,1120) name,base,id,to                               
      call prtout (1)                                                   
 1120     format(1h0, '  BRAKE AT ', a8, 1x, f5.1, 1x, a1, ' HAS ',     
     1           'COMPLETED THE -DELAY- PERIOD AND IS STARTING THE -ON-'
     2           ,' CYCLE AT TIME ', f8.4, ' CYCLES.')                  
      go to 840                                                         
C * * *                                                                 
C * * * CHECK RC CARD LOGIC TO SEE IF BRAKE SHOULD BE INSERTED          
C * * *                                                                 
 1130 if(ibrkc(n) .eq. 0) go to 1300                                    
      if(bkconc(n) .eq. 0.0) go to 1300                                 
      if(bkx5n(n) .lt. bkfins(n)) go to 1300                            
      ibus2 = ibkib2(n)                                                 
      jbus2 = ibkjb2(n)                                                 
      name1 = bname(ibus2)                                              
      base1 = buskv(ibus2)                                              
      name2 = bname(jbus2)                                              
      base2 = buskv(jbus2)                                              
      write (outbuf,1220) name1,base1,name2,base2                       
      call prtout (1)                                                   
      go to 1230                                                        
C * * *                                                                 
C * * * CHECK RB CARD LOGIC TO SEE IF BRAKE SHOULD BE 'ON'              
C * * *                                                                 
 1200 if(vlow1 .gt. bkvlow(n)) go to 1130                               
      if(pdrop .lt. bktrg1(n)) go to 1130                               
      ibus1 = ibkib1(n)                                                 
      jbus1 = ibkjb1(n)                                                 
      name1 = bname(ibus1)                                              
      base1 = buskv(ibus1)                                              
      name2 = bname(jbus1)                                              
      base2 = buskv(jbus1)                                              
      write (outbuf,1220) name1,base1,name2,base2                       
      call prtout (1)                                                   
 1220     format(1h0, ' BRAKE -ON- SIGNAL WAS RECEIVED FROM LINE ',     
     1           a8, 1x, f5.1, 9x, a8, 1x, f5.1)                        
 1230 bktim1(n) = bktnst(n)                                             
      bktim5(n) = bktblk(n)                                             
      ibus = ibkbno(n)                                                  
      name = bname(ibus)                                                
      base = buskv(ibus)                                                
      write(outbuf,1240) name,base,ibkid(n),to                          
      call prtout (1)                                                   
 1240 format(1h0, '  BRAKE AT ', a8, 1x, f5.1, 1x, a1, ' HAS',          
     1           ' RECEIVED THE -ON- SIGNAL AND IS STARTING THE',       
     2           ' -DELAY- PERIOD AT TIME ', f8.4, ' CYCLES.')          
      write (outbuf,1260) pdrop,vlow1,bkx5n(n)                          
      call prtout (1)                                                   
      call skipln (1)                                                   
 1260 format(1h ,'  PDROP =  ',e15.5,'  BUSVLT =  ',e15.5,'  FRQ. EST  =
     1  ',e15.5)                                                        
      ibrk=1                                                            
      go to 500                                                         
 1300 continue                                                          
 1500 continue                                                          
      return                                                            
      end                                                               
