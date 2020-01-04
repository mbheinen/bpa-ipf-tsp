C    %W% %G%
      subroutine rbint                                                  
C * * *                                                                 
C * * * THIS SUBROUTINE INITIALIZES DATA TABLES FOR RESISTANCE BRAKE    
C * * * MODELS.  IT IS CALLED BY INITL4.                                
C * * *                                                                 
      include 'tspinc/params.inc' 
      include 'tspinc/pointr.inc' 
      include 'tspinc/param.inc' 
      include 'tspinc/busnum.inc' 
      include 'tspinc/busvolt.inc' 
      include 'tspinc/rbcom.inc' 
      include 'tspinc/brakn.inc' 
      include 'tspinc/prt.inc' 
      include 'tspinc/matrow.inc' 
      include 'tspinc/bname.inc' 
      include 'tspinc/buskv.inc' 
      include 'tspinc/brake1.inc' 

      character*8 bus1,bus2,bus3                                        

      nbreak = nbrake                                                   
      do 3600 itrr = 1,nbrake                                           
      bktim1(itrr)  = 0.0                                               
      bktim2(itrr)  = 0.0                                               
      bktim3(itrr)  = 0.0                                               
      bktim3(itrr)  = 0.0                                               
      bktim5(itrr)  = 0.0                                               
      bkx2n(itrr) = 0.0                                                 
      bkx2o(itrr) = 0.0                                                 
      bkx5n(itrr) = 0.0                                                 
      bkx5o(itrr) = 0.0                                                 
C * * *                                                                 
C * * * CONVERT BUS NUMBERS FROM EXTERNAL TO INTERNAL SWING ORDER       
C * * *                                                                 
      ibkbno(itrr) = indx2n(ibkbno(itrr))                               
      ibkib1(itrr) = indx2n(ibkib1(itrr))                               
      ibkjb1(itrr) = indx2n(ibkjb1(itrr))                               
C * * *                                                                 
C * * * IF AN RC CARD WAS ENTERED CONVERT THOSE BUS NUMBERS ALSO        
C * * *                                                                 
      if(ibrkc(itrr) .ne. 0)then                                        
         ibkib2(itrr) = indx2n(ibkib2(itrr))                            
         ibkjb2(itrr) = indx2n(ibkjb2(itrr))                            
      endif                                                             
      ibusb = ibkbno(itrr)                                              
      irc = 0                                                           
      ibusl = ibkib1(itrr)                                              
      jbusl = ibkjb1(itrr)                                              
C * * *                                                                 
C * * * GET LINE ADDMITANCES FOR MONITORED LINE ON RB CARD              
C * * *                                                                 
 1200 call getmat(ibusl, ii)                                                
      lens = matrow(2)                                                  
      loclen = matrow(3)                                                
      indx =4                                                           
      if(lens.eq.0) go to 1600                                          
      do  1400 irbc=1,lens                                              
      if(matrow(indx).eq.jbusl) go to 2200                              
 1400 indx = indx + 3                                                   
 1600 do 1800 irbc=1,loclen                                             
      if(matrow(indx).eq.jbusl) go to 2200                              
 1800 indx = indx+3                                                     
      bus1 = bname(ibusl)                                               
      bkv1 = buskv(ibusl)                                               
      bus2 = bname(jbusl)                                               
      bkv2 = buskv(jbusl)                                               
      write (errbuf(1), 2000)bus1,bkv1,bus2,bkv2                        
      call prterr ('E',1)                                               
 2000 format('  RESISTANCE BRAKE CARD LINE ',2(a8,2x,f5.1,2x),          
     1       ' COULD NOT BE FOUND.')                                    
      iabort =1                                                         
      if(irc.eq.0) go to 2600                                           
      go to 3600                                                        
C * * *                                                                 
C * * * STORE RESISTANCE BREAK BRANCH TRANSFER ADMITTANCE               
C * * *                                                                 
2200  gijl = -atrow(indx+1)                                             
      bijl = -atrow(indx+2)                                             
C * * *                                                                 
C * * * CALCULATE INITIAL POWER ON MONITORED LINE                       
C * * *                                                                 
 2400 ein = eyr(ibusl)                                                  
      fin = eyi(ibusl)                                                  
      ejn = eyr(jbusl)                                                  
      fjn = eyi(jbusl)                                                  
      eij = ein - ejn                                                   
      fij = fin - fjn                                                   
      ctr = eij * gijl - fij * bijl                                     
      cti = eij * bijl + fij * gijl                                     
      pwr=ein*ctr+fin*cti                                               
      if(irc.ne.0) go to 2800                                           
      bkgij1(itrr) = gijl                                               
      bkbij1(itrr) = bijl                                               
      bkpbi1(itrr) = bkpbi1(itrr)/bmva                                  
      bktrg1(itrr) = bktrg1(itrr)/bmva                                  
      if(pwr.lt. bkpbi1(itrr)) then                                     
         ib1 = ibkib1(itrr)                                             
         bus1 = bname(ib1)                                              
         bkv1 = buskv(ib1)                                              
         ib2 = ibkjb1(itrr)                                             
         bus2 = bname(ib2)                                              
         bkv2 = buskv(itrr)                                             
         go to 3000                                                     
      endif                                                             
      bktw(itrr) = bktw(itrr)*frqbse                                    
      bkx1n(itrr) = pwr                                                 
      bkx1o(itrr) = pwr                                                 
      bkgbrk(itrr) = bkgbrk(itrr)/bmva                                  
C * * *                                                                 
C * * * IF AN RC CARD WAS ENTERED JUMP BACK TO OBTAIN LINE ADMITTANCES  
C * * *                                                                 
 2600 if(ibrkc(itrr) .ne. 0) then                                       
         ibusl = ibkib2(itrr)                                           
         jbusl = ibkjb2(itrr)                                           
         irc = 1                                                        
         go to  1200                                                    
      endif                                                             
2800  bkpbi2(itrr) = bkpbi2(itrr)/bmva                                  
      bkgij2(itrr) = gijl                                               
      bkbij2(itrr) = bijl                                               
      if(pwr .lt. bkpbi2(itrr)) then                                    
         ib1 = ibkib2(itrr)                                             
         bus1 = bname(ib1)                                              
         bkv1 = buskv(ib1)                                              
         ib2 = ibkjb2(itrr)                                             
         bus2 = bname(ib2)                                              
         bkv2 = buskv(itrr)                                             
         go to 3000                                                     
      endif                                                             
      bkx3n(itrr) = pwr                                                 
      bkx3o(itrr) = pwr                                                 
      bkcona(itrr) = bkcona(itrr)*frqbse                                
      if(bkconc(itrr) .ne. 0.0) then                                    
         bkconc(itrr) = bkconc(itrr)*frqbse                             
         go to 3600                                                     
      endif                                                             
      bkconb(itrr) = bkconb(itrr)*frqbse                                
      bkx4n(itrr) = pwr                                                 
      bkx4o(itrr) = pwr                                                 
      go to 3600                                                        
 3000 nbreak=nbreak-1                                                   
      ib3 = ibkbno(itrr)                                                
      bkv3 = buskv(ib3)                                                 
      bus3 = bname(ib3)                                                 
      write (errbuf(1),3200) bus3,bkv3,ibkid(itrr),bus1,bkv1,bus2,bkv2  
      write (errbuf(2),3400)                                            
      call prterr ('E',2)                                               
 3200 format(1h0,' BRAKE AT BUS ',a8,1x,f5.1,1x,a1,' WILL NOT ',        
     1'OPERATE BECAUSE INITIAL POWER IN LINE ',2(a8,1x,f5.1,1x))        
 3400 format(1x,'IS BELOW THE BIAS LEVEL')                              
 3600 continue                                                          
      nbrake=nbreak                                                     
      if(nbrake .gt. 0) return                                          
      write (errbuf(1),3800)                                            
      call prterr ('E',1)                                               
 3800 format(1h0,'  ALL BRAKES HAVE BEEN DISABLED DUE TO HIGH VALUES ', 
     1'OF BIAS POWER LEVEL')                                            
      return                                                            
      end                                                               
