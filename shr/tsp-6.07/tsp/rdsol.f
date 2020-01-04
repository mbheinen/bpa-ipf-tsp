C    %W% %G%
      subroutine rdsol                                                  
C                                                                       
C     THIS SUBROUTINE PROCESS THE DISTANCE RELAY LOGIC.             
C     IT IS CALLED BY RELAY.                                        
C                                                                       
      include 'tspinc/params.inc' 
      include 'tspinc/vrgov.inc' 
      include 'tspinc/prate.inc' 
      include 'tspinc/param.inc' 
      include 'tspinc/matrow.inc' 
      include 'tspinc/lnk12.inc' 
      include 'tspinc/rdcom.inc' 
      include 'tspinc/kntrly.inc' 
      include 'tspinc/prt.inc' 
      include 'tspinc/busvolt.inc' 
      include 'tspinc/bname.inc' 
      include 'tspinc/buskv.inc' 

      character*1 idpar                                                 
      character*8 name1,name2                                           

      if(idisw .eq. 2)return                                            

      do 2400 k = 1,kntrd                                               
      icd1 = icd1rd(k)                                                  
      icd2 = icd2rd(k)                                                  
      icd3 = icd3rd(k)                                                  
      icd4 = icd4rd(k)                                                  
C                                                                       
C     IF ICD1 = 3, BOTH LOCAL AND REMOTE TRIPPING HAS BEEN COMPLETED  
C                                                                       
      if(icd1 .eq. 3) go to 2400                                        
C                                                                       
C     IF ICD1 = 2 LOCAL TRIPPING IS COMPLETE BUT TRANSFER            
C     IS STILL IN PROGRESS                                           
C                                                                       
      if(icd1 .eq. 2)go to 2300                                         
C                                                                       
C     TEST FOR FIRST TEN CYCLE BYPASS                                 
C                                                                       
      if(mrelay .eq. 1 .and. to .le. 10.)go to 2400                     
C                                                                       
C     DISTANCE RELAYS                                                
C                                                                       
      ibus = ibusrd(k)                                                  
      jbus = jbusrd(k)                                                  
      idpar = iparrd(k)                                                 
      name1 = bname(ibus)                                               
      name2 = bname(jbus)                                               
      bkv1 = buskv(ibus)                                                
      bkv2 = buskv(jbus)                                                
      ttrip = ttrprd(k)                                                 
      btrip = btrprd(k)                                                 
      gij = gijrd(k)                                                    
      bij = bijrd(k)                                                    
      gio = giord(k)                                                    
      bio = biord(k)                                                    
      gjo = gjord(k)                                                    
      bjo = bjord(k)                                                    
      ei = eyr(ibus)                                                    
      fi = eyi(ibus)                                                    
      ej = eyr(jbus)                                                    
      fj = eyi(jbus)                                                    
      gt=gij+gio                                                        
      bt=bij+bio                                                        
      c1r=ei*gt-fi*bt                                                   
      c1i=ei*bt+fi*gt                                                   
      c2r=ej*gij-fj*bij                                                 
      c2i=ej*bij+fj*gij                                                 
      ctr=c1r-c2r                                                       
      cti=c1i-c2i                                                       
      if( abs(ctr)+ abs(cti).eq. 0.0) ctr=0.0001                        
      zr=(ei*ctr+fi*cti)/(ctr*ctr+cti*cti)                              
      zi=(fi*ctr-ei*cti)/(ctr*ctr+cti*cti)                              
      brad2 = brd2rd(k)                                                 
      bzdly = bzdyrd(k)                                                 
C                                                                       
C     BYPASS ZONE 2 LOGIC IF NO DATA (I.E., IF BRAD2 = 0.).           
C                                                                       
      if(brad2.eq.0.) go to 1350                                        
      brc = brcrd(k)                                                    
      bxc = bxcrd(k)                                                    
      zbapp2=(zr-brc)*(zr-brc)+(zi-bxc)*(zi-bxc)                        
      if (zbapp2.le.brad2) go to 1100                                   
      if(bzdly.lt.0.) go to 1350                                        
      write (outbuf, 1000) name1, bkv1, name2, bkv2, to                 
 1000 format('0', 5x, ' ZONE 2 RESET FOR DISTANCE RELAY BETWEEN ', a8,
     &  1x, f5.1, ' AND ', a8, 1x, f5.1, ' AT ', f7.2,' CYCLES.')
      call prtout (1)                                                   
      bzdly=-1.0                                                        
      brd2rd(k) = brad2                                                 
      bzdyrd(k) = bzdly                                                 
      go to 1350                                                        
 1100 if (bzdly .eq. -1.0) go to 1300                                   
      brd2rd(k) = brad2                                                 
      bzdyrd(k) = bzdly                                                 
      if(bzdly.gt.to+0.0001) go to 1250                                 
 1150 write (outbuf, 1200) name1, bkv1, name2, bkv2                     
 1200 format('0', 5x, ' ZONE 2 TRIP FOR DISTANCE RELAY BETWEEN ', a8,
     &  1x, f5.1, ' AND ', a8, 1x, f5.1)                                        
      call prtout (1)                                                   
      go to 1750                                                        
 1250 continue                                                          
      if ((icd3.eq.1).and.(bzdly.lt.dnxrly)) dnxrly=bzdly               
      go to 1350                                                        
 1300 bzdly=btrip+to                                                    
      if(btrip.eq.0.0) go to 1150                                       
      if ((icd3.eq.1).and.(bzdly.lt.dnxrly)) dnxrly=bzdly               
      brd2rd(k) = brad2                                                 
      bzdyrd(k) = bzdly                                                 
 1350 trad2 = trd2rd(k)                                                 
      tzdly = tzdyrd(k)                                                 
C                                                                       
C     BYPASS ZONE 1 LOGIC IF NO DATA (I.E., IF TRAD2 = 0.).           
C                                                                       
      if(trad2.eq.0.) go to 1700                                        
      trc = trcrd(k)                                                    
      txc = txcrd(k)                                                    
      ztapp2=(zr-trc)*(zr-trc)+(zi-txc)*(zi-txc)                        
      if (ztapp2.le.trad2) go to 1450                                   
      if(tzdly.lt.0.) go to 1700                                        
      write (outbuf, 1400) name1, bkv1, name2, bkv2, to                 
 1400 format('0', 5x, ' ZONE 1 RESET FOR DISTANCE RELAY BETWEEN ', a8,
     &  1x, f5.1, ' AND ', a8, 1x, f5.1, ' AT ', f7.2, ' CYCLES.')
      call prtout (1)                                                   
      tzdly=-1.0                                                        
      go to 1700                                                        
 1450 if (tzdly .eq. -1.0) go to 1650                                   
      if(tzdly.gt.to+0.0001) go to 1600                                 
 1500 write (outbuf, 1550) name1, bkv1, name2, bkv2                     
 1550 format('0', 5x, 'ZONE 1 TRIP FOR DISTANCE RELAY BETWEEN ', a8,
     &  1x, f5.1, ' AND ', a8, 1x, f5.1)                                        
      call prtout (1)                                                   
      go to 1750                                                        
 1600 continue                                                          
      if ((icd3.eq.1).and.(tzdly.lt.dnxrly)) dnxrly=tzdly               
      go to 1700                                                        
 1650 tzdly=ttrip+to                                                    
      if(ttrip.eq.0.0) go to 1500                                       
      if ((icd3.eq.1).and.(tzdly.lt.dnxrly)) dnxrly=tzdly               
 1700 trd2rd(k) = trad2                                                 
      tzdyrd(k) = tzdly                                                 
      go to 2400                                                        
 1750 icd1=2                                                            
      icd1rd(k) = icd1                                                  
      ttimrd(k) = to + triprd(k)                                        
      idum=0                                                            
      write (outbuf, 1800) name1, bkv1, name2, bkv2, to                 
 1800 format('0', 5x, 'DISTANCE RELAY BETWEEN ', a8, 1x, f5.1,          
     1          ' AND ', a8, 11x, f5.1, ' TRIPPED AT ', f7.2,           
     2           ' CYCLES ')                                            
      call prtout (1)                                                   
      if(icd3 .eq. 2) go to 2300                                        
      ircall = 1                                                        
      ibusn = ibus                                                      
      jbusn = jbus                                                      
      if(ibus .lt. jbus) go to 1850                                     
      ibusn = jbus                                                      
      jbusn = ibus                                                      
 1850 icden = -1                                                        
      itparn = 0                                                        
C                                                                       
C     CALL TRPHIS TO FORM A TABLE OF LINES THAT HAVE BEEN TRIPPED     
C                                                                       
      call trphis(itparn,ibusn,jbusn,gij,bij,gio,                       
     1           bio,gjo,bjo,icden,idpar,jobdo,mcode)                   
      if(jobdo.eq.0) go to 2300                                         
 1900 continue                                                          
      call getmat(ibus, ii)                                                 
      do 1950 i=4,ii,3                                                  
      if (jbus.eq.matrow(i)) go to 2000                                 
 1950 continue                                                          
      go to 2500                                                        
 2000 if(abs(atrow(i+1)).lt.0.0001.and.abs(atrow(i+2)).lt.0.0001)       
     1   go to 2100                                                     
      if (keybrd(30) .ne. 0) then                                       
          write (outbuf, 2050) jbus, atrow(i + 1),                      
     1               atrow(i + 2), ibus, atrow(ii - 1), atrow(ii)       
 2050     format('0 RDSOL, S1900+4, B4 ', 2(i10, e16.6, e16.6))             
          call prtout (1)                                               
      endif                                                             
      atrow(i+1) = atrow(i+1) + gij                                     
      atrow(i+2) = atrow(i+2) + bij                                     
      if(abs(atrow(i+1)) .le. 0.0001) atrow(i+1) = 0.0                  
      if(abs(atrow(i+2)) .le. 0.0001) atrow(i+2) = 0.0                  
      atrow(ii-1)=atrow(ii-1)-gij-gio                                   
      atrow(ii)=atrow(ii)-bij-bio                                       
 2100 call putmat(ibus, ii)                                                 
      if (keybrd(30) .ne. 0) then                                       
         write (outbuf, 2150) jbus, atrow(i + 1),                       
     1               atrow(i + 2), ibus, atrow(ii - 1), atrow(ii)       
 2150    format('0 RDSOL, S2100+2, AFT ', 2(i10, e16.6, e16.6))            
         call prtout (1)                                                
      endif                                                             
      if (ibus.lt.lfrst) lfrst=ibus                                     
      if (idum.eq.jbus) go to 2250                                      
      write (outbuf, 2200) name1, bkv1, name2, bkv2, to                 
 2200 format('0', 5x, 'DISTANCE RELAY BETWEEN ', a8, 1x, f5.1,          
     1           ' AND ', a8, 1x, f5.1, ' TRIPPED AND LINE OPENED AT ', 
     2           f7.2, ' CYCLES')                                       
      call prtout (1)                                                   
      idum=ibus                                                         
      ibus=jbus                                                         
      jbus=idum                                                         
      gio=gjo                                                           
      bio=bjo                                                           
      go to 1900                                                        
 2250 ivpc=2                                                            
      go to 2300                                                        
C                                                                       
C     ICD4 IS THE NUMBER OF REMOTE RELAYS ATTACHED TO THIS LOCAL      
C     RELAY.  TTIMR1 IS THE TRANSFER TRIP TIMER.                      
C                                                                       
 2300 if(icd4.eq.0) go to 2400                                          
      if(ttimrd(k) .lt. dnxrly)dnxrly = ttimrd(k)                       
      if(ttimrd(k) .gt. to) go to 2400                                  
      icd1rd(k) = 3                                                     
      do 2350 itrr = 1,icd4                                             
      ind = idrrrd(itrr,k)                                              
C                                                                       
C     CALL RRSOL TO PROCESS REMOTE RELAY LOGIC                        
C                                                                       
      call rrsol(ind)                                                   
 2350 continue                                                          
 2400 continue                                                          
      return                                                            
 2500 write (errbuf(1), 2600) jbus                                      
 2600 format('0', 5x, 'ERROR IN AUTOMATIC SWITCHING LOGIC--UNABLE       
     1           to find bus number', i5, ' PROGRAM STOPPED.')          
      call prterr ('E',1)                                               
      call mpost('RELAY')                                               
      call erexit                                                       
      end                                                               
