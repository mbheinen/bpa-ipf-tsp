C    %W% %G%
      subroutine rgsol                                                  
C                                                                       
C     THIS SUBBROUTINE PROCESS THE SERIES CAPACITOR GAP SWITCHING   
C     LOGIC.  IT IS CALLED BY RELAY.                                
C                                                                       
      include 'tspinc/params.inc' 
      include 'tspinc/param.inc' 
      include 'tspinc/matrow.inc' 
      include 'tspinc/lnk12.inc' 
      include 'tspinc/rgcom.inc' 
      include 'tspinc/kntrly.inc' 
      include 'tspinc/prt.inc' 
      include 'tspinc/busvolt.inc' 
      include 'tspinc/prate.inc' 
      include 'tspinc/bname.inc' 
      include 'tspinc/buskv.inc' 

      character*1 idpar                                                 
      character*8 name1,name2                                           

      if(idisw .eq. 2)return                                            
      do 1000 k = 1,kntrg                                               
C                                                                       
C     IF ICD1RG = 3 THIS CAPACITOR IS PERMANENTLY SHORT CIRCUITED     
C     AND TRANSFER TRIP TO THE REMOTE RELAYS HAS BEEN COMPLETED.      
C                                                                       
      if(icd1rg(k) .eq. 3)go to 1000                                    
C                                                                       
C     IF ICD1RG = 2 THIS CAPACITOR IS PERMANENTLY SHORT CIRCUITED     
C     AND TRANSFER TRIP TO THE REMOTE RELAYS HAS NOT TIMED OUT        
C                                                                       
      if(icd1rg(k) .eq. 2) go to 920                                    
      ibus = ibusrg(k)                                                  
      jbus = jbusrg(k)                                                  
      idpar = iparrg(k)                                                 
      name1 = bname(ibus)                                               
      name2 = bname(jbus)                                               
      bkv1 = buskv(ibus)                                                
      bkv2 = buskv(jbus)                                                
      ei = eyr(ibus)                                                    
      fi = eyi(ibus)                                                    
      ej = eyr(jbus)                                                    
      fj = eyi(jbus)                                                    
C                                                                       
C     GET PI EQUIVALENT ADMITTANCES FOR TOTAL LINE                    
C                                                                       
      gija = gijarg(k)                                                  
      bija = bijarg(k)                                                  
      gioa = gioarg(k)                                                  
      bioa = bioarg(k)                                                  
      gjoa = gjoarg(k)                                                  
      bjoa = bjoarg(k)                                                  
C                                                                       
C     GET PI EQUIVALENT ADMITTANCES FOR LINE SECTION TO THE           
C     LEFT OF SERIES CAPACITOR                                        
C                                                                       
      gijp = gijprg(k)                                                  
      bijp = bijprg(k)                                                  
      giop = gioprg(k)                                                  
      biop = bioprg(k)                                                  
      gjop = gjoprg(k)                                                  
      bjop = bjoprg(k)                                                  
      c1r=ei*(gioa+gija+giop)-fi*(bioa+bija+biop)                       
      c1i=ei*(bioa+bija+biop)+fi*(gioa+gija+giop)                       
      c2r=c1r-ej*gija+fj*bija                                           
      c2i=c1i-ej*bija-fj*gija                                           
      if ((gijp.ne.0.0).or.(bijp.ne.0.0)) go to 480                     
      capr=c2r                                                          
      capi=c2i                                                          
      go to 500                                                         
C                                                                       
C     CALCULATE CURRENT THRU CAPACITOR--GAPCUR                        
C                                                                       
  480 rijp=gijp/(gijp*gijp+bijp*bijp)                                   
      xijp=-bijp/(gijp*gijp+bijp*bijp)                                  
      cnstr=rijp*gjop-xijp*bjop+1.                                      
      cnsti=rijp*bjop+xijp*gjop                                         
      c3r=c2r*cnstr-c2i*cnsti                                           
      c3i=c2r*cnsti+c2i*cnstr                                           
      capr=c3r-ei*gjop+fi*bjop                                          
      capi=c3i-ei*bjop-fi*gjop                                          
  500 gapcur=sqrt(capr*capr+capi*capi)                                  
      write (outbuf, 520) gapcur,to,icd1rg(k)                       
  520 format('0', 5x, 'GAP CURRENT =', f8.4, ' AT', f7.2,           
     1           ' CYCLES ICD1=', i3)                                    
      call prtout (1)                                               
      gapfc = gpfcrg(k)                                                 
      reinc = rincrg(k)                                                 
      if (gapcur .le. gapfc)go to 700                                   
C                                                                       
C     IF ICD1RG NOT EQUAL 1 THE SERIES CAPACITOR GAP HAS  FIRED       
C     PRIOR TO THIS                                                   
C                                                                       
      if(icd1rg(k) .ne. 1) then                                       
C                                                                       
C       ICD1RG = 4 MEANS THE LINE CURRENT EXCEEDS THE FIRING CURRENT    
C       AND THE GAP IS ALREADY FIRED, SO  CHECK                         
C       THE PERMANENT SHORT CIRCUIT TIMER                               
C                                                                       
        icd1rg(k) = 4                                                  
        if(timprg(k) - .0001 .gt. to) go to 580                        
C                                                                       
C       GAP IS PERMANENTLY SHORT CIRCUITED IF KBORRG =3,               
C       REMOTE TRANSFER TRIP IS NOT COMPLETE.  IF KBORRG = 4           
C       RETMOTE TRANSER TRIP IS COMPLETE.                              
C                                                                       
        if(kborrg(k) .eq. 3) icd1rg(k) = 2                             
        if(kborrg(k) .eq. 4) icd1rg(k) = 3                             
        write (outbuf, 560) name1, bkv1, name2, bkv2,idpar, to         
  560   format('0', 5x, 'SERIES CAPACITOR BETWEEN ', a8, 1x, f5.1,     
     1          ' AND ', a8, 1x, f5.1,1x,a1,                            
     2          ' PERMANENTLY SHORT CIRCUITED AT ', f7.2, ' CYCLES ')   
        call prtout (1)                                                
C                                                                       
C       IF KBORRG = 3 REMOTE RELAY ACTIVATED                            
C                                                                       
  580   if (kborrg(k) .eq. 3) go to 920                                
        go to 1000                                                     
      endif                                                             
C                                                                       
C     IF ICD1RG EQUAL 1 THE SERIES CAPACITOR GAP HAS NOT FIRED        
C     PRIOR TO THIS SO SHORT CIRCUIT THE GAP NOW                      
C                                                                       
      icd1rg(k) = 4                                                     
      timprg(k) = (permrg(k) + to)                                      
C                                                                       
C     GET PI EQUIVALENT ADMITTANCE FOR LINE WITH CAPACITOR            
C     SHORT CIRCUITED AND MODIFY ADMITTANCE MATRIX  TO                
C     TO REFLECT NEW LINE ADMITTANCE                                  
C                                                                       
      gijas = gijsrg(k)                                                 
      bijas = bijsrg(k)                                                 
      gioas = giosrg(k)                                                 
      bioas = biosrg(k)                                                 
      gjoas = gjosrg(k)                                                 
      bjoas = bjosrg(k)                                                 
      ircall = 2                                                        
      ibusn = ibus                                                      
      jbusn = jbus                                                      
      if(ibus .lt. jbus) go to 592                                      
      ibusn = jbus                                                      
      jbusn = ibus                                                      
  592 icden = 1                                                         
      gijt = gijsrg(k)                                                  
      bijt = bijsrg(k)                                                  
      giot = giosrg(k)                                                  
      biot = biosrg(k)                                                  
      gjot = gjosrg(k)                                                  
      bjot = bjosrg(k)                                                  
      itparn = 0                                                        
C                                                                       
C     CALL TRPHIS TO FORM A TABLE OF LINES THAT HAVE BEEN TRIPPED     
C     OR MODIFIED.                                                    
C                                                                       
      call trphis(itparn,ibusn,jbusn,gijt,bijt,giot,                    
     1           biot,gjot,bjot,icden,idpar,jobdo,mcode)                
      idum=0                                                            
  600 continue                                                          
      call getmat(ibus, ii)                                                 
      if (keybrd(30) .ne. 0) then                                       
         write (outbuf, 602) jbus, atrow(i + 1),                        
     1               atrow(i + 2), ibus, atrow(ii - 1), atrow(ii)       
  602    format('0 RGSOL, S600+4, B4 ', 2(i10, e16.6, e16.6))              
         call prtout (1)                                                
      endif                                                             
      atrow(ii-1)=atrow(ii-1)-gija-gioa+gijas+gioas                     
      atrow(ii)=atrow(ii)-bija-bioa+bijas+bioas                         
      do 620 i=4,ii,3                                                   
      if (jbus.eq.matrow(i)) go to 640                                  
  620 continue                                                          
      go to 1440                                                        
  640 atrow(i+1)=atrow(i+1)+gija-gijas                                  
      atrow(i+2)=atrow(i+2)+bija-bijas                                  
      call putmat(ibus, ii)                                                 
      if (keybrd(30) .ne. 0) then                                       
         write (outbuf, 642) jbus, atrow(i + 1),                        
     1               atrow(i + 2), ibus, atrow(ii - 1), atrow(ii)       
  642    format('0 RGSOL, S640+4, AFT ', 2(i10, e16.6, e16.6))             
         call prtout (1)                                                
      endif                                                             
      ivpc=2                                                            
      if (ibus.lt.lfrst) lfrst=ibus                                     
      if (idum.eq.jbus) go to 680                                       
      write (outbuf, 660) name1, bkv1, name2, bkv2,idpar, to            
  660 format('0', 5x, 'SERIES CAPACITOR BETWEEN ', a8, 1x, f5.1,        
     1           ' AND ', a8, 1x, f5.1, 1x, a1, ' SHORT CIRCUITED AT ',   
     2           f7.2,' CYCLES')                                        
      call prtout (1)                                                   
      idum=ibus                                                         
      ibus=jbus                                                         
      jbus=idum                                                         
      gioa=gjoa                                                         
      bioa=bjoa                                                         
      gioas=gjoas                                                       
      bioas=bjoas                                                       
      go to 600                                                         
C                                                                       
C     SWITCH ADMITTANCES TABLES SO GIJARG CONTAINS SHORT CIRCUITED DAT
C     AND GIJSRG CONTAINS ORGINAL LINE DATA                           
C                                                                       
  680 t51n = gijarg(k)                                                  
      t52n = bijarg(k)                                                  
      t61n = gioarg(k)                                                  
      t62n = bioarg(k)                                                  
      t71n = gjoarg(k)                                                  
      t72n = bjoarg(k)                                                  
      gijarg(k) = gijsrg(k)                                             
      bijarg(k) = bijsrg(k)                                             
      gioarg(k) = giosrg(k)                                             
      bioarg(k) = biosrg(k)                                             
      gjoarg(k) = gjosrg(k)                                             
      bjoarg(k) = bjosrg(k)                                             
      gijsrg(k) = t51n                                                  
      bijsrg(k) = t52n                                                  
      giosrg(k) = t61n                                                  
      biosrg(k) = t62n                                                  
      gjosrg(k) = t71n                                                  
      bjosrg(k) = t72n                                                  
C                                                                       
C     IF TRANSFER TRIP TO REMOTE RELAYS STARTS THEN                   
C     FIRST TIME THE GAP IS FIRED, CHECK TO SEE                       
C     IF REMOTE TIMER HAS STARTED                                     
C                                                                       
      if(kborrg(k) .eq. 1)then                                          
         timtrg(k) = to + ttrprg(k)                                     
         kborrg(k) = 3                                                  
      endif                                                             
C                                                                       
C     CHECK TO SEE IF NUMBER OF REINSERTIONS EQUALS MAX,              
C     IF SO, PERMANENTLY SHORT CIRCUIT CAPACITOR                      
C                                                                       
      if (icd3rg(k) .ne. nrecrg(k)) then                                
         if(kborrg(k) .eq. 3)go to 920                                  
         go to 1000                                                     
      endif                                                             
C                                                                       
C     IDC1RG = 2 MEANS THE GAP IS PERMANENTLY SHORT CIRCUITED         
C     AS THE NUMBER OF REINSETION ATTEMPS HAS REACHED                 
C     THE MAXIMUM OF NRECRG.                                          
C                                                                       
      icd1rg(k) = 2                                                     
      write (outbuf, 560) name1,bkv1,name2,bkv2,idpar,to                
      call prtout (1)                                                   
C                                                                       
C     KBORRG = 2 MEANS THE REMOTE RELAY TRANSFER TRIP STARTS          
C     WHEN ALL REINSERTIONS ARE COMPLETE SO START REMOTE TIMER        
C                                                                       
      if (kborrg(k) .eq. 2) then                                        
         kborrg(k) = 3                                                  
         timtrg(k) = to +ttrprg(k)                                      
         go to 920                                                      
      endif                                                             
C                                                                       
C     KBORRG = 4 MEANS THE GAP IS PERMANENTLY SHORT CIRCUITED AND     
C     TRANSFER TRIP DELAY HAS BEEN COMPLETED                          
C                                                                       
      if(kborrg(k) .eq. 4)icd1rg(k) = 3                                 
      go to 1000                                                        
                                                                        
C                                                                       
C     THIS BLOCK OF LOGIC IS FOR THE CONDITON THAT THE CAPACITOR      
C     CURRENT IS LESS THAN THE FIRING CURRENT                         
C                                                                       
  700 if (icd1rg(k) .eq. 1)then                                         
         if(kborrg(k) .eq. 3) go to 920                                 
         go to 1000                                                     
      endif                                                             
C                                                                       
C     SET PERMANENT SHORT CIRCUIT TIMER TO ZERO                       
C                                                                       
      timprg(k) = 0.0                                                   
      if(gapcur .gt. rincrg(k)) then                                    
         if(kborrg(k) .eq. 3) go to 920                                 
         go to 1000                                                     
      endif                                                             
C                                                                       
C     ICD1RG = 6 MEANS THE LINE CURRENT IS LESS THAN THE REINSETION CU
C     AND THE REINSERTTION TIMER IT2 HAS STARTED.                     
C                                                                       
      if (icd1rg(k) .ne. 6) then                                        
         icd1rg(k) = 6                                                  
         timrrg(k) = to + retdrg(k)                                     
      endif                                                             
      if(timrrg(k) .gt. to)go to 900                                    
C                                                                       
C     INCREMENT THE NUMBER OF REINSERTION ATTEMPTS-ICD3RG             
C                                                                       
      icd3rg(k) = icd3rg(k) +1                                          
C                                                                       
C     RESET ICD1RG = 1 WHICH MEANS GAP IS NOT FIRED                   
C                                                                       
      icd1rg(k) = 1                                                     
C                                                                       
C     OBTAIN EQUIVALENT SHORT CIRCUITED ADMITTANCE AND REINSET        
C     THE CAPACITOR IN THE LINE                                       
C                                                                       
      gijas = gijsrg(k)                                                 
      bijas = bijsrg(k)                                                 
      gioas = giosrg(k)                                                 
      bioas = biosrg(k)                                                 
      gjoas = gjosrg(k)                                                 
      bjoas = bjosrg(k)                                                 
      ircall = 2                                                        
      ibusn = ibus                                                      
      jbusn = jbus                                                      
      if(ibus .lt. jbus) go to 792                                      
      ibusn = jbus                                                      
      jbusn = ibus                                                      
  792 icden = 1                                                         
      gijt = gijsrg(k)                                                  
      bijt = bijsrg(k)                                                  
      giot = giosrg(k)                                                  
      biot = biosrg(k)                                                  
      gjot = gjosrg(k)                                                  
      bjot = bjosrg(k)                                                  
      itparn = 0                                                        
C                                                                       
C     CALL TRPHIS TO FORM A TABLE OF LINES THAT HAVE BEEN TRIPPED OR  
C     MODIFIED                                                        
C                                                                       
      call trphis(itparn,ibusn,jbusn,gijt,bijt,giot,                    
     1           biot,gjot,bjot,icden,idpar,jobdo,mcode)                
      idum=0                                                            
  800 continue                                                          
      call getmat(ibus, ii)                                                 
      if (keybrd(30) .ne. 0) then                                       
         write (outbuf, 802) jbus, atrow(i + 1),                        
     1               atrow(i + 2), ibus, atrow(ii - 1), atrow(ii)       
  802    format('0 RGSOL, S800+4, B4 ', 2(i10, e16.6, e16.6))              
         call prtout (1)                                                
      endif                                                             
      atrow(ii-1)=atrow(ii-1)+gijas+gioas-gija-gioa                     
      atrow(ii)=atrow(ii)+bijas+bioas-bija-bioa                         
      do 820 i=4,ii,3                                                   
      if (jbus.eq.matrow(i)) go to 840                                  
  820 continue                                                          
      go to 1440                                                        
  840 atrow(i+1)=atrow(i+1)+gija-gijas                                  
      atrow(i+2)=atrow(i+2)+bija-bijas                                  
      call putmat(ibus, ii)                                                 
      if (keybrd(30) .ne. 0) then                                       
         write (outbuf, 842) jbus, atrow(i + 1),                        
     1               atrow(i + 2), ibus, atrow(ii - 1), atrow(ii)       
  842    format('0 RGSOL, S840+4, AFT ', 2(i10, e16.6, e16.6))             
         call prtout (1)                                                
      endif                                                             
      ivpc=2                                                            
      if (ibus.lt.lfrst) lfrst=ibus                                     
      if (idum.eq.jbus) go to 880                                       
      write (outbuf, 860) name1, bkv1, name2, bkv2,idpar, to            
  860 format('0', 5x, ' SERIES CAPACITOR BETWEEN ', a8, 1x, f5.1,       
     1           ' AND ', a8, 1x, f5.1, 1x, a1, ' REINSERTED AT ',        
     2           f7.2,' CYCLES ')                                       
      call prtout (1)                                                   
      idum=ibus                                                         
      ibus=jbus                                                         
      jbus=idum                                                         
      gioa=gjoa                                                         
      bioa=bjoa                                                         
      gioas=gjoas                                                       
      bioas=bjoas                                                       
      go to 800                                                         
  880 t51n = gijarg(k)                                                  
      t52n = bijarg(k)                                                  
      t61n = gioarg(k)                                                  
      t62n = bioarg(k)                                                  
      t71n = gjoarg(k)                                                  
      t72n = bjoarg(k)                                                  
      gijarg(k) = gijsrg(k)                                             
      bijarg(k) = bijsrg(k)                                             
      gioarg(k) = giosrg(k)                                             
      bioarg(k) = biosrg(k)                                             
      gjoarg(k) = gjosrg(k)                                             
      bjoarg(k) = bjosrg(k)                                             
      gijsrg(k) = t51n                                                  
      bijsrg(k) = t52n                                                  
      giosrg(k) = t61n                                                  
      biosrg(k) = t62n                                                  
      gjosrg(k) = t71n                                                  
      bjosrg(k) = t72n                                                  
  900 if(kborrg(k) .eq. 3) go to 920                                    
      go to 1000                                                        
C                                                                       
C     IF KBORRG = 3, REMOTE RELAY TRANSFER TRIP TIMER HAS             
C     STARTED                                                         
C                                                                       
  920 if (timtrg(k) .le. to) then                                       
         kborrg(k) = 4                                                  
         write(outbuf,930)                                              
  930    format(' CAPACITOR GAP LOGIC READY TO TRIP REMOTE RELAY')      
         call prtout(1)                                                 
         if(icd1rg(k) .eq. 2)icd1rg(k) = 3                              
         if(icd4rg(k) .eq. 0)go to 1000                                 
         do 940 itrr = 1,icd4rg(k)                                      
         ind = idrrrg(itrr,k)                                           
         call rrsol(ind)                                                
  940    continue                                                       
      endif                                                             
 1000 continue                                                          
      return                                                            
 1440 write (errbuf(1), 1460) jbus                                      
 1460 format('0', 5x, 'ERROR IN AUTOMATIC SWITCHING LOGIC--UNABLE       
     1           to find bus number', i5, ' PROGRAM STOPPED.')          
      call prterr ('E',1)                                               
      call mpost('RGSOL')                                               
      call erexit                                                       
      return                                                            
      end                                                               
