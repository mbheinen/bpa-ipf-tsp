C    %W% %G%
      subroutine rusol                                                  
C                                                                       
C     THIS SUBROUTINE PROCESS THE UNDERFREQUENCY LINE TRIPPING      
C     RELAY LOGIC.  IT IS CALLED BY RELAY.                          
C                                                                       
      include 'tspinc/params.inc' 
      include 'tspinc/vrgov.inc' 
      include 'tspinc/prate.inc' 
      include 'tspinc/param.inc' 
      include 'tspinc/matrow.inc' 
      include 'tspinc/lnk12.inc' 
      include 'tspinc/rucom.inc' 
      include 'tspinc/kntrly.inc' 
      include 'tspinc/prt.inc' 
      include 'tspinc/busvolt.inc' 
      include 'tspinc/bname.inc' 
      include 'tspinc/buskv.inc' 
      include 'tspinc/ldropn.inc' 
      character*1 idpar                                                 
C     
C     ICD1 = 1 RELAY NOT TRIPPED                                       
C     ICD1 = 2 RELAY TRIPPED BREAKER STILL CLOSED                      
C     ICD1 = 3 RELAY TRIPPED BREAKER TRIPPED REMOTE TRIPPING STARTED   
C     ICD4 = 4 ALL TRIPPING COMPLETE                                   
C                                                                       
      do 2400 k = 1,kntru                                               
      icd1 = icd1ru(k)                                                  
C                                                                    
C     IF ICD1 = 4, ALL TRIPPING IS COMPLETE                           
C                                                                       
      if(icd1 .eq. 4) go to 2400                                        
      icd3 = icd3ru(k)                                                  
      icd4 = icd4ru(k)                                                  
C                                                                       
C     IF ICD1 = 3, RELAY BREAKER HAS TRIPPED BUT REMOTE TRIPPING      
C     IS IN PROGRESS                                                  
C                                                                       
      if(icd1 .eq. 3) go to 2120                                        
C                                                                       
C     TEST FOR FIRST TEN CYCLE BYPASS                                 
C                                                                       
      if(mrelay .eq. 1 .and. to .le. 10.)go to 2400                     
      ibus = ibusru(k)                                                  
      jbus = jbusru(k)                                                  
      idpar = iparru(k)                                                 
      name1 = bname(ibus)                                               
      name2 = bname(jbus)                                               
      bkv1 = buskv(ibus)                                                
      bkv2 = buskv(jbus)                                                
C                                                                       
C     IF ICD1 = 2 RELAY HAS TRIPPED BUT BREAKER HAS NOT TIMED OUT    
C                                                                       
      if(icd1 .eq. 2)go to 2090                                         
      ei = eyr(ibus)                                                    
      fi = eyi(ibus)                                                    
C
C     IDISW = 2 MEANS NETWORK DISCONTINUITY.  STORE NEW PAST VALUES OF  
C     VOLTAGE TO AVOID DISCONTINUITY IN FREQUENCY                      
C                                                                    
      if(idisw .eq. 2) then                                             
         eiru(k) = ei                                                   
         firu(k) = fi                                                   
         go to 2400                                                     
      endif                                                             
C
C     CALCULATE BUS FREQUENCY                                           
C
      eysq = ei * ei + fi * fi                                          
      if(eysq.eq.0.0) go to 1960                                        
      wnowru(k) = (fi * eiru(k) - ei * firu(k)) / (eysq * edt)
 1960 eiru(k) = ei                                                      
      firu(k) = fi                                                      
      if(keybrd(1).eq.0) go to 2000                                     
      write (outbuf, 1980) namec, bkv1, ftrpru(k),wnowru(k)             
 1980 format('0', 5x, ' REFERENCE AND ACTUAL BUS FREQ DEVIATIONS AT ',
     &  a8, 1x, f5.1, ' ARE ', e13.4, ' AND ', e13.4)                       
      call prtout (1)                                                   
 2000 if(wnowru(k) .lt. ftrpru(k)) go to 2020                           
      if(timeru(k) .eq. -1.)go to 2060                                  
      write (outbuf, 2001) name1, bkv1, name2, bkv2, to                 
 2001 format('0', 5x, ' UNDERFREQ. LINE TRIP RELAY BETWEEN ', a8, 1x,
     &  f5.1, ' AND ', a8, 1x, f5.1, ' HAS RESET AT ', f7.2,' CYCLES.')       
      call prtout (1)                                                   
      timeru(k) = -1.0                                                  
      go to 2060                                                        
C 
C     FREQUENCY IS LESS THAN TRIP POINT FTRIP                           
C 
 2020 if(timeru(k) .eq. -1.0)go to 2040                                 
      if(timeru(k) .le. to + 0.0001)go to 2080                          
      go to 2060                                                        
C
C     SET UNDERFREQUENCY TIMER TIMERU TO CURRENT TIME PLUS TRELY        
C
 2040 timeru(k) = trlyru(k) + to                                        
 2060 go to 2400                                                        
C 
C     UNDERFREQUENCY RELAY HAS TIMED OUT.  START BREAKER TIMER.         
C 
 2080 icd1 = 2                                                          
      icd1ru(k) = icd1                                                  
      write (outbuf, 2085) name1, bkv1, name2, bkv2, to                 
 2085 format('0', 5x, 'UNDER FREQ LINE TRIP RELAY BETWEEN ', a8,        
     1           1x, f5.1, ' AND ', a8, 1x, f5.1, ' TIMED OUT AT ',     
     2           f7.2, ' CYCLES')                                       
      call prtout (1)                                                   
      timeru(k) = trpcru(k) + to                                        
      if(trpcru(k) .eq. 0.0 .and. icd3 .eq. 1)then                      
          dnx = to                                                      
          go to 2095                                                    
      endif                                                             
      if(icd3 .eq. 1 .and. timeru(k) .lt. dnxrly)then                   
            dnxrly = timeru(k)                                          
      endif                                                             
      go to 2060                                                        
C 
C     CHECK BREAKER TIMEOUT                                             
C                                                                   
 2090 if(timeru(k) .le. to + 0.001)go to 2095                           
      if(icd3 .eq. 1 .and. timeru(k) .lt. dnxrly)then                   
         dnxrly = timeru(k)                                             
      endif                                                             
      go to 2060                                                        
C 
C     BREAKER HAS TIMED OUT                                             
C                                                                   
 2095 icd1 = 3                                                          
      icd1ru(k) = icd1                                                  
      ttimru(k) = tripru(k) + to                                        
      write (outbuf, 2100) name1, bkv1, name2, bkv2, to                 
 2100 format('0', 5x, 'UNDER FREQ LINE TRIP RELAY BETWEEN ', a8,        
     1           1x, f5.1, ' AND ', a8, 1x, f5.1, ' TRIPPED AT ',       
     2           f7.2, ' CYCLES')                                       
      call prtout (1)                                                   
C
C     IF ICD3 .NE. 1, BREAKER IS NOT ACTUALLY TRIPPED C= NOTRIP         
C
      if(icd3.ne.1) go to 2120                                          
      ircall = 5                                                        
      ibusn = ibus                                                      
      jbusn = jbus                                                      
      if(ibus .lt. jbus) go to 2105                                     
      ibusn = jbus                                                      
      jbusn = ibus                                                      
 2105 icden = -1                                                        
      gijt = gijru(k)                                                   
      bijt = bijru(k)                                                   
      giot = gioru(k)                                                   
      biot = bioru(k)                                                   
      gjot = gjoru(k)                                                   
      bjot = bjoru(k)                                                   
      itparn = 0                                                        
C                                                                       
C     CALL TRPHIS TO FORM A TABLE OF LINES THAT HAVE BEEN TRIPPED     
C                                                                       
      call trphis(itparn,ibusn,jbusn,gijt,bijt,giot,                    
     1           biot,gjot,bjot,icden,idpar,jobdo,mcode)                
 2110 if(jobdo.eq.0) go to 2120                                         
C                                                                       
C     PUT LINE ADMITTANCES IN TEMPN TABLE FOR TRANSFER TO LLDROP      
C                                                                       
      tempn(1, 9) = gijt                                                
      tempn(2, 9) = bijt                                                
      tempn(1, 10) = giot                                               
      tempn(2, 10) = biot                                               
      tempn(1, 11) = gjot                                               
      tempn(2, 11) = bjot                                               
      call lldrop                                                       
 2120 if(icd4 .eq. 0) go to 2400                                        
C                                                                       
C     ICD4 IS THE NUMBER OF REMOTE RELAYS ATTACHED TO THIS LOCAL      
C     RELAY.  TTIMR1 IS THE TRANSFER TRIP TIMER.                      
C                                                                       
      if(ttimru(k) .lt. dnxrly)dnxrly = ttimru(k)                       
      if(ttimru(k) .gt. to) go to 2400                                  
      icd1ru(k) = 4                                                     
      do 2350 itrr = 1,icd4                                             
      ind = idrrru(itrr,k)                                              
C                                                                       
C     CALL RRSOL TO PROCESS REMOTE RELAY LOGIC                        
C                                                                       
      call rrsol(ind)                                                   
 2350 continue                                                          
 2400 continue                                                          
      return                                                            
      end                                                               
