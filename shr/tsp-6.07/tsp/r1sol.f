C    %W% %G%
      subroutine r1sol                                                  
C                                                                       
C     THIS SUBBROUTINE PROCESS THE RATE OF CHANGE OF POWER RELAY    
C     LOGIC(R1,R2, AND R3 CARDS).  IT IS CALLED BY RELAY.           
C                                                                       
      include 'tspinc/params.inc' 
      include 'tspinc/vrgov.inc' 
      include 'tspinc/prate.inc' 
      include 'tspinc/param.inc' 
      include 'tspinc/matrow.inc' 
      include 'tspinc/lnk12.inc' 
      include 'tspinc/r1com.inc' 
      include 'tspinc/kntrly.inc' 
      include 'tspinc/prt.inc' 
      include 'tspinc/busvolt.inc' 
      include 'tspinc/bname.inc' 
      include 'tspinc/buskv.inc' 

      character*1 idpar                                                 
      character*8 name1,name2                                           

      do 2400 k = 1,kntr1                                               
      irmkt = 0                                                         
      icd1 = icd1r1(k)                                                  
      icd2 = icd2r1(k)                                                  
      icd3 = icd3r1(k)                                                  
      icd4 = icd4r1(k)                                                  
C                                                                       
C     IF ICD1 = 3, BOTH LOCAL AND REMOTE TRIPPING HAS BEEN COMPLETED  
C                                                                       
      if(icd1 .eq. 3) go to 2400                                        
C                                                                       
C     IF ICD1 = 2 LOCAL TRIPPING IS COMPLETE BUT TRANSFER            
C     IS STILL IN PROGRESS                                           
C                                                                       
      if(icd1 .eq. 2)go to 2250                                         
C                                                                       
C     TEST FOR FIRST TEN CYCLE BYPASS                                 
C                                                                       
      if(mrelay .eq. 1 .and. to .le. 10.)go to 2400                     
      if(idisw .eq. 2) go to 2400                                       
      ibus = ibusr1(k)                                                  
      jbus = jbusr1(k)                                                  
      idpar = iparr1(k)                                                 
      name1  = bname(ibus)                                              
      name2  = bname(jbus)                                              
      bkv1 = buskv(ibus)                                                
      bkv2 = buskv(jbus)                                                
      ei = eyr(ibus)                                                    
      fi = eyi(ibus)                                                    
      ej = eyr(jbus)                                                    
      fj = eyi(jbus)                                                    
      gjo = gjor1(k)                                                    
      bjo = bjor1(k)                                                    
      gio = gior1(k)                                                    
      bio = bior1(k)                                                    
      gij = gijr1(k)                                                    
      bij = bijr1(k)                                                    
      gt = gio + gij                                                    
      bt = bio+bij                                                      
C                                                                       
C     CALCULATE POWER FLOW ON LINE CONTAINING RELAY                   
C                                                                       
      c1r = ei*gt - fi*bt                                               
      c1i = ei*bt + fi*gt                                               
      c2r = ej*gij - fj*bij                                             
      c2i = ej*bij + fj*gij                                             
      ctr = c1r-c2r                                                     
      cti = c1i - c2i                                                   
      pi = ei*ctr + fi*cti                                              
      piiold = piir1(k)                                                 
      p3iold = piiir1(k)                                                
      if(idisw .ne. 1)then                                              
         piold = pi                                                     
         ps = psr1(k)                                                   
         piodr1(k) = piold                                              
         go to 2400                                                     
      endif                                                             
      ta = tar1(k)                                                      
      tb = tbr1(k)                                                      
      ps = psr1(k)                                                      
      piold = piodr1(k)                                                 
C                                                                       
C     PII IS THE OUTPUT OF THE A BLOCK                                
C     PIII IS THE OUTPUT OF THE B BLOCK                               
C                                                                       
      pii = (piiold*(2.*ta/edt-1.) + piold + pi)/(2.*ta/edt + 1.)       
      piii = (p3iold*(2.*tb/edt -1.) + piiold + pii)/(2. * tb/edt +1.)  
      pdot = (piii - p3iold)/edt                                        
      piold = pi                                                        
      piiold = pii                                                      
      p3iold = piii                                                     
      piir1(k) = piiold                                                 
      piiir1(k) = p3iold                                                
      psr1(k) = ps                                                      
      piodr1(k) = piold                                                 
C                                                                       
C     ICD2 = 5 FOR THE R1 MODEL                                       
C     ICD2 = 6 FOR THE R2 MODEL                                       
C     ICD2 = 7 FOR THE R3 MODEL                                       
C     THIS LOGIC IS FOR THE R3 MODEL                                  
C                                                                       
      if(icd2 .lt. 7)go to 675                                          
      pi1 = piii/ps                                                     
      if(keybrd(1) .ne. 0) then                                         
         write (outbuf, 400) piii,pi1,to                                
  400    format('0', 5x, ' PIII, PI1, TO', f6.4, f8.5, f6.1)            
         call prtout (1)                                                
      endif                                                             
      in1 = 0                                                           
      t1 = t1r1(k)                                                      
      t2 = t2r1(k)                                                      
      te = ter1(k)                                                      
      th = thr1(k)                                                      
      if(pi1 .ge. 1.) t2 = to                                           
      if((t2 + te) .ge. to) in1 = 1                                     
      rmax = rmaxr1(k)                                                  
      rmin = rminr1(k)                                                  
      if(pdot .gt. rmax) pdot = rmax                                    
      if(pdot .lt. rmin) pdot = rmin                                    
      rs = rsr1(k)                                                      
      tc = tcr1(k)                                                      
      riiold = riir1(k)                                                 
      riold = rir1(k)                                                   
      ri = pdot/rs                                                      
C                                                                       
C     RII IS THE OUTPUT OF THE 'C' BLOCK                              
C                                                                       
      rii = (riiold*(2.*tc/edt -1.) +riold + ri)/(2.*tc/edt + 1.)       
      riold = ri                                                        
      riiold = rii                                                      
      riir1(k) = riiold                                                 
      rir1(k) = riold                                                   
      if(keybrd(1) .ne. 0) then                                         
          write (outbuf, 500)                                           
  500     format('0', 5x, 'RI,RII,PDOT')                                
          call prtout (1)                                               
          write (outbuf,  550) ri,rii,pdot                              
  550     format( 5x, 3f7.3)                                            
          call prtout (1)                                               
      endif                                                             
      in2 = 0                                                           
      if(abs(rii) .ge. 1.) in2=1                                        
      write (outbuf, 600) pi1,rii,to                                    
  600 format('0', 5x, 'POWER RATE RELAY P PRIME, R", TO', f6.3, 
     &   f7.3, f6.1) 
      call prtout (1)                                                   
      if(in1 .eq. 1 .and. in2 .eq. 1) go to 700                         
      t1 = 0.0                                                          
  650 t1r1(k) = t1                                                      
      t2r1(k) = t2                                                      
      go to 2400                                                        
C                                                                       
C     THIS LOGIC IS FOR THE R1 AND R2 MODELS                          
C                                                                       
  675 t1 = t1r1(k)                                                      
      td = tdr1(k)                                                      
      bias = biasr1(k)                                                  
      rb = rbr1(k)                                                      
      rmax = rmaxr1(k)                                                  
      rs = rsr1(k)                                                      
      tc = tcr1(k)                                                      
      t1 = t1-edt                                                       
      if(abs(pdot) .gt. rb) t1 = td                                     
C                                                                       
C     ICD2 = 5 FOR THE R1 MODEL                                       
C     ICD2 = 6 FOR THE R2 MODEL                                       
C     ICD2 = 7 FOR THE R3 MODEL                                       
C                                                                       
      if(icd2 .ne. 5)then                                               
         if(t1 .gt. 0.0) pdot = 0.0                                     
      endif                                                             
      if(pdot .lt. -rmax) pdot = -rmax                                  
      if(pdot .gt. rmax) pdot = rmax                                    
      ri = (pdot/rs) + (piii-bias)/ps                                   
      qold = qoldr1(k)                                                  
      riold = rir1(k)                                                   
      q = (qold*((2.*tc/edt)-1.)+riold + ri)/((2.*tc/edt)+1.)           
      if(icd2 .eq. 6) then                                              
         if(abs(q) .gt. 1.)go to 800                                    
      endif                                                             
      if(icd2 .eq. 5)then                                               
         if((t1 .le. 0.0) .and.  (abs(q) .gt. 1.0)) go to 800           
      endif                                                             
      t1r1(k) = t1                                                      
      tdr1(k) = td                                                      
      qoldr1(k) = q                                                     
      rir1(k) = ri                                                      
      go to 2400                                                        
  700 t1 = t1 + edt                                                     
      if(t1 .lt. th) go to 650                                          
C                                                                       
C     POWER RATE RELAY HAS BEEN TRIPPED SO SET ICD1 = 2 AND          
C     INITIALIZE TRANSFER TRIP TIMER TTIMR1                          
C                                                                       
  800 icd1 = 2                                                          
      icd1r1(k) = icd1                                                  
      ttimr1(k) = to + ttrpr1(k)                                        
      if(ttimr1(k) .lt. dnxrly)dnxrly = ttimr1(k)                       
      idum = 0                                                          
      write (outbuf, 900) name1, bkv1, name2, bkv2, to                  
      call prtout (1)                                                   
  900 format('0', 5x, 'RATE OF CHANGE OF POWER RELAY BETWEEN ', a8,     
     1        1x, f5.1, ' AND ', a8, 1x, f5.1, 'TRIPPED AT ',           
     2        f7.2, ' CYCLES')                                          
C                                                                       
C     IF ICD3 = 1 THE MONITORED LINE IS TRIPPED                       
C     IF ICD3 = 2 THE MONITORED LINE IS NOT TRIPPED                   
C                                                                       
      if(icd3 .ne. 1) go to  2250                                       
C                                                                       
C     TRIP LINE CONTAINING POWER RATE RELAY                          
C                                                                       
      ircall = 3                                                        
      ibusn = ibus                                                      
      jbusn = jbus                                                      
      if(ibus .lt. jbus) go to 1450                                     
      ibusn = jbus                                                      
      jbusn = ibus                                                      
 1450 icden = -1                                                        
      gijt = gijr1(k)                                                   
      bijt = bijr1(k)                                                   
      giot = gior1(k)                                                   
      biot = bior1(k)                                                   
      itparn = 0                                                        
      gjot = gjor1(k)                                                   
      bjot = bjor1(k)                                                   
C                                                                       
C     CALL TRPHIS TO FORM A TABLE OF LINES THAT HAVE BEEN TRIPPED     
C                                                                       
      call trphis(itparn,ibusn,jbusn,gijt,bijt,giot,                    
     1           biot,gjot,bjot,icden,idpar,jobdo,mcode)                
      if(jobdo.eq.0) go to 2250                                         
 1500 continue                                                          
      call getmat(ibus, ii)                                                 
      do 1600 i=4,ii,3                                                  
      if(jbus .eq. matrow(i)) go to 1700                                
 1600 continue                                                          
      go to 2500                                                        
 1700 if(abs(atrow(i+1)).lt.0.0001.and.abs(atrow(i+2)).lt.0.0001)       
     1     go to 1850                                                   
      if (keybrd(30) .ne. 0) then                                       
          write (outbuf, 1800) jbus, atrow(i + 1),                      
     1               atrow(i + 2), ibus, atrow(ii - 1), atrow(ii)       
 1800     format('0 R1SOL, S1700+2, B4 ', 2(i10, e16.6, e16.6))             
          call prtout (1)                                               
      endif                                                             
      atrow(i+1) = atrow(i+1) + gij                                     
      atrow(i+2) = atrow(i+2) + bij                                     
      if(abs(atrow(i+1)) .le. 0.0001) atrow(i+1) = 0.0                  
      if(abs(atrow(i+2)) .le. 0.0001) atrow(i+2) = 0.0                  
      atrow(ii-1) = atrow(ii-1) - gij - gio                             
      atrow(ii  ) = atrow(ii  ) - bij - bio                             
 1850 call putmat(ibus, ii)                                                 
      if (keybrd(30) .ne. 0) then                                       
         write (outbuf, 1950) jbus, atrow(i + 1),                       
     1               atrow(i + 2), ibus, atrow(ii - 1), atrow(ii)       
 1950    format('0 R1SOL, S1950-4, AFT ', 2(i10, e16.6, e16.6))            
         call prtout (1)                                                
      endif                                                             
      if(ibus.lt. lfrst)lfrst = ibus                                    
      if(idum .ne. 0) go to 2150                                        
      write (outbuf, 2050) name1, bkv1, name2, bkv2, to                 
 2050 format('0', 5x, 'RATE OF CHANGE OF POWER RELAY BETWEEN ', a8,     
     1           1x, f5.1, ' AND ', a8, 1x, f5.1,                       
     2           ' LINE OPENED AT ', f7.2, ' CYCLES')                   
      call prtout (1)                                                   
      idum = ibus                                                       
      ibus = jbus                                                       
      gio = gjo                                                         
      bio = bjo                                                         
      go to 1500                                                        
 2150 ivpc = 2                                                          
C                                                                       
C     ICD4 IS THE NUMBER OF REMOTE RELAYS ATTACHED TO THIS LOCAL      
C     RELAY.  TTIMR1 IS THE TRANSFER TRIP TIMER.                      
C                                                                       
 2250 if(icd4.eq.0) go to 2400                                          
      if(ttimr1(k) .gt. to) go to 2400                                  
      icd1r1(k) = 3                                                     
      do 2350 itrr = 1,icd4                                             
      ind = idrrr1(itrr,k)                                              
C                                                                       
C     CALL RRSOL TO PROCESS REMOTE RELAY LOGIC                        
C                                                                       
      call rrsol(ind)                                                   
 2350 continue                                                          
 2400 continue                                                          
      return                                                            
 2500 write (errbuf(1), 2600) jbus                                      
 2600 format('0', 5x, 'ERROR IN AUTOMATIC SWITCHING LOGIC--UNABLE       
     1           TO FIND BUS NUMBER', i5, ' PROGRAM STOPPED.')          
      call prterr ('E',1)                                               
      call mpost('R1SOL')                                               
      call erexit                                                       
      return                                                            
      end                                                               
