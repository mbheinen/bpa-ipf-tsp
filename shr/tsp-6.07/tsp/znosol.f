C    %W% %G%
      subroutine znosol                                                 
c                                                                       
c     THIS SUBROUTINE CALCULATES THE EQUIVALENT ADMITTANCE            
c     FOR THE ZINC OXIDE ARRESTOR MODEL AND THE VARIABLE              
c     ADMITTANCE ELEMENTS                                             
c                                                                       
      include 'tspinc/params.inc' 
      include 'tspinc/znox.inc' 
      include 'tspinc/tmptbl.inc' 
      include 'tspinc/lowzno.inc' 
      include 'tspinc/znox2.inc' 
      include 'tspinc/vy1.inc' 
      include 'tspinc/param.inc' 
      include 'tspinc/vrgov.inc' 
      include 'tspinc/prt.inc' 
      include 'tspinc/matrow.inc' 
      include 'tspinc/bname.inc' 
      include 'tspinc/buskv.inc' 
      include 'tspinc/spare1.inc' 

      common /znotmp/ newtby, nznobg                                
      character*8 ibus1,jbus1                                           

      data pi/3.1415927/                                                

      do 1000, itrr = 1,iznmax                                          
      lowzno = nmx + 1                                                 
c                                                                       
c     STORE EXISTING TOTAL LINE EQUIVALENT IN TEMPORARY              
c     VARIABLES                                                      
c                                                                       
      gij = zngij(itrr)                                                 
      bij = znbij(itrr)                                                 
      gii = zngii(itrr)                                                 
      bii = znbii(itrr)                                                 
      gjj = zngjj(itrr)                                                 
      bjj = znbjj(itrr)                                                 
c                                                                       
c     IF A VARIABLE ADMITTANCE ELEMENT EXISTS ON THIS LINE, THEN      
c     COMBINE TOTAL VARIABLE ADMITTANCE WITH EXISTING ZNO ADMITTANCE  
c                                                                       
      if (vygtot(itrr) .ne. 0.0 .or. vybtot(itrr) .ne. 0.0) then         
          recp1=1./(vygtot(itrr)*vygtot(itrr)                         
     1             +vybtot(itrr)*vybtot(itrr))                          
      else                                                           
          recp1 = 0.0                                                 
      endif                                                          
      if (zngc(itrr) .ne. 0.0 .or. znbc(itrr) .ne. 0.0) then             
         recp2 = 1./(zngc(itrr)*zngc(itrr)                           
     1              + znbc(itrr)*znbc(itrr))                            
      else                                                           
         recp2 = 0.0                                                 
      endif                                                          
      rtot = vygtot(itrr)*recp1 + zngc(itrr)*recp2                   
      xtot = -(vybtot(itrr)*recp1 + znbc(itrr)*recp2)                
      recp1 = 1./(rtot*rtot + xtot*xtot)                             
      zng12(itrr) = rtot*recp1                                       
      znb12(itrr) =-xtot*recp1                                       
c                                                                       
c     IZNOSW = 3 MEANS THERE IS NO ZNO CAPACITOR                      
c                                                                       
      if (iznosw(itrr) .eq. 3 .and. idsw .eq. 7) go to 1000              
      if (iznosw(itrr) .eq. 3) go to 700                                 
      if (idsw .ne. 7 .and. lppwr .eq. 0) then                            
c                                                                       
c       IF IZNBYP = 2, THIS CAPACITOR HAS BEEN BYPASSED                 
c                                                                       
        if (iznbyp(itrr) .ne. 2) go to 100                                 
c                                                                       
c       TEMPORARY BYPASS ON ZNO REINSERT LOGIC                          
c                                                                       
        if (0 .eq. 0) go to 710                                                      

        if ( iznosw(itrr) .eq. 1 ) go to 950                           
        go to 710                                                      

c       -  Next 106 lines are bypassed 
c                                                                       
c       REINSRT CAP. IF CURRENT .LT. .05*IPROT                          
c                                                                       
   40   if (ctro(itrr) .gt. 0.05 *cznpro(itrr)) then
           if ( iznosw(itrr) .eq. 1 ) go to 950                           
           go to 710                                                      
        endif
        ibus1 = bname(iznbus(itrr))                                      
        jbus1 = bname(jznbus(itrr))                                      
        write (outbuf,50 ) ibus1, jbus1, tsim                               
   50   format ( '0', 5x, ' CAP  REINSERTED BETWEEN BUS ',a8, 
     &           ' AND ', a8, ' AT ', f8.2, ' CYCLES ' )                                 
        call prtout(1)                                                   
        izop(itrr) = 0                                                   
        znbc(itrr) = -1./znxco(itrr)                                
        zngc(itrr) = 0.0                                            
        iznbyp(itrr) = 0                                            
        go to 620                                                   
c                                                                       
c       IF ZNO STATE CHANGED OVER TWO CONSEC. TIMESTEP, PRINT INFO.FOR  
c       LAST STATE                                                      
c                                                                       
  100   if ( iznbyp(itrr) .ne. izop(itrr)) then                         
           izop(itrr) = iznbyp(itrr)                                    
           ibus1 = bname(iznbus(itrr))                                  
           jbus1 = bname(jznbus(itrr))                                  
           if ( iznbyp(itrr) .eq. 0 ) then                              
              write (outbuf, 60 ) ibus1, jbus1, tsim                       
              call prtout(1)                                            
              znengt(itrr) = 0.0                                        
              go to 115                                                 
            endif                                                       
            if (iznbyp(itrr) .eq. 1 ) write (outbuf, 65 ) ibus1, jbus1, 
     &                                tsim
  60        format('0',5x,'ZINC OXIDE ARRESTOR BETWEEN BUS ',a8,        
     1         ' AND ',a8,' STOPPED CONDUCTING AT ',f8.2,' CYCLES ')    
  65        format('0',5x,'ZINC OXIDE ARRESTOR BETWEEN BUS ',a8,        
     1         ' AND ',a8,' CONDUCTING AT ',f8.2,' CYCLES ')            
            call prtout(1)                                              
        endif                                                            
c                                                                       
c       INITIALIZE ACCUM. ENERGY IF ZNO IS NONCONDUCTING AT THE         
c       LAST TIME T                                                     
C                                                                       
c       CALCULATE ACCUMULATED ENERGY                                    
c                                                                       
        if ( iznbyp(itrr) .eq. 0 ) znengt(itrr) = 0.0                  
        znengt(itrr) = znengt(itrr) + zneng(itrr)                      
        if (znengt(itrr) .lt. znjou(itrr)) go to 115                       
        deltim = tsim - zntim(itrr)                                       
        if ( deltim .gt. enrcyc(itrr) ) go to 115                      
        iznbyp(itrr) = 2                                               
        ibus1 = bname(iznbus(itrr))                                    
        jbus1 = bname(jznbus(itrr))                                    
        write(outbuf, 110) ibus1, jbus1, tsim                                
 110    format('0',5x,'ZINC OXIDE ARRESTOR BETWEEN BUS ',a8,           
     1         ' AND ',a8,' BYPASSED AT ',f8.2,' CYCLES ')              
        call prtout(1)                                                 
c                                                                       
c       INITIALIZE ENERGY AND TIME TABLES                                
c                                                                       
        zntim(itrr) = endt                                            
        zneng(itrr) = 0.0                                             
        znengt(itrr) = 0.0                                            
        zngc(itrr) = 0.0                                              
        znbc(itrr) = -10000.0                                         
        go to 620                                                     
      endif                                                             
c                                                                       
c     CALL ZNMAT TO OBTAIN THE VOLTAGES ACROSS THE VARIABLE ADMITTANCE
c     SECTION--EG(1)+JFG(1) AND EG(2) + JFG(2)                        
c                                                                       
  115 call znmat(itrr)                                                  
c                                                                       
c     CALCULATE CURRENT THROUGH ZNO PROTECTED CAPACITOR               
c                                                                       
      zng = zng12(itrr) + zng11l(itrr)                                  
      znb = znb12(itrr) + znb11l(itrr)                                  
      c1r=eg(1)*zng - fg(1)*znb                                         
      c1i=eg(1)*znb + fg(1)*zng                                         
      c2r=eg(2)*zng12(itrr) - fg(2)*znb12(itrr)                         
      c2i=eg(2)*znb12(itrr) + fg(2)*zng12(itrr)                         
      ctr=c1r-c2r                                                       
      cti=c1i-c2i                                                       
      cmag = sqrt(ctr*ctr + cti*cti)                                    
c                                                                       
c     IF CAP IS BYPASSED STORE CURRENT AND GO TO NEXT ZNO             
c                                                                       
      if (iznbyp(itrr) .eq. 2 ) then                                 
         ctro(itrr) = cmag                                              
         go to 950                                                      
      endif                                                          
c                                                                       
c       COMPARE ACTUAL CURRENT WITH PROTECTION CURRENT                  
c                                                                       
      if (cmag .lt. .98*cznpro(itrr)) then                                
         zntim(itrr) = endt                                             
         zneng(itrr) = 0.0                                              
c                                                                       
c        IF ZNO WAS CONDUCTING PREVIOUSLY, RESET ADMITTANCE TO NOMINAL   
c                                                                       
         if (iznbyp(itrr) .eq. 1) then                                    
            znbc(itrr) = -1./znxco(itrr)                                
            zngc(itrr) = 0.0                                            
            iznbyp(itrr) = 0                                            
            go to 620                                                   
         endif                                                          
         iznbyp(itrr) = 0                                               
c                                                                       
c        IF ONLY ZNO AND IS NONCONDUCTING DURING PREVIOUS TIMESTEP,      
c        THEN BYPASS YMATRIX UPDATE BELOW                              
c                                                                       
         if ( iznosw(itrr) .eq. 1 ) go to 950                           
         go to 710                                                      
      endif                                                             
c                                                                       
c     ZNO IS CONDUCTING                                               
c                                                                       
      ctr = cmag/cznpro(itrr)                                           
c                                                                       
c     IF IZNBYP = 0 THE ZNO WAS NOT CONDUCTING PREVIOUSLY             
c                                                                       
      if (iznbyp(itrr) .eq. 0) then                                       
         iznbyp(itrr) = 1                                               
         zntim(itrr) = tsim                                                
c                                                                       
c        CALCULATE NEW XC AND RC USING NEWTON'S METHOD                   
c                                                                       
c        OPTION TO BYPASS NEWTONS APPROX. SOLN.*****                     

         if (newtby .eq. 2 ) go to 560                                 
         intzn = 0                                                      
         xs = 1.0/cmag - xtot                                           
 500     intzn = intzn +1                                               
         if (intzn .gt. 10) then                                         
            write(errbuf(1),550)cmag,delctr                             
 550        format('0',5x,'ITERATION LIMIT REACHED IN ZNO CAPACITOR ',  
     1      'LOGIC CMAG AND DELCTR: ',f7.4,f9.6)                        
            call prterr('E',1)                                          
            go to 1000                                                  
         endif                                                          
         xc  = 0.1010 - 0.005749*ctr + 2.088* exp(-0.8566*ctr)           
         xcp = -0.005749 - 2.088 * 0.8566 * exp (-0.8566* ctr)           
         fpofc = xs + znxco(itrr)*(xc + xcp*ctr)                        
         fofc = -1./cznpro(itrr) + ctr*(xc*znxco(itrr) + xs)            
         ctr1 = ctr - fofc/fpofc                                        
         delctr = abs(ctr1 - ctr)                                       
         if (delctr .gt. .01) then                                        
            ctr = ctr1                                                  
            go to 500                                                   
         endif                                                          
      endif                                                             
  560 xcpu = 0.1010 - 0.005749*ctr + 2.088* exp(-0.8566*ctr)          
      rcpu = 0.0745  + 0.49*exp (-0.243*ctr) - 35.0*exp (-5.0*ctr) -  
     1   0.60*exp (-1.4*ctr)                                            
      xc =  znxco(itrr)*xcpu                                            
      rc = -znxco(itrr)*rcpu                                            
      recip = 1./(rc*rc + xc*xc)                                     
      gc = rc*recip                                                  
      bc = -xc*recip                                                 
      zngc(itrr) = gc                                                
      znbc(itrr) = bc                                                
c                                                                       
c     STORE CURRENT ESTIMATE, CTR                                     
c                                                                       
      ctro(itrr) = ctr                                               
c                                                                       
c     TALLY ENERGY OVER THIS TIMESTEP ******                          
c                                                                       
      if (idsw .ne. 7 ) zneng(itrr) = edt *cmag*cmag*rc               
c                                                                       
c     COMBINE TOTAL VARIABLE ADMITTANCE WITH NEW ZNO ADMITTANCE       
c                                                                       
 620  if (vygtot(itrr).ne. 0.0 .or. vybtot(itrr).ne. 0.0) then         
          recp1=1./(vygtot(itrr)*vygtot(itrr)                         
     1           +vybtot(itrr)*vybtot(itrr))                          
      else                                                           
          recp1 = 0.0                                                 
      endif                                                          
      recp2 = 1./(zngc(itrr)*zngc(itrr)+ znbc(itrr)*znbc(itrr))      
      rtot = vygtot(itrr)*recp1 + zngc(itrr)*recp2                   
      xtot = -(vybtot(itrr)*recp1 + znbc(itrr)*recp2)                
      recp1 = 1./(rtot*rtot + xtot*xtot)                             
      zng12(itrr) = rtot*recp1                                       
      znb12(itrr) =-xtot*recp1                                       

c                                                                       
c     CALL ZNMAT TO CALCULATE NEW LINE EQUIVALENT                     
c                                                                       
  700 call znmat(itrr)                                               
c                                                                       
c     REMOVE OLD EQUIVALENT ADMITTANCE FROM Y MATRIX                  
c     AND ADD NEW EQUIVALENT                                          
c                                                                       
  710 ibus = iznbus(itrr)                                               
      jbus = jznbus(itrr)                                               
      call getmat(ibus, ii)                                                 
      iii = ii                                                          
      do i = 4, iii, 3                                              
        if (jbus.eq.matrow(i)) go to 920                                   
      enddo
  920 atrow(ii-1) = atrow(ii-1) -gij - gii + zngij(itrr) + zngii(itrr)   
      atrow(ii) = atrow(ii) - bij - bii + znbij(itrr) + znbii(itrr)      
      atrow(i+1) = atrow(i+1) + gij  - zngij(itrr)                      
      atrow(i+2) = atrow(i+2) + bij  - znbij(itrr)                      
      call putmat(ibus, ii)                                                 
      call getmat(jbus, ii)                                                 
      iii = ii                                                          
      do i = 4, iii, 3                                              
        if (ibus.eq.matrow(i)) go to 940                                   
      enddo
  940 atrow(ii-1) = atrow(ii-1) - gij - gjj + zngij(itrr) + zngjj(itrr)   
      atrow(ii) = atrow(ii) - bij - bjj + znbij(itrr) + znbjj(itrr)      
      atrow(i+1) = atrow(i+1) + gij  - zngij(itrr)                      
      atrow(i+2) = atrow(i+2) + bij  - znbij(itrr)                      
      call putmat(jbus, ii)                                                 
c                                                                       
c      GET LOWEST BUS NO. FOR YMATRIX FACTORIZING                      
c                                                                       
       lowzno = min0 (lowzno, ibus)                                     
       lowzno = min0 (lowzno, jbus)                                     
  950  continue                                                         
c                                                                       
c      TEMP DEBUG OUTPUT ******                                        
c                                                                       
       if ( nznobg .ne. 0 ) then                                        
          write(outbuf, 650) itrr, iznosw(itrr), iznbyp(itrr), intzn,   
     1    edt, eg(1), fg(1), eg(2), fg(2) ,zneng(itrr), znengt(itrr)     
  650     format (1x, 'ZNODEBUG...', 4i5, 7e13.6 )                      
          call prtout (1)                                               
          write ( outbuf, 655 )                                         
     1    zng, znb, zngc(itrr), znbc(itrr), zng12(itrr), znb12(itrr)    
  655     format ( 5x, 6e13.6 )                                         
          call prtout(1)                                                
          write (outbuf, 656 ) cmag, ctr, cznpro(itrr)                  
  656     format (1x, 3e13.6 )                                          
          call prtout(1)                                                
       endif                                                            
1000  continue                                                          
      return                                                            
      end                                                               
