C    %W% %G%
      subroutine rrsol(ind)                                             
C                                                                       
C     THIS SUBBROUTINE PROCESS THE REMOTE RELAY                     
C     LOGIC.  IT IS CALLED BY RELAY.  IND IS THE INDEX              
C     IN THE RR DATA TABLES FOR THIS REMOTE RELAY.                  
C                                                                       
      include 'tspinc/params.inc' 
      include 'tspinc/cntrl2.inc' 
      include 'tspinc/param.inc' 
      include 'tspinc/newton.inc' 
      include 'tspinc/matrow.inc' 
      include 'tspinc/vrgov.inc' 
      include 'tspinc/lnk12.inc' 
      include 'tspinc/busdta.inc' 
      include 'tspinc/ldidxn.inc' 
      include 'tspinc/ldshdn.inc' 
      include 'tspinc/rrcom.inc' 
      include 'tspinc/kntrly.inc' 
      include 'tspinc/prt.inc' 
      include 'tspinc/bname.inc' 
      include 'tspinc/buskv.inc' 
      include 'tspinc/igentn.inc' 
      include 'tspinc/pointr.inc'                                              !dem

      dimension pd(4), qd(4), ktmp1n(2, 9), temp1n(2, 9),               
     1          ktempn(8, 13), ktempc(2, 13)                            
      character*1 ktempc*1                                              
      equivalence (tempc, ktempc), (tempn, ktempn),                     
     1            (ktmp1n, temp1n)                                      
      dimension indt(2), indlt(2)                                       

c     -  Local variables

      character*1 idpar,id1,id2,indc                                    
      character*8 name1,name2,name1c                                    
c     -     Begin     Begin     Begin     Begin     Begin     Begin 
      if (to .lt. dnxrly) dnxrly = to                                   
      mcode = mkodrr(ind)                                               
      idum=0                                                            
      jbus = ibs2rr(ind)                                                
      ibus = jbs2rr(ind)                                                
      idpar = ipr2rr(ind)                                               
C                                                                       
C     MCODE = 4 IS THE BUS LOAD DROPPING OPTION 'B'                   
C                                                                       
      if(mcode.eq.4) go to 1620                                         
C                                                                       
C     MCODE = 5 IS THE GENERATOR DROPPING OPTION 'G'                  
C                                                                       
      if (mcode.eq.5) go to 1720                                        
C                                                                       
C     THE FOLLOWING LOGIC IS FOR MCODE = 1,2,3 LINE MODIFICATION,     
C     DELETION, OR REINSETION                                         
C                                                                       
      name1 = bname(ibus)                                               
      name2 = bname(jbus)                                               
      bkv1 = buskv(ibus)                                                
      bkv2 = buskv(jbus)                                                
C                                                                       
C     GET MODIFIED LINE ADMITTANCE IF MCODE = 1                       
C                                                                       
      gijo = gijorr(ind)                                                
      bijo = bijorr(ind)                                                
      gioo = gioorr(ind)                                                
      bioo = bioorr(ind)                                                
      gjoo = gjoorr(ind)                                                
      bjoo = bjoorr(ind)                                                
      ibusn = ibus                                                      
      jbusn = jbus                                                      
      if(ibus .lt. jbus) go to 1385                                     
      ibusn = jbus                                                      
      jbusn = ibus                                                      
 1385 ircall = 6                                                        
      icden = 1                                                         
      if(mcode.eq.2) icden = -1                                         
      if(mcode .eq. 1) then                                             
         gijm = gijmrr(ind)                                             
         gijt = gijm                                                    
         bijm = bijmrr(ind)                                             
         bijt = bijm                                                    
         giom = 0.0                                                     
         giot = 0.0                                                     
         biom = biomrr(ind)                                             
         biot = biom                                                    
         gjom = 0.0                                                     
         gjot = 0.0                                                     
         bjom = bjomrr(ind)                                             
         bjot = bjom                                                    
      else                                                              
         gijt = gijo                                                    
         bijt = bijo                                                    
         giot = gioo                                                    
         biot = bioo                                                    
         gjot = gjoo                                                    
         bjot = bjoo                                                    
      endif                                                             
      itparn = 0                                                        
C                                                                       
C     CALL TRPHIS TO FORM A TABLE OF LINES THAT HAVE BEEN TRIPPED     
C                                                                       
      call trphis(itparn,ibusn,jbusn,gijt,bijt,giot,                    
     1           biot,gjot,bjot,icden,idpar,jobdo,mcode)                
      if(jobdo.eq.0) go to 1890                                         
 1400 continue                                                          
      call getmat(ibus, ii)                                                 
C                                                                       
C     FIND LINE TO BE SWITCHED IN ADMITTANCE MATRIX                   
C                                                                       
      do 1420 i=4,ii,3                                                  
      if (jbus.eq.matrow(i)) go to 1470                                 
 1420 continue                                                          
      write (errbuf(1), 1460) jbus                                      
 1460 format('0', 5x, 'ERROR IN AUTOMATIC SWITCHING LOGIC--UNABLE TO FIN
     &D BUS NUMBER', i5, ' PROGRAM STOPPED.')          
      call prterr ('E',1)                                               
      call mpost('RRSOL')                                               
      call erexit                                                       
 1470 if (keybrd(30) .ne. 0) then                                       
          write (outbuf, 1472) jbus, atrow(i + 1),                      
     1               atrow(i + 2), ibus, atrow(ii - 1), atrow(ii)       
 1472     format('0 RRSOL, S1470+1, BJ ', 2(i10, e16.6, e16.6))             
          call prtout (1)                                               
      endif                                                             
C                                                                       
C     CHANGE ADMITTANCE MATRIX FOR LINE MODIFICATION OPTION           
C     MCODE = 1 'M'                                                   
C                                                                       
      if(mcode .eq. 1)then                                              
         atrow(ii-1)=atrow(ii-1)-gijo-gioo+gijm+giom                    
         atrow(ii)=atrow(ii)-bijo-bioo+bijm+biom                        
         atrow(i+1)=atrow(i+1)+gijo-gijm                                
         atrow(i+2)=atrow(i+2)+bijo-bijm                                
         if(abs(atrow(i+1)) .le. 0.0001) atrow(i+1) = 0.0               
         if(abs(atrow(i+2)) .le. 0.0001) atrow(i+2) = 0.0               
         call putmat(ibus, ii)                                              
         if (keybrd(30) .ne. 0) then                                    
             write (outbuf, 1502) jbus, atrow(i + 1),                   
     1               atrow(i + 2), ibus, atrow(ii - 1), atrow(ii)       
 1502        format('0 RRSOL, S1500+6, AFT ', 2(i10, e16.6, e16.6))         
             call prtout (1)                                            
         endif                                                          
         if (ibus.lt.lfrst) lfrst=ibus                                  
         if (jbus.eq.idum) go to 1860                                   
         write (outbuf, 1520) name1, bkv1, name2, bkv2, idpar, to       
 1520    format('0', 5x, 'REMOTE SWITCHING ACTION---LINE BETWEEN ',     
     &           a8, 1x, f5.1, ' AND ', a8, 1x, f5.1, 1x, a1,
     &           ' MODIFIED AT ', f7.2, ' CYCLES')                                       
         call prtout (1)                                                
         idum=ibus                                                      
         ibus=jbus                                                      
         jbus=idum                                                      
         gioo=gjoo                                                      
         bioo=bjoo                                                      
         giom=gjom                                                      
         biom=bjom                                                      
         go to 1400                                                     
      endif                                                             
C                                                                       
C     CHANGE ADMITTANCE MATRIX FOR LINE DELETION MCODE =2  'D' BUT    
C     FIRST TEST IF COMPLETE LINE ALREADY TRIPPED                     
C                                                                       
      if(mcode .eq. 2)then                                              
         if(abs(atrow(i+1)).lt.0.0001.and.abs(atrow(i+2)).lt.0.0001)    
     1      go to 1582                                                  
         atrow(ii-1)=atrow(ii-1)-gijo-gioo                              
         atrow(ii)=atrow(ii)-bijo-bioo                                  
         atrow(i+1)=atrow(i+1)+gijo                                     
         atrow(i+2)=atrow(i+2)+bijo                                     
         if(abs(atrow(i+1)) .le. 0.0001) atrow(i+1) = 0.0               
         if(abs(atrow(i+2)) .le. 0.0001) atrow(i+2) = 0.0               
 1582    call putmat(ibus, ii)                                              
         if (keybrd(30) .ne. 0) then                                    
            write (outbuf, 1584) jbus, atrow(i + 1),                    
     1             atrow(i + 2), ibus, atrow(ii - 1), atrow(ii)         
 1584       format('0 RRSOL, S1582+3, B4 ', 2(i10, e16.6, e16.6))          
            call prtout (1)                                             
         endif                                                          
         if (ibus.lt.lfrst) lfrst=ibus                                  
         if (jbus.eq.idum) go to 1860                                   
         write (outbuf, 1560) name1, bkv1, name2, bkv2,idpar, to        
 1560    format('0', 5x, 'REMOTE SWITCHING ACTION---LINE BETWEEN ',     
     1           a8, 1x, f5.1, ' AND ', a8, 1x,f5.1,1x, a1,             
     2           ' OPENED AT ', f7.2, ' CYCLES')                        
         call prtout (1)                                                
         idum=ibus                                                      
         ibus=jbus                                                      
         jbus=idum                                                      
         gioo=gjoo                                                      
         bioo=bjoo                                                      
         go to 1400                                                     
      endif                                                             
C                                                                       
C     CHANGE ADMITTANCE MATRIX FOR LINE REINSERTION MCODE = 3 'R'     
C                                                                       
      if(mcode .eq. 3)then                                              
         atrow(ii-1)=atrow(ii-1)+gijo+gioo                              
         atrow(ii)=atrow(ii)+bijo+bioo                                  
         atrow(i+1)=atrow(i+1)-gijo                                     
         atrow(i+2)=atrow(i+2)-bijo                                     
         call putmat(ibus, ii)                                              
         if (keybrd(30) .ne. 0) then                                    
             write (outbuf, 1586) jbus, atrow(i + 1),                   
     1              atrow(i + 2), ibus, atrow(ii - 1), atrow(ii)        
 1586        format('0 RRSOL, S1580+6, AFT ', 2(i10, e16.6, e16.6))         
             call prtout (1)                                            
         endif                                                          
         if (ibus.lt.lfrst) lfrst=ibus                                  
         if (jbus.eq.idum) go to 1860                                   
         write (outbuf, 1600) name1, bkv1, name2, bkv2,idpar, to        
 1600    format('0', 5x, 'REMOTE SWITCHING ACTION---LINE BETWEEN ',     
     1           a8, 1x, f5.1, ' AND ', a8, 1x, f5.1, 1x, a1,             
     2           ' REINSERTED AT ', f7.2, ' CYCLES')                    
         call prtout (1)                                                
         idum=ibus                                                      
         ibus=jbus                                                      
         jbus=idum                                                      
         gioo=gjoo                                                      
         bioo=bjoo                                                      
         go to 1400                                                     
      endif                                                             
C                                                                       
C     BUS LOAD DROPPING OPTION MCODE = 4 'B'                          
C                                                                       
 1620 shdld1 = shl1rr(ind)                                              
      shdld2 = shl2rr(ind)                                              
      if(ibus .eq. 0) go to 1664                                        
      go to 1668                                                        
 1640 pold = pltotn(1,ibus)                                             
      qold = pltotn(2,ibus)                                             
      pdrop=pold*shdld1                                                 
      qdrop=qold*shdld1                                                 
      emag = emagrn(1,ibus)                                             
      eang = emagrn(2,ibus)                                             
      vsq=emag*emag                                                     
C                                                                       
C     INEWTS = 1 FOR ADMITTANCE MATRIX SOLUTION                       
C     INEWTS = 2 FOR NEWTONS METHOD SOLUTION                          
C                                                                       
      go to ( 1650, 1645 ), inewts                                      
 1645 gadmit( ibus ) = gadmit(  ibus ) - pdrop*vsq                      
      badmit( ibus ) = badmit( ibus ) + qdrop*vsq                       
      go to 1655                                                        
 1650 atrow(ii-1)=atrow(ii-1)-pdrop*vsq                                 
      atrow(ii)=atrow(ii)+qdrop*vsq                                     
 1655 name1c = bname(ibus)                                              
      bkv1 = buskv(ibus)                                                
      write (outbuf, 1660) name1c, bkv1, pdrop, qdrop, to               
 1660 format('0', 5x, 'REMOTE SWITCHING--LOAD SHED AT ', a8, 1x,        
     1           f5.1, ' P(PU) ', f6.2, ' Q(PU) ', f6.2, ' AT ',        
     2           f7.2, ' CYCLES')                                       
      call prtout (1)                                                   
      if (keybrd(30) .ne. 0) then                                       
         write (outbuf, 1662) jbus, atrow(i + 1),                       
     1               atrow(i + 2), ibus, atrow(ii - 1), atrow(ii)       
 1662    format('0 RRSOL, S1660+4, AFT ', 2(i10, e16.6, e16.6))            
         call prtout (1)                                                
      endif                                                             
      call putmat(ibus, ii)                                                 
      if (ibus.lt.lfrst) lfrst=ibus                                     
      if (jbus.eq.idum) go to 1860                                      
 1664 idum = ibus                                                       
      ibus=jbus                                                         
      jbus=idum                                                         
      shdld1=shdld2                                                     
 1668 if(ibus.eq.0) go to 1860                                          
      call getmat(ibus, ii)                                                 
      if (lrep.eq.0) go to 1640                                         
      iecsl = ldidxn(6,ibus)                                            
      nitem = ldidxn(5,ibus)                                            
      if (iecsl.eq.0) go to 1640                                        
      qtot = 0.0                                                        
      ptot = 0.0                                                        
      iecsl = iecsl - 1                                                 
      do 1680 i=1,nitem                                                 
      pd(i) = busldn(1,iecsl+i)                                         
      qd(i) = busldn(2,iecsl+i)                                         
      ptot=ptot+pd(i)                                                   
      qtot=qtot+qd(i)                                                   
 1680 continue                                                          
      do 1700 i=1,nitem                                                 
      if(abs(ptot).lt.0.00001)go to 1690                                
      pd(i)=pd(i)*(1.+pdrop/ptot)                                       
 1690 if(abs(qtot).lt.0.00001)go to 1700                                
      qd(i)=qd(i)*(1.+qdrop/qtot)                                       
      busldn(1,iecsl+i) = pd(i)                                         
      busldn(2,iecsl+i) = qd(i)                                         
 1700 continue                                                          
      go to 1640                                                        
C                                                                       
C     GENERATION DROPPING OPTION MCODE = 5 'G'                        
C                                                                       
 1720 id1 = ipr2rr(ind)                                                 
      id2 = jpr2rr(ind)                                                 
      indx = idg1rr(ind)                                                
      gndrop = gdr1rr(ind)                                              
      if(ibus.eq.0) go to 1840                                          
 1740 indt(1) = igentn(1,indx)                                          
      indt(2) = igentn(2,indx)                                          
      indc = igentc(indx)                                               
C                                                                       
C     DETECT HP MACHINE ID AND PICK UP THE LP MACHINE NO. FOR        
C     SIMULTANEOUS DROP                                              
C                                                                       
 1800 if(id1.ne.'H') go to 1812                                         
      ilow = 0                                                          
      do 1804 ij=1,isg                                                  
      if(igentn(1,ij) .ne. ibus) go to 1804                             
      if(igentc(ij) .eq. 'L') go to 1810                                
 1804 continue                                                          
      go to 1812                                                        
 1810 ilow=ij                                                           
 1812 call gendrp(indx, gndrop, indt(1), indt(2), indc)                 
      ivpc=2                                                            
      if (ibus.lt.lfrst) lfrst=ibus                                     
      name1c = bname(ibus)                                              
      bkv1 = buskv(ibus)                                                
      write (outbuf, 1820) name1c, bkv1, id1, to                        
 1820 format('0', 5x, 'REMOTE SWITCHING ACTION---GENERATION DROPPE'     
     1           ,'D AT ', a8, 1x, f5.1, 1x, a1, ' AT ', f7.2,          
     2           ' CYCLES')                                             
      call prtout (1)                                                   
      if (jbus.eq.idum) go to 1860                                      
 1840 idum=ibus                                                         
      if(jbus.eq.0) go to 1860                                          
      ibus=jbus                                                         
      jbus=idum                                                         
      id1=id2                                                           
      indx = idg2rr(ind)                                                
      gndrop = gdr2rr(ind)                                              
      go to 1740                                                        
 1860 ivpc=2                                                            
 1880 itrip = 1                                                         
 1890 continue                                                          
      return                                                            
      end                                                               
