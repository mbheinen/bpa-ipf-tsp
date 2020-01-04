C    %W% %G%
      subroutine loadrp                                                 
C                                                                       
C     THIS SUBROUTINE CONTAINS LOGIC TO MODIFY THE ADMITTANCE MATRIX  
C     DIAGONALS TO SIMULATE LOAD MODIFICATION.  IT IS CALLED BY       
C     RSSOL.                                                          
C                                                                       
      include 'tspinc/params.inc' 
      include 'tspinc/cntrl2.inc' 
      include 'tspinc/blkcom1.inc' 
      include 'tspinc/lnk12.inc' 
      include 'tspinc/matrow.inc' 
      include 'tspinc/newton.inc' 
      include 'tspinc/fltopt.inc' 
      include 'tspinc/param.inc' 
      include 'tspinc/prt.inc' 
      include 'tspinc/ldidxn.inc' 
      include 'tspinc/ldshdn.inc' 
      include 'tspinc/busdta.inc' 
      include 'tspinc/ldropn.inc' 

      common /relay3/ ldbus, pshed1, qshed1, pshed2, qshed2, pshed3,
     &                qshed3, pshed4, qshed4, ldmde                                              
      equivalence (pshed1,plz), (qshed1,qlz), (pshed2,plcp),
     &            (qshed2,qlcq), (pshed3,plpp), (qshed3,qlpq),
     &            (pshed4,pldf), (qshed4,qldf)          
      dimension iecsn(6), ecsn2(2)                                  

      ildrp = 0                                                         
      ityp = 0                                                          
      ityp1 = 0                                                         
      ityp2 = 0                                                         
      ityp3 = 0                                                         
      ityp4 = 0                                                         
      iecsl = ldidxn(6,ldbus)                                           
      ptot = 0.                                                         
      qtot = 0.                                                         
C                                                                       
C     CHECK FOR CONST IMP LOAD                                       
C                                                                       
      if(plz.eq.0..and.qlz.eq.0.) go to 2220                            
      pm = -plz                                                         
      qm = -qlz                                                         
C                                                                       
C     IF ALL LOADS ARE CONSTANT IMPEDANCE OR THIS BUS HAS ALL         
C     CONSTANT IMPEDANCE LOAD, DONT CHECK LOAD TABLES                 
C                                                                       
      if(iecsl .eq. 0 .or. ipwr .eq. 0)go to 2100                       
      ityp1 = ldidxn(4,ldbus)                                           
      if(ityp1.eq.1) go to 2010                                         
      write (errbuf(1),2000) name1,bkv1                                 
 2000 format('0', 5x, ' CONST IMP LOAD NOT DEFINED AT BUS ', a8, 1x,
     &  f5.1, ' NO LOAD DROP ACTION TAKEN.')                             
      call prterr ('E',1)                                               
      call mpost('LOADRP')                                              
      go to 2220                                                        
 2010 ityp = 1                                                          
 2020 ildrp = 1                                                         
      pl = busldn(1,iecsl)                                              
      ql = busldn(2,iecsl)                                              
C                                                                       
C     CHECK IF LOAD EXISTS IN SUFFICIENT AMOUNT                       
C                                                                       
      if(pm.eq.0.) go to 2140                                           
      if(pl.lt.0.) go to 2060                                           
      write (errbuf(1),2040) name1,bkv1                                 
 2040 format('0', 5x, 'AT BUS ', a8, 1x, f5.1,
     &   ' P LOAD IS ZERO OR NEGATIVE. NO LOAD DROP ACTION TAKEN.')                             
      call prterr ('E',1)                                               
      call mpost('LOADRP')                                              
      go to 2140                                                        
 2060 if(pm.ge.pl) go to 2100                                           
      write (errbuf(1),2080) name1,bkv1                                 
 2080 format('0', 5x, 
     &  ' P LOAD DROP LIMITED TO AVAILABLE P LOAD AT BUS ', a8, 1x, 
     &  f5.1)                                                       
      call prterr ('E',1)                                               
      call mpost('LOADRP')                                              
      pm = pl                                                           
 2100 pl = pl - pm                                                      
      pmn = -pm                                                         
      write (outbuf,2120) ityp, pmn, name1,bkv1,to                      
 2120 format('0', 5x, ' TYPE ', i2, ' P LOAD --- ', f6.2,
     &  ' P.U.  DROPPED AT Bus ', a8, 1x, f5.1, ' at ', f7.2,' cycles.')
      call prtout (1)                                                   
      ptot = ptot - pmn                                                 
 2140 if(qm.eq.0.0) go to 2180                                          
      ql = ql - qm                                                      
      qmn = - qm                                                        
      write (outbuf,2160) ityp, qmn, name1,bkv1,to                      
 2160 format('0', 5x, ' TYPE ', i2, ' Q LOAD --- ', f6.2,
     &  ' P.U.  DROPPED AT Bus ', a8, 1x, f5.1, ' at ', f7.2,' cycles.')
      call prtout (1)                                                   
      qtot = qtot - qmn                                                 
 2180 if (iecsl .eq. 0 .or. ipwr .eq. 0) then                            
         ildrp = 1                                                      
         go to 2400                                                     
      endif                                                             
      busldn(1,iecsl) = pl                                              
      busldn(2,iecsl) = ql                                              
 2200 go to (2220, 2400, 2340, 2280), ityp                              
C                                                                       
C      CHECK FOR CONST MVA( FREQ DEP ) LOAD                             
C                                                                       
 2220 ityp2 = ldidxn(3,ldbus)                                           
      if(ityp2 .eq. 2)iecsl = iecsl+1                                   
      if(pldf.eq.0..and.qldf.eq.0.) go to 2280                          
      if(iecsl.eq.0) go to 2420                                         
      pm = -pldf                                                        
      qm = -qldf                                                        
      if(ityp2 .ne. 2) then                                             
         write (errbuf(1),2240) name1,bkv1                              
 2240    format('0', 1x, ' CONST MVA( FREQ DEP ) LOAD NOT DEFINED AT',    
     1          'BUS', 1x, f5.1, '. NO LOAD DROP ACTION TAKEN.')           
         call prterr ('E',1)                                            
         call mpost('LOADRP')                                           
         go to 2280                                                     
      endif                                                             
 2260 ityp = 4                                                          
      go to 2020                                                        
C                                                                       
C          CHECK FOR CONST MVA LOAD                                     
C                                                                       
 2280 ityp3 = ldidxn(2,ldbus)                                           
      if(ityp3 .eq. 3)iecsl = iecsl-1                                   
      if(plpp.eq.0..and.qlpq.eq.0.) go to 2340                          
      if(iecsl.eq.0) go to 2420                                         
      pm = -plpp                                                        
      qm = -qlpq                                                        
      if(ityp3 .ne. 3) then                                             
         write (errbuf(1),2300) name1,bkv1                              
 2300    format('0', 1x, ' CONST MVA LOAD NOT DEFINED AT BUS ', a8,        
     1          1x, f5.1, '. NO LOAD DROP ACTION TAKEN.')                 
         call prterr ('E',1)                                            
         call mpost('LOADRP')                                           
         go to 2340                                                     
      endif                                                             
 2320 ityp = 3                                                          
      go to 2020                                                        
C                                                                       
C         CHECK FOR CONST CURR LOAD                                     
C                                                                       
 2340 ityp4 = ldidxn(1,ldbus)                                           
      if(ityp4 .eq. 4)iecsl = iecsl - 1                                 
      if(plcp.eq.0..and.qlcq.eq.0.) go to 2400                          
      if(iecsl.eq.0) go to 2420                                         
      pm = -plcp                                                        
      qm =-qlcq                                                         
      if(ityp4 .ne. 4) then                                             
         write (errbuf(1),2360) name1,bkv1                              
 2360    format('0', 5x, 'CONST CURR LOAD NOT DEFINED AT BUS ', a8, 1x,     
     1           f5.1, ' NO LOAD DROP ACTION TAKEN')                     
         call mpost('LOADRP')                                           
         call prterr ('E',1)                                            
         go to 2400                                                     
      endif                                                             
 2380 ityp = 2                                                          
      go to 2020                                                        
 2400 if(ildrp.eq.0)  go to 2460                                        
      ivpc = 2                                                          
      call getmat(ldbus, ii)                                                
      gkk = atrow(ii-1)                                                 
      bkk = atrow(ii)                                                   
      if (keybrd(30) .ne. 0) then                                       
         write (outbuf, 2402) ldbus, gkk, bkk                           
 2402    format('0 LOADRP, S2400+7, B4 ', i10, 2e16.6)                  
         call prtout (1)                                                
      endif                                                             
      emag = emagrn(1,ldbus)                                            
      vsq = emag * emag                                                 
C                                                                       
C       YISOLN, NEWTON OPTION                                           
C                                                                       
      go to (2410,2405), inewts                                         
 2405 gadmit(ldbus) = gadmit(ldbus) + ptot*vsq                          
      badmit(ldbus) = badmit(ldbus) - qtot*vsq                          
      go to 2415                                                        
C                                                                       
C       ADMITTANCE MATRIX MODIFICATION                                  
C                                                                       
 2410 gkk = gkk + ptot * vsq                                            
      bkk = bkk - qtot * vsq                                            
      atrow(ii) = bkk                                                   
      atrow(ii-1) = gkk                                                 
      call putmat(ldbus, ii)                                                
 2415 if (keybrd(30) .ne. 0) then                                       
         write (outbuf, 2404) ldbus, gkk, bkk                           
 2404    format('0 LOADRP, S2420-3, B4 ', i10, 2e16.6)                  
         call prtout (1)                                                
      endif                                                             
      if(ldbus.lt.lfrst) lfrst = ldbus                                  
      go to 2460                                                        
 2420 write (errbuf(1),2440) name1,bkv1                                 
 2440 format('0', 5x, 'ERROR IN LOAD DROPPING - - - -- LOADS MAY NOT HAV
     &E BEEN DEFINED AT BUS ', a8, 1x, f5.1,' .  NO ACTION TAKEN.')          
      call prterr ('E',1)                                               
      call mpost('LOADRP')                                              
 2460 continue                                                          
      return                                                            
      end                                                               
